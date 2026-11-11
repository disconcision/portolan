type command =
  | Move(Pos.dir)
  | Attack(Pos.dir)
  | Use(Creatures.item)
  | Wait;

type event =
  | Moved(Pos.t)
  | Blocked(Pos.t)
  | OpenedDoor(Pos.t)
  | UnlockedDoor(Pos.t, Dungeon.Tile.key)
  | DoorLocked(Pos.t, Dungeon.Tile.key)
  | Descended
  | Struck({
      attacker: Creatures.t,
      defender: Creatures.t,
      damage: int,
      critical: bool,
    })
  | Slain(Creatures.t)
  | SwungAtNothing(Pos.t)
  | Sprung({
      at: Pos.t,
      damage: int,
    })
  | Disarmed(Pos.t)
  | Used({
      item: Creatures.item,
      effect: Creatures.Items.effect,
    })
  | NoSuchItem(Creatures.item)
  | Waited
  | Died;

type state = {
  room: Dungeon.room,
  creatures: list((Pos.t, Creatures.t)), /* at most one per cell */
  turn: int,
  log: list(event) /* newest first */
};

let make = (~room, ~creatures) => {
  room,
  creatures,
  turn: 0,
  log: [],
};

let room = s => s.room;

let turn = s => s.turn;

let creatures = s => s.creatures;

let log = s => List.rev(s.log);

let at = (s, p) =>
  List.find_opt(((q, _)) => Pos.equal(p, q), s.creatures)
  |> Option.map(snd);

let is_player =
  fun
  | Creatures.Player(_) => true
  | Creatures.Monster(_)
  | Creatures.Trap(_) => false;

let player = s => List.find_opt(((_, c)) => is_player(c), s.creatures);

/* --- small edits to the state --- */

let note = (e, s) => {
  ...s,
  log: [e, ...s.log],
};

let set_at = (p, c, s) => {
  ...s,
  creatures:
    List.map(
      ((q, c')) => Pos.equal(p, q) ? (q, c) : (q, c'),
      s.creatures,
    ),
};

let remove_at = (p, s) => {
  ...s,
  creatures: List.filter(((q, _)) => !Pos.equal(p, q), s.creatures),
};

let move_from_to = (p, q, s) => {
  ...s,
  creatures:
    List.map(((r, c)) => Pos.equal(p, r) ? (q, c) : (r, c), s.creatures),
};

/* --- what the pieces of a turn do --- */

/* One blow from attacker onto whoever stands at target. */
let strike = (~attacker, ~target, s) =>
  switch (at(s, target)) {
  | None => note(SwungAtNothing(target), s)
  | Some(defender) =>
    let roll = Rules.Crit.roll_for_turn(s.turn);
    let {Rules.damage, critical} = Rules.blow(~attacker, ~defender, ~roll);
    switch (Creatures.Combat.resolve(~damage, defender)) {
    | NoEffect => note(SwungAtNothing(target), s)
    | Wounded({survivor, damage}) =>
      s
      |> set_at(target, survivor)
      |> note(
           Struck({
             attacker,
             defender,
             damage,
             critical,
           }),
         )
    | Slain({damage}) =>
      let s =
        note(
          Struck({
            attacker,
            defender,
            damage,
            critical,
          }),
          s,
        );
      if (is_player(defender)) {
        s
        |> set_at(target, Creatures.Combat.wound(defender, damage))
        |> note(Died);
      } else {
        s |> remove_at(target) |> note(Slain(defender));
      };
    };
  };

/* The player steps to target, or does whatever stands in the way. */
let move = (~from, ~target, s) =>
  switch (at(s, target)) {
  | Some(Creatures.Monster(_)) =>
    strike(~attacker=Option.get(at(s, from)), ~target, s)
  | Some(Creatures.Trap({armed: true, _}) as trap) =>
    let player = Option.get(at(s, from));
    let roll = Rules.Crit.roll_for_turn(s.turn);
    let {Rules.damage, _} =
      Rules.blow(~attacker=trap, ~defender=player, ~roll);
    let s =
      s
      |> remove_at(target)
      |> move_from_to(from, target)
      |> note(Moved(target))
      |> note(
           Sprung({
             at: target,
             damage,
           }),
         );
    switch (Creatures.Combat.resolve(~damage, player)) {
    | Wounded({survivor, _}) => set_at(target, survivor, s)
    | Slain({damage}) =>
      s
      |> set_at(target, Creatures.Combat.wound(player, damage))
      |> note(Died)
    | NoEffect => s
    };
  | Some(Creatures.Trap({armed: false, _})) =>
    s
    |> remove_at(target)
    |> move_from_to(from, target)
    |> note(Moved(target))
  | Some(Creatures.Player(_)) => note(Blocked(target), s)
  | None =>
    let open_door = (door: Dungeon.Tile.door, s) =>
      {
        ...s,
        room:
          Dungeon.set_tile(
            s.room,
            target,
            Door({
              ...door,
              state: Open,
            }),
          ),
      }
      |> note(OpenedDoor(target));
    switch (Dungeon.tile_at(s.room, target)) {
    | Door({state: Closed, lock: Unlocked} as door) => open_door(door, s)
    | Door({state: Closed, lock: Locked(key)} as door) =>
      let player = Option.get(at(s, from));
      if (Creatures.Items.carries(Key(key), player)) {
        s |> note(UnlockedDoor(target, key)) |> open_door(door);
      } else {
        note(DoorLocked(target, key), s);
      };
    | Stairs =>
      s
      |> move_from_to(from, target)
      |> note(Moved(target))
      |> note(Descended)
    | tile when Dungeon.Tile.passable(tile) =>
      s |> move_from_to(from, target) |> note(Moved(target))
    | _ => note(Blocked(target), s)
    };
  };

let attack = (~from, ~target, s) =>
  switch (at(s, target)) {
  | Some(Creatures.Trap({armed: true, damage})) =>
    s
    |> set_at(
         target,
         Creatures.Trap({
           armed: false,
           damage,
         }),
       )
    |> note(Disarmed(target))
  | _ => strike(~attacker=Option.get(at(s, from)), ~target, s)
  };

let use = (~from, item, s) =>
  switch (Creatures.Items.use(item, Option.get(at(s, from)))) {
  | None => note(NoSuchItem(item), s)
  | Some((player, effect)) =>
    s
    |> set_at(from, player)
    |> note(
         Used({
           item,
           effect,
         }),
       )
  };

let act = (cmd, ~from, s) =>
  switch (cmd) {
  | Move(d) => move(~from, ~target=Pos.step(from, d), s)
  | Attack(d) => attack(~from, ~target=Pos.step(from, d), s)
  | Use(item) => use(~from, item, s)
  | Wait => note(Waited, s)
  };

/* Every monster next to the player takes a swing. */
let monsters_act = s =>
  switch (player(s)) {
  | None => s
  | Some((pp, _)) =>
    let adjacent =
      List.filter(
        ((q, c)) =>
          switch (c) {
          | Creatures.Monster(_) => Pos.manhattan(pp, q) == 1
          | _ => false
          },
        s.creatures,
      );
    List.fold_left(
      (s, (_, monster)) =>
        switch (at(s, pp)) {
        | Some(p) when Creatures.is_alive(p) =>
          strike(~attacker=monster, ~target=pp, s)
        | _ => s
        },
      s,
      adjacent,
    );
  };

let step = (cmd, s) =>
  switch (player(s)) {
  | None => s
  | Some((_, p)) when !Creatures.is_alive(p) => s
  | Some((from, _)) =>
    {
      ...s,
      turn: s.turn + 1,
    }
    |> act(cmd, ~from)
    |> monsters_act
  };

/* --- tests --- */

/* A 5x4 floor with walls round it, a shut door in the east wall, a brass-locked
   door in the south wall, and stairs in the corner. */
let arena = {
  Dungeon.(
    make(~width=8, ~height=7, Tile.Wall)
    |> Carve.walled_rect(_, ~origin=Pos.make(1, 1), ~width=5, ~height=4)
    |> Carve.cut_door(_, Pos.make(6, 2), Tile.plain_door)
    |> Option.get
    |> Carve.cut_door(_, Pos.make(1, 5), Tile.locked_door(Brass))
    |> Option.get
    |> Carve.place_stairs(_, Pos.make(5, 4))
    |> Option.get
  );
};

let hero = Creatures.new_player(~hp=10, ~inventory=[Potion(3)]);

let fixture = creatures =>
  make(~room=arena, ~creatures=[(Pos.make(1, 1), hero), ...creatures]);

let player_pos = s => Option.map(fst, player(s));

let player_hp = s => Option.bind(player(s), ((_, p)) => Creatures.hp(p));

let%test "moving onto floor moves the player and counts a turn" = {
  let s = step(Move(East), fixture([]));
  player_pos(s) == Some(Pos.make(2, 1))
  && turn(s) == 1
  && log(s) == [Moved(Pos.make(2, 1))];
};

let%test "walking into a wall is blocked" = {
  let s = step(Move(North), fixture([]));
  player_pos(s) == Some(Pos.make(1, 1))
  && log(s) == [Blocked(Pos.make(1, 0))];
};

let%test "a shut door opens on the first push and admits on the second" = {
  let s =
    fixture([])
    |> step(Move(South))
    |> step(Move(East))
    |> step(Move(East))
    |> step(Move(East))
    |> step(Move(East));
  let before = step(Move(East), s);
  let after = step(Move(East), before);
  player_pos(s) == Some(Pos.make(5, 2))
  && List.nth(log(before), 5) == OpenedDoor(Pos.make(6, 2))
  && Dungeon.tile_at(room(before), Pos.make(6, 2))
  == Door({
       state: Open,
       lock: Unlocked,
     })
  && player_pos(after) == Some(Pos.make(6, 2));
};

let%test "a locked door keeps out whoever lacks the key" = {
  let s =
    fixture([])
    |> step(Move(South))
    |> step(Move(South))
    |> step(Move(South));
  let s = step(Move(South), s);
  player_pos(s) == Some(Pos.make(1, 4))
  && List.nth(log(s), 3) == DoorLocked(Pos.make(1, 5), Brass)
  && Dungeon.tile_at(room(s), Pos.make(1, 5))
  == Door({
       state: Closed,
       lock: Locked(Brass),
     });
};

let%test "the right key unlocks a door and is kept" = {
  let keyed = Creatures.new_player(~hp=10, ~inventory=[Key(Brass)]);
  let s = make(~room=arena, ~creatures=[(Pos.make(1, 4), keyed)]);
  let s = step(Move(South), s);
  log(s)
  == [UnlockedDoor(Pos.make(1, 5), Brass), OpenedDoor(Pos.make(1, 5))]
  && Dungeon.tile_at(room(s), Pos.make(1, 5))
  == Door({
       state: Open,
       lock: Locked(Brass),
     })
  && player(s) == Some((Pos.make(1, 4), keyed))
  && player_pos(step(Move(South), s)) == Some(Pos.make(1, 5));
};

let%test "the wrong key is no key" = {
  let keyed = Creatures.new_player(~hp=10, ~inventory=[Key(Iron)]);
  let s = make(~room=arena, ~creatures=[(Pos.make(1, 4), keyed)]);
  log(step(Move(South), s)) == [DoorLocked(Pos.make(1, 5), Brass)];
};

let%test "stepping on the stairs descends" = {
  let s =
    fixture([])
    |> step(Move(South))
    |> step(Move(South))
    |> step(Move(South));
  let s =
    s
    |> step(Move(East))
    |> step(Move(East))
    |> step(Move(East))
    |> step(Move(East));
  player_pos(s) == Some(Pos.make(5, 4)) && List.mem(Descended, log(s));
};

let%test "attacking a goblin fells it" = {
  let s =
    step(
      Attack(East),
      fixture([(Pos.make(2, 1), Creatures.spawn(Goblin))]),
    );
  at(s, Pos.make(2, 1)) == None
  && log(s)
  == [
       Struck({
         attacker: hero,
         defender: Creatures.spawn(Goblin),
         damage: 3,
         critical: false,
       }),
       Slain(Creatures.spawn(Goblin)),
     ];
};

let%test "walking into an orc attacks it and it hits back" = {
  let orc = Creatures.spawn(Orc);
  let s = step(Move(East), fixture([(Pos.make(2, 1), orc)]));
  player_pos(s) == Some(Pos.make(1, 1))
  && at(s, Pos.make(2, 1))
  == Some(
       Monster({
         kind: Orc,
         hp: 5,
       }),
     )
  && player_hp(s) == Some(7)
  && log(s)
  == [
       Struck({
         attacker: hero,
         defender: orc,
         damage: 3,
         critical: false,
       }),
       Struck({
         attacker:
           Monster({
             kind: Orc,
             hp: 5,
           }),
         defender: hero,
         damage: 3,
         critical: false,
       }),
     ];
};

let%test "swinging at empty air does nothing" =
  log(step(Attack(East), fixture([])))
  == [SwungAtNothing(Pos.make(2, 1))];

let%test "stepping on a trap springs it" = {
  let s =
    step(Move(South), fixture([(Pos.make(1, 2), Creatures.trap(4))]));
  player_pos(s) == Some(Pos.make(1, 2))
  && player_hp(s) == Some(7)
  && List.length(creatures(s)) == 1
  && log(s)
  == [
       Moved(Pos.make(1, 2)),
       Sprung({
         at: Pos.make(1, 2),
         damage: 3,
       }),
     ];
};

let%test "attacking a trap disarms it, and then it can be walked over" = {
  let s =
    step(Attack(South), fixture([(Pos.make(1, 2), Creatures.trap(4))]));
  let s' = step(Move(South), s);
  at(s, Pos.make(1, 2))
  == Some(
       Creatures.Trap({
         damage: 4,
         armed: false,
       }),
     )
  && log(s) == [Disarmed(Pos.make(1, 2))]
  && player_hp(s') == Some(10)
  && player_pos(s') == Some(Pos.make(1, 2));
};

let%test "using a potion heals and uses it up" = {
  let hurt =
    Creatures.Player({
      hp: 5,
      max_hp: 10,
      inventory: [Potion(3)],
    });
  let s =
    step(
      Use(Potion(3)),
      make(~room=arena, ~creatures=[(Pos.make(1, 1), hurt)]),
    );
  player(s)
  == Some((
       Pos.make(1, 1),
       Player({
         hp: 8,
         max_hp: 10,
         inventory: [],
       }),
     ))
  && log(s)
  == [
       Used({
         item: Potion(3),
         effect: Healed(3),
       }),
     ];
};

let%test "using something not carried is noted" =
  log(step(Use(Elixir), fixture([]))) == [NoSuchItem(Elixir)];

let%test "waiting next to monsters lets them all strike" = {
  let s =
    step(
      Wait,
      fixture([
        (Pos.make(2, 1), Creatures.spawn(Goblin)),
        (Pos.make(1, 2), Creatures.spawn(Skeleton)),
        (Pos.make(4, 4), Creatures.spawn(Orc)),
      ]),
    );
  player_hp(s) == Some(8)
  && List.length(log(s)) == 3
  && List.hd(log(s)) == Waited;
};

let%test "a dead player takes no more turns" = {
  let s = fixture([(Pos.make(2, 1), Creatures.spawn(Orc))]);
  let s = s |> step(Wait) |> step(Wait) |> step(Wait) |> step(Wait);
  let again = step(Move(South), s);
  player_hp(s) == Some(-2) && List.mem(Died, log(s)) && again == s;
};

let%test "on the fifth turn a blow is critical" = {
  let goblin = Creatures.spawn(Goblin);
  let s = fixture([(Pos.make(2, 1), goblin)]);
  let s = s |> step(Wait) |> step(Wait) |> step(Wait) |> step(Wait);
  let s = step(Attack(East), s);
  turn(s) == 5
  && player_hp(s) == Some(6)
  && at(s, Pos.make(2, 1)) == None
  && List.mem(
       Struck({
         attacker:
           Player({
             hp: 6,
             max_hp: 10,
             inventory: [Potion(3)],
           }),
         defender: goblin,
         damage: 6,
         critical: true,
       }),
       log(s),
     );
};
