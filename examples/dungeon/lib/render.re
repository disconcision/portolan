module Glyph = {
  let tile =
    fun
    | Dungeon.Tile.Floor => '.'
    | Dungeon.Tile.Wall => '#'
    | Dungeon.Tile.Stairs => '>'
    | Dungeon.Tile.Door({state: Open, _}) => '\''
    | Dungeon.Tile.Door({state: Closed, lock: Unlocked}) => '+'
    | Dungeon.Tile.Door({state: Closed, lock: Locked(_)}) => '*';

  let creature =
    fun
    | Creatures.Player(_) => '@'
    | Creatures.Monster({kind: Goblin, _}) => 'g'
    | Creatures.Monster({kind: Orc, _}) => 'o'
    | Creatures.Monster({kind: Skeleton, _}) => 's'
    | Creatures.Trap({armed: true, _}) => '^'
    | Creatures.Trap({armed: false, _}) => ',';

  let%test "doors show whether they are shut and locked" =
    tile(Door(Dungeon.Tile.plain_door)) == '+'
    && tile(Door(Dungeon.Tile.locked_door(Iron))) == '*'
    && tile(
         Door({
           state: Open,
           lock: Locked(Iron),
         }),
       )
    == '\'';

  let%test "a sprung trap looks different from an armed one" =
    creature(Creatures.trap(2)) == '^'
    && creature(
         Creatures.Trap({
           damage: 2,
           armed: false,
         }),
       )
    == ',';
};

let cell = (s, p) =>
  switch (Turn.at(s, p)) {
  | Some(c) => Glyph.creature(c)
  | None => Glyph.tile(Dungeon.tile_at(Turn.room(s), p))
  };

let row = (s, y) =>
  String.init(Dungeon.width(Turn.room(s)), x => cell(s, Pos.make(x, y)));

let map = s =>
  List.init(Dungeon.height(Turn.room(s)), row(s)) |> String.concat("\n");

let pack =
  fun
  | [] => "nothing"
  | items => List.map(Creatures.Items.name, items) |> String.concat(", ");

let status = s =>
  switch (Turn.player(s)) {
  | Some((_, Creatures.Player({hp, max_hp, inventory}))) =>
    Printf.sprintf(
      "Turn %d  HP %d/%d  Pack: %s",
      Turn.turn(s),
      hp,
      max_hp,
      pack(inventory),
    )
  | _ => Printf.sprintf("Turn %d  No player", Turn.turn(s))
  };

let describe = s => map(s) ++ "\n" ++ status(s) ++ "\n";

let narrate = e =>
  Turn.(
    switch (e) {
    | Moved(p) => "You step to " ++ Pos.to_string(p) ++ "."
    | Blocked(p) => "Something solid at " ++ Pos.to_string(p) ++ "."
    | OpenedDoor(p) => "The door at " ++ Pos.to_string(p) ++ " swings open."
    | UnlockedDoor(_, key) =>
      "Your " ++ Dungeon.Tile.key_name(key) ++ " key turns in the lock."
    | DoorLocked(_, key) =>
      "The door is locked. It wants a "
      ++ Dungeon.Tile.key_name(key)
      ++ " key."
    | Descended => "You find the stairs down."
    | Struck({attacker, defender, damage, critical}) =>
      Printf.sprintf(
        "%s %s %s for %d%s.",
        String.capitalize_ascii(Creatures.name(attacker)),
        Creatures.name(attacker) == "you" ? "hit" : "hits",
        Creatures.name(defender),
        damage,
        critical ? " (critical!)" : "",
      )
    | Slain(c) => "The " ++ Creatures.name(c) ++ " falls."
    | SwungAtNothing(_) => "You swing at empty air."
    | Sprung({damage, _}) =>
      Printf.sprintf("A trap! It bites you for %d.", damage)
    | Disarmed(_) => "You disarm the trap."
    | Used({item, effect: Healed(n)}) =>
      Printf.sprintf(
        "You use %s and heal %d.",
        Creatures.Items.name(item),
        n,
      )
    | Used({item, effect: Nothing}) =>
      "You fiddle with " ++ Creatures.Items.name(item) ++ ". Nothing happens."
    | NoSuchItem(item) => "You have no " ++ Creatures.Items.name(item) ++ "."
    | Waited => "You wait."
    | Died => "You die."
    }
  );

/* --- tests --- */

let tiny = {
  Dungeon.(
    make(~width=5, ~height=4, Tile.Wall)
    |> Carve.walled_rect(_, ~origin=Pos.make(1, 1), ~width=3, ~height=2)
    |> Carve.cut_door(_, Pos.make(4, 1), Tile.plain_door)
    |> Option.get
    |> Carve.place_stairs(_, Pos.make(3, 2))
    |> Option.get
  );
};

let scene =
  Turn.make(
    ~room=tiny,
    ~creatures=[
      (
        Pos.make(1, 1),
        Creatures.new_player(~hp=10, ~inventory=[Potion(3), Key(Brass)]),
      ),
      (Pos.make(3, 1), Creatures.spawn(Orc)),
      (Pos.make(2, 2), Creatures.trap(2)),
    ],
  );

let%test "the room is drawn with creatures over the tiles" =
  describe(scene) == "#####\n"
  ++ "#@.o+\n"
  ++ "#.^>#\n"
  ++ "#####\n"
  ++ "Turn 0  HP 10/10  Pack: a potion (+3), a brass key\n";

let%test "an empty pack says so" =
  status(
    Turn.make(
      ~room=tiny,
      ~creatures=[
        (Pos.make(1, 1), Creatures.new_player(~hp=4, ~inventory=[])),
      ],
    ),
  )
  == "Turn 0  HP 4/4  Pack: nothing";

let%test "blows are narrated from the right side" =
  narrate(
    Struck({
      attacker: Creatures.spawn(Orc),
      defender: Creatures.new_player(~hp=10, ~inventory=[]),
      damage: 3,
      critical: true,
    }),
  )
  == "Orc hits you for 3 (critical!)."
  && narrate(
       Struck({
         attacker: Creatures.new_player(~hp=10, ~inventory=[]),
         defender: Creatures.spawn(Goblin),
         damage: 3,
         critical: false,
       }),
     )
  == "You hit goblin for 3.";
