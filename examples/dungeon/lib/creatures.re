type item =
  | Potion(int)
  | Elixir
  | Key(Dungeon.Tile.key);

type kind =
  | Goblin
  | Orc
  | Skeleton;

type player = {
  hp: int,
  max_hp: int,
  inventory: list(item),
};

type monster = {
  kind,
  hp: int,
};

type trap = {
  damage: int,
  armed: bool,
};

type t =
  | Player(player)
  | Monster(monster)
  | Trap(trap);

type creature = t;

let kind_name =
  fun
  | Goblin => "goblin"
  | Orc => "orc"
  | Skeleton => "skeleton";

let name =
  fun
  | Player(_) => "you"
  | Monster({kind, _}) => kind_name(kind)
  | Trap(_) => "a trap";

let max_hp_of_kind =
  fun
  | Goblin => 3
  | Orc => 8
  | Skeleton => 5;

let new_player = (~hp, ~inventory) =>
  Player({
    hp,
    max_hp: hp,
    inventory,
  });

let spawn = kind =>
  Monster({
    kind,
    hp: max_hp_of_kind(kind),
  });

let trap = damage =>
  Trap({
    damage,
    armed: true,
  });

let hp =
  fun
  | Player({hp, _})
  | Monster({hp, _}) => Some(hp)
  | Trap(_) => None;

let is_alive = c =>
  switch (hp(c)) {
  | Some(n) => n > 0
  | None => true
  };

let%test "a fresh player is at full health" =
  switch (new_player(~hp=10, ~inventory=[])) {
  | Player({hp, max_hp, _}) => hp == 10 && max_hp == 10
  | _ => false
  };

let%test "spawned monsters start with their kind's hit points" =
  hp(spawn(Orc)) == Some(8) && hp(spawn(Goblin)) == Some(3);

let%test "traps have no hit points and never die" =
  hp(trap(2)) == None && is_alive(trap(2));

module Items = {
  type t = item;

  type effect =
    | Healed(int)
    | Nothing;

  let name =
    fun
    | Potion(n) => Printf.sprintf("a potion (+%d)", n)
    | Elixir => "an elixir"
    | Key(k) => Printf.sprintf("a %s key", Dungeon.Tile.key_name(k));

  let carries = (item, c) =>
    switch (c) {
    | Player({inventory, _}) => List.mem(item, inventory)
    | Monster(_)
    | Trap(_) => false
    };

  /* hp raised by amount, but never past full; written so a huge amount cannot overflow. */
  let raise_hp = (~hp, ~max_hp, amount) =>
    amount >= max_hp - hp ? max_hp : hp + amount;

  let heal = (amount, c) =>
    switch (c) {
    | Player(p) =>
      let hp = raise_hp(~hp=p.hp, ~max_hp=p.max_hp, amount);
      (
        Player({
          ...p,
          hp,
        }),
        Healed(hp - p.hp),
      );
    | Monster(m) =>
      let hp = raise_hp(~hp=m.hp, ~max_hp=max_hp_of_kind(m.kind), amount);
      (
        Monster({
          ...m,
          hp,
        }),
        Healed(hp - m.hp),
      );
    | Trap(_) => (c, Nothing)
    };

  let apply = (item, c) =>
    switch (item) {
    | Potion(n) => heal(n, c)
    | Elixir => heal(max_int, c)
    | Key(_) => (c, Nothing) /* keys work on doors, not on creatures */
    };

  let rec remove_first = (item, inv) =>
    switch (inv) {
    | [] => None
    | [x, ...rest] when x == item => Some(rest)
    | [x, ...rest] =>
      Option.map(rest' => [x, ...rest'], remove_first(item, rest))
    };

  /* An item that did something is used up; one that did nothing stays in the pack. */
  let use = (item, c) =>
    switch (c) {
    | Player(p) =>
      switch (remove_first(item, p.inventory)) {
      | None => None
      | Some(inventory) =>
        switch (
          apply(
            item,
            Player({
              ...p,
              inventory,
            }),
          )
        ) {
        | (_, Nothing) => Some((c, Nothing))
        | applied => Some(applied)
        }
      }
    | Monster(_)
    | Trap(_) => None
    };

  let%test "a potion heals but not past full health" = {
    let hurt =
      Player({
        hp: 4,
        max_hp: 10,
        inventory: [],
      });
    apply(Potion(3), hurt)
    == (
         Player({
           hp: 7,
           max_hp: 10,
           inventory: [],
         }),
         Healed(3),
       )
    && apply(Potion(30), hurt)
    == (
         Player({
           hp: 10,
           max_hp: 10,
           inventory: [],
         }),
         Healed(6),
       );
  };

  let%test "an elixir restores a monster fully" =
    apply(
      Elixir,
      Monster({
        kind: Orc,
        hp: 1,
      }),
    )
    == (
         Monster({
           kind: Orc,
           hp: 8,
         }),
         Healed(7),
       );

  let%test "items do nothing to traps" =
    apply(Elixir, trap(2)) == (trap(2), Nothing);

  let%test "using an item takes one copy out of the inventory" = {
    let p =
      Player({
        hp: 5,
        max_hp: 10,
        inventory: [Potion(2), Potion(2)],
      });
    use(Potion(2), p)
    == Some((
         Player({
           hp: 7,
           max_hp: 10,
           inventory: [Potion(2)],
         }),
         Healed(2),
       ));
  };

  let%test "an item not carried cannot be used" =
    use(
      Elixir,
      Player({
        hp: 5,
        max_hp: 10,
        inventory: [Potion(2)],
      }),
    )
    == None;

  let%test "a key does nothing on its own and is not used up" = {
    let p =
      Player({
        hp: 5,
        max_hp: 10,
        inventory: [Key(Brass)],
      });
    use(Key(Brass), p) == Some((p, Nothing));
  };

  let%test "carries looks in the player's pack" =
    carries(
      Key(Iron),
      Player({
        hp: 5,
        max_hp: 10,
        inventory: [Key(Iron)],
      }),
    )
    && !
         carries(
           Key(Brass),
           Player({
             hp: 5,
             max_hp: 10,
             inventory: [Key(Iron)],
           }),
         )
    && !carries(Key(Iron), spawn(Orc));
};

module Combat = {
  type outcome =
    | Wounded({
        survivor: creature,
        damage: int,
      })
    | Slain({damage: int})
    | NoEffect;

  let wound = (c, damage) =>
    switch (c) {
    | Player(p) =>
      Player({
        ...p,
        hp: p.hp - damage,
      })
    | Monster(m) =>
      Monster({
        ...m,
        hp: m.hp - damage,
      })
    | Trap(_) => c
    };

  /* How much a blow takes off is decided elsewhere (see Rules); this only lands it. */
  let resolve = (~damage, defender) =>
    switch (defender) {
    | Trap(_) => NoEffect
    | Player(_)
    | Monster(_) =>
      let survivor = wound(defender, damage);
      is_alive(survivor)
        ? Wounded({
            survivor,
            damage,
          })
        : Slain({damage: damage});
    };

  let%test "three points fell a goblin" =
    resolve(~damage=3, spawn(Goblin)) == Slain({damage: 3});

  let%test "three points leave an orc standing" =
    resolve(~damage=3, spawn(Orc))
    == Wounded({
         survivor:
           Monster({
             kind: Orc,
             hp: 5,
           }),
         damage: 3,
       });

  let%test "a blow with no force still counts as a blow" =
    resolve(~damage=0, new_player(~hp=10, ~inventory=[]))
    == Wounded({
         survivor:
           Player({
             hp: 10,
             max_hp: 10,
             inventory: [],
           }),
         damage: 0,
       });

  let%test "traps cannot be fought" =
    resolve(~damage=3, trap(2)) == NoEffect;
};
