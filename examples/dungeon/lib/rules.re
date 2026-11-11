module Crit = {
  type roll = int;

  let every = 5;

  let multiplier = 2;

  let roll_for_turn = turn => turn;

  let can_crit =
    fun
    | Creatures.Player(_)
    | Creatures.Monster(_) => true
    | Creatures.Trap(_) => false;

  let lands = (~attacker, roll) =>
    can_crit(attacker) && roll > 0 && roll mod every == 0;

  let%test "a critical lands on every fifth roll" =
    lands(~attacker=Creatures.spawn(Goblin), 5)
    && lands(~attacker=Creatures.spawn(Goblin), 10)
    && !lands(~attacker=Creatures.spawn(Goblin), 4)
    && !lands(~attacker=Creatures.spawn(Goblin), 0);

  let%test "traps never land a critical" =
    !lands(~attacker=Creatures.trap(3), 5);
};

type hit = {
  damage: int,
  critical: bool,
};

let damage_dealt =
  fun
  | Creatures.Player(_) => 3
  | Creatures.Monster({kind: Goblin, _}) => 1
  | Creatures.Monster({kind: Orc, _}) => 4
  | Creatures.Monster({kind: Skeleton, _}) => 2
  | Creatures.Trap({damage, armed: true}) => damage
  | Creatures.Trap({armed: false, _}) => 0;

let armor =
  fun
  | Creatures.Player(_) => 1
  | Creatures.Monster({kind: Skeleton, _}) => 1
  | Creatures.Monster(_) => 0
  | Creatures.Trap(_) => 0;

let blow = (~attacker, ~defender, ~roll) => {
  let raw = damage_dealt(attacker);
  if (raw == 0) {
    {
      damage: 0,
      critical: false,
    };
  } else {
    let critical = Crit.lands(~attacker, roll);
    let force = critical ? raw * Crit.multiplier : raw;
    {
      damage: max(1, force - armor(defender)),
      critical,
    };
  };
};

let hero = Creatures.new_player(~hp=10, ~inventory=[]);

let%test "a skeleton's armor takes the edge off" =
  blow(~attacker=hero, ~defender=Creatures.spawn(Skeleton), ~roll=1)
  == {
       damage: 2,
       critical: false,
     };

let%test "a goblin still lands one point on an armored player" =
  blow(~attacker=Creatures.spawn(Goblin), ~defender=hero, ~roll=1)
  == {
       damage: 1,
       critical: false,
     };

let%test "a disarmed trap has no force at all" =
  blow(
    ~attacker=
      Creatures.Trap({
        damage: 5,
        armed: false,
      }),
    ~defender=hero,
    ~roll=5,
  )
  == {
       damage: 0,
       critical: false,
     };

let%test "a critical blow doubles the force before armor" =
  blow(~attacker=hero, ~defender=Creatures.spawn(Skeleton), ~roll=5)
  == {
       damage: 5,
       critical: true,
     }
  && blow(~attacker=Creatures.spawn(Goblin), ~defender=hero, ~roll=5)
  == {
       damage: 1,
       critical: true,
     };
