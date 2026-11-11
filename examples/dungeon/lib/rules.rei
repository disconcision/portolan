/* When a blow lands harder than usual. */
module Crit: {
  /* The luck of one blow. Comes from the turn counter, so games are repeatable. */
  type roll;

  /* A critical lands on every roll that is a multiple of this. */
  let every: int;

  /* How many times harder a critical blow strikes. */
  let multiplier: int;

  /* The roll used for every blow struck on a given turn. */
  let roll_for_turn: int => roll;

  /* True when this creature is able to land a critical. Traps are not. */
  let can_crit: Creatures.t => bool;

  /* True when the attacker's blow on this roll is a critical. */
  let lands: (~attacker: Creatures.t, roll) => bool;
};

/* What one blow does: the points it takes off, and whether it was a critical. */
type hit = {
  damage: int,
  critical: bool,
};

/* How many points a creature's blow carries before armor. A disarmed trap carries none. */
let damage_dealt: Creatures.t => int;

/* How many points a creature's armor absorbs. */
let armor: Creatures.t => int;

/* The damage a blow does after criticals and the defender's armor.
   At least one point lands when the blow had any force. */
let blow:
  (~attacker: Creatures.t, ~defender: Creatures.t, ~roll: Crit.roll) => hit;
