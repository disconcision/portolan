/* Something a creature can carry and use. */
type item =
  | Potion(int) /* heals this many hit points */
  | Elixir /* heals to full */
  | Key(Dungeon.Tile.key); /* opens doors with the matching lock */

/* The kinds of monster that live in the dungeon. */
type kind =
  | Goblin
  | Orc
  | Skeleton;

/* The adventurer: current and full hit points, and what they carry. */
type player = {
  hp: int,
  max_hp: int,
  inventory: list(item),
};

/* A monster of some kind and its current hit points. */
type monster = {
  kind,
  hp: int,
};

/* A trap: how hard it bites, and whether it still can. */
type trap = {
  damage: int,
  armed: bool,
};

/* Anything that occupies a cell of the room. A trap is not alive, but it sits in the same list. */
type t =
  | Player(player)
  | Monster(monster)
  | Trap(trap);

/* The same type under another name, for use inside nested modules whose own t is something else. */
type creature = t;

/* The lower-case name of a kind of monster. */
let kind_name: kind => string;

/* What to call a creature in a message: "you", "orc", "a trap". */
let name: t => string;

/* The hit points a kind of monster starts with. */
let max_hp_of_kind: kind => int;

/* A player at full health carrying the given items. */
let new_player: (~hp: int, ~inventory: list(item)) => t;

/* A monster of a kind, at full health. */
let spawn: kind => t;

/* An armed trap that bites for the given damage. */
let trap: int => t;

/* Current hit points; None for a trap. */
let hp: t => option(int);

/* True while hit points are above zero. Traps are always "alive". */
let is_alive: t => bool;

/* Items and what they do. */
module Items: {
  type t = item;

  /* What applying an item did. */
  type effect =
    | Healed(int)
    | Nothing;

  /* The name of an item in a message. */
  let name: t => string;

  /* True when the creature is a player with the item in their pack. */
  let carries: (t, creature) => bool;

  /* The item's effect on a creature, with the creature afterwards. Keys have none. */
  let apply: (t, creature) => (creature, effect);

  /* Applies one copy of the item from a player's inventory. An item that had an
     effect is used up; one that had none stays in the pack.
     None when the creature is not a player or does not carry the item. */
  let use: (t, creature) => option((creature, effect));
};

/* Fights between creatures. */
module Combat: {
  /* What an attack did to the defender. */
  type outcome =
    | Wounded({
        survivor: creature,
        damage: int,
      })
    | Slain({damage: int})
    | NoEffect;

  /* The creature with that many hit points taken away. Traps are unchanged. */
  let wound: (creature, int) => creature;

  /* Lands a blow of the given damage on the defender. How much damage a blow
     carries is the business of Rules, not of this module. */
  let resolve: (~damage: int, creature) => outcome;
};
