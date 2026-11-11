/* What the player asks to do on their turn. */
type command =
  | Move(Pos.dir) /* step that way; bumping a monster attacks it, bumping a shut door opens it if you have its key */
  | Attack(Pos.dir) /* strike whatever is one step that way; an armed trap is disarmed instead */
  | Use(Creatures.item) /* use an item from the inventory */
  | Wait; /* stand still */

/* One thing that happened during a turn, in the order it happened. */
type event =
  | Moved(Pos.t)
  | Blocked(Pos.t)
  | OpenedDoor(Pos.t)
  | UnlockedDoor(Pos.t, Dungeon.Tile.key) /* always followed by OpenedDoor */
  | DoorLocked(Pos.t, Dungeon.Tile.key) /* the player lacks this key */
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

/* A room, who stands where in it, how many turns have passed, and what has happened. */
type state;

/* A game at turn zero with nothing in its log. Put the player in the creature list. */
let make:
  (~room: Dungeon.room, ~creatures: list((Pos.t, Creatures.t))) => state;

/* The room as it is now; doors may have been opened. */
let room: state => Dungeon.room;

/* The number of turns taken so far. */
let turn: state => int;

/* Everyone still in the room, with where they stand. */
let creatures: state => list((Pos.t, Creatures.t));

/* Everything that has happened, oldest first. */
let log: state => list(event);

/* Whoever stands at a position. */
let at: (state, Pos.t) => option(Creatures.t);

/* The player and where they stand, if they are in the room. */
let player: state => option((Pos.t, Creatures.t));

/* Carries out one command for the player, then lets every adjacent monster strike.
   Adds one to the turn counter and appends what happened to the log.
   A dead player takes no more turns: the state comes back unchanged. */
let step: (command, state) => state;
