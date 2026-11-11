/* The tiles a room is made of. */
module Tile: {
  /* Whether a door stands open or shut. */
  type door_state =
    | Open
    | Closed;

  /* The kinds of key a lock can ask for. */
  type key =
    | Brass
    | Iron;

  /* What a door asks of whoever wants to open it. */
  type lock =
    | Unlocked
    | Locked(key);

  /* A door: open or shut, and what it takes to open it. */
  type door = {
    state: door_state,
    lock,
  };

  /* One cell of the dungeon. */
  type t =
    | Floor
    | Wall
    | Door(door)
    | Stairs;

  /* A shut door that anyone can open. */
  let plain_door: door;

  /* A shut door that only the holder of the key can open. */
  let locked_door: key => door;

  /* The lower-case name of a key: "brass", "iron". */
  let key_name: key => string;

  /* True when a creature can stand on the tile. A shut door blocks whether or not it is locked. */
  let passable: t => bool;
};

/* A rectangular grid of tiles. Cells outside the grid read as Wall. */
type room;

/* A room of the given size filled with one tile. */
let make: (~width: int, ~height: int, Tile.t) => room;

/* The number of columns. */
let width: room => int;

/* The number of rows. */
let height: room => int;

/* True when the position lies inside the grid. */
let in_bounds: (room, Pos.t) => bool;

/* The tile at a position; Wall for positions outside the grid. */
let tile_at: (room, Pos.t) => Tile.t;

/* A copy of the room with one tile replaced. Positions outside the grid are ignored. */
let set_tile: (room, Pos.t, Tile.t) => room;

/* True when a creature can stand at the position. */
let passable: (room, Pos.t) => bool;

/* Every position in the grid, row by row from the top left. */
let positions: room => list(Pos.t);

/* Ways of cutting shapes into a room. */
module Carve: {
  /* A floor rectangle with its top-left corner at origin, ringed by walls one cell thick. */
  let walled_rect: (room, ~origin: Pos.t, ~width: int, ~height: int) => room;

  /* Turns a wall into the given door. None when the position is not a wall. */
  let cut_door: (room, Pos.t, Tile.door) => option(room);

  /* Puts stairs on a floor cell. None when the position is not floor. */
  let place_stairs: (room, Pos.t) => option(room);
};
