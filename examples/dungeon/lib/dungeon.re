module Tile = {
  type door_state =
    | Open
    | Closed;

  type key =
    | Brass
    | Iron;

  type lock =
    | Unlocked
    | Locked(key);

  type door = {
    state: door_state,
    lock,
  };

  type t =
    | Floor
    | Wall
    | Door(door)
    | Stairs;

  let plain_door = {
    state: Closed,
    lock: Unlocked,
  };

  let locked_door = key => {
    state: Closed,
    lock: Locked(key),
  };

  let key_name =
    fun
    | Brass => "brass"
    | Iron => "iron";

  let passable =
    fun
    | Floor
    | Stairs
    | Door({state: Open, _}) => true
    | Wall
    | Door({state: Closed, _}) => false;

  let%test "floor, stairs and open doors can be walked on" =
    passable(Floor)
    && passable(Stairs)
    && passable(
         Door({
           state: Open,
           lock: Locked(Iron),
         }),
       );

  let%test "walls and closed doors block, locked or not" =
    !passable(Wall)
    && !passable(Door(plain_door))
    && !passable(Door(locked_door(Brass)));
};

type room = {
  width: int,
  height: int,
  tiles: array(Tile.t) /* row-major: index = y * width + x */
};

let make = (~width, ~height, fill) => {
  width,
  height,
  tiles: Array.make(width * height, fill),
};

let width = r => r.width;

let height = r => r.height;

let in_bounds = (r, p) => {
  let (x, y) = (Pos.x(p), Pos.y(p));
  x >= 0 && y >= 0 && x < r.width && y < r.height;
};

let tile_at = (r, p) =>
  if (in_bounds(r, p)) {
    r.tiles[Pos.y(p) * r.width + Pos.x(p)];
  } else {
    Tile.Wall;
  };

let set_tile = (r, p, tile) =>
  if (in_bounds(r, p)) {
    let tiles = Array.copy(r.tiles);
    tiles[Pos.y(p) * r.width + Pos.x(p)] = tile;
    {
      ...r,
      tiles,
    };
  } else {
    r;
  };

let passable = (r, p) => Tile.passable(tile_at(r, p));

let positions = r => {
  let acc = ref([]);
  for (y in r.height - 1 downto 0) {
    for (x in r.width - 1 downto 0) {
      acc := [Pos.make(x, y), ...acc^];
    };
  };
  acc^;
};

let%test "outside the grid counts as wall" =
  tile_at(make(~width=3, ~height=3, Tile.Floor), Pos.make(3, 0))
  == Tile.Wall
  && tile_at(make(~width=3, ~height=3, Tile.Floor), Pos.make(0, -1))
  == Tile.Wall;

let%test "set_tile changes one cell and leaves the original room alone" = {
  let r = make(~width=2, ~height=2, Tile.Wall);
  let r' = set_tile(r, Pos.make(1, 1), Tile.Floor);
  tile_at(r', Pos.make(1, 1)) == Tile.Floor
  && tile_at(r', Pos.make(0, 0)) == Tile.Wall
  && tile_at(r, Pos.make(1, 1)) == Tile.Wall;
};

let%test "positions are listed row by row" =
  positions(make(~width=2, ~height=2, Tile.Floor))
  == [Pos.make(0, 0), Pos.make(1, 0), Pos.make(0, 1), Pos.make(1, 1)];

module Carve = {
  let walled_rect = (r, ~origin, ~width as w, ~height as h) => {
    let (ox, oy) = (Pos.x(origin), Pos.y(origin));
    let r = ref(r);
    for (y in oy - 1 to oy + h) {
      for (x in ox - 1 to ox + w) {
        let inside = x >= ox && x < ox + w && y >= oy && y < oy + h;
        let tile = inside ? Tile.Floor : Tile.Wall;
        r := set_tile(r^, Pos.make(x, y), tile);
      };
    };
    r^;
  };

  let cut_door = (r, p, door) =>
    switch (tile_at(r, p)) {
    | Tile.Wall => Some(set_tile(r, p, Tile.Door(door)))
    | _ => None
    };

  let place_stairs = (r, p) =>
    switch (tile_at(r, p)) {
    | Tile.Floor => Some(set_tile(r, p, Tile.Stairs))
    | _ => None
    };

  let rock = make(~width=6, ~height=6, Tile.Wall);

  let%test "walled_rect lays floor inside and wall around" = {
    let r = walled_rect(rock, ~origin=Pos.make(1, 1), ~width=2, ~height=2);
    tile_at(r, Pos.make(1, 1)) == Tile.Floor
    && tile_at(r, Pos.make(2, 2)) == Tile.Floor
    && tile_at(r, Pos.make(0, 0)) == Tile.Wall
    && tile_at(r, Pos.make(3, 1)) == Tile.Wall
    && tile_at(r, Pos.make(1, 3)) == Tile.Wall;
  };

  let%test "walled_rect leaves the rest of the rock alone" = {
    let r = walled_rect(rock, ~origin=Pos.make(1, 1), ~width=2, ~height=2);
    tile_at(r, Pos.make(5, 5)) == Tile.Wall;
  };

  let%test "a door can only be cut into a wall" = {
    let r = walled_rect(rock, ~origin=Pos.make(1, 1), ~width=2, ~height=2);
    switch (
      cut_door(r, Pos.make(3, 1), Tile.locked_door(Iron)),
      cut_door(r, Pos.make(1, 1), Tile.plain_door),
    ) {
    | (Some(r'), None) =>
      tile_at(r', Pos.make(3, 1))
      == Tile.Door({
           state: Closed,
           lock: Locked(Iron),
         })
    | _ => false
    };
  };

  let%test "stairs go on floor" = {
    let r = walled_rect(rock, ~origin=Pos.make(1, 1), ~width=2, ~height=2);
    switch (
      place_stairs(r, Pos.make(2, 2)),
      place_stairs(r, Pos.make(0, 0)),
    ) {
    | (Some(r'), None) => tile_at(r', Pos.make(2, 2)) == Tile.Stairs
    | _ => false
    };
  };
};
