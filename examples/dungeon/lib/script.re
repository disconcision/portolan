/* A 6x4 room: a plain door east, a brass-locked door south, stairs in the far corner. */
let level = {
  Dungeon.(
    make(~width=8, ~height=7, Tile.Wall)
    |> Carve.walled_rect(_, ~origin=Pos.make(1, 1), ~width=6, ~height=4)
    |> Carve.cut_door(_, Pos.make(7, 2), Tile.plain_door)
    |> Option.get
    |> Carve.cut_door(_, Pos.make(3, 5), Tile.locked_door(Brass))
    |> Option.get
    |> Carve.place_stairs(_, Pos.make(6, 4))
    |> Option.get
  );
};

let hero =
  Creatures.new_player(~hp=10, ~inventory=[Potion(3), Key(Brass)]);

let goblin = Creatures.spawn(Goblin);

let orc = Creatures.spawn(Orc);

let start =
  Turn.make(
    ~room=level,
    ~creatures=[
      (Pos.make(1, 1), hero),
      (Pos.make(2, 1), goblin),
      (Pos.make(3, 1), orc),
      (Pos.make(3, 3), Creatures.trap(2)),
    ],
  );

let commands: list(Turn.command) = [
  Attack(East), /* fell the goblin */
  Move(East), /* step into its place; the orc is now adjacent and strikes */
  Move(East), /* bump the orc: a blow each way */
  Use(Potion(3)), /* drink; the orc hits again */
  Attack(East) /* turn five: a critical fells the orc */
];

let play = (commands, state) =>
  List.fold_left((s, c) => Turn.step(c, s), state, commands);

let final = play(commands, start);

let transcript = s =>
  Render.describe(s)
  ++ (List.map(Render.narrate, Turn.log(s)) |> String.concat("\n"))
  ++ "\n";

/* --- tests on the final state --- */

let%test "five commands take five turns" = Turn.turn(final) == 5;

let%test "the hero ends at the goblin's old spot, wounded, with only the key" =
  Turn.player(final)
  == Some((
       Pos.make(2, 1),
       Creatures.Player({
         hp: 4,
         max_hp: 10,
         inventory: [Key(Brass)],
       }),
     ));

let%test "both monsters are gone and the trap is untouched" =
  Turn.at(final, Pos.make(2, 1)) != None
  && Turn.at(final, Pos.make(3, 1)) == None
  && Turn.at(final, Pos.make(3, 3)) == Some(Creatures.trap(2))
  && List.length(Turn.creatures(final)) == 2;

let%test "the doors are as they were" =
  Dungeon.tile_at(Turn.room(final), Pos.make(7, 2))
  == Door(Dungeon.Tile.plain_door)
  && Dungeon.tile_at(Turn.room(final), Pos.make(3, 5))
  == Door(Dungeon.Tile.locked_door(Brass));

/* --- tests on the event log --- */

let%test "the log tells the whole story in order" = {
  let wounded_orc =
    Creatures.Monster({
      kind: Orc,
      hp: 5,
    });
  let hero_at = (hp, inventory) =>
    Creatures.Player({
      hp,
      max_hp: 10,
      inventory,
    });
  let full_pack = [Creatures.Potion(3), Key(Brass)];
  Turn.log(final)
  == [
       /* turn 1 */
       Struck({
         attacker: hero,
         defender: goblin,
         damage: 3,
         critical: false,
       }),
       Slain(goblin),
       /* turn 2 */
       Moved(Pos.make(2, 1)),
       Struck({
         attacker: orc,
         defender: hero,
         damage: 3,
         critical: false,
       }),
       /* turn 3 */
       Struck({
         attacker: hero_at(7, full_pack),
         defender: orc,
         damage: 3,
         critical: false,
       }),
       Struck({
         attacker: wounded_orc,
         defender: hero_at(7, full_pack),
         damage: 3,
         critical: false,
       }),
       /* turn 4 */
       Used({
         item: Potion(3),
         effect: Healed(3),
       }),
       Struck({
         attacker: wounded_orc,
         defender: hero_at(7, [Key(Brass)]),
         damage: 3,
         critical: false,
       }),
       /* turn 5 */
       Struck({
         attacker: hero_at(4, [Key(Brass)]),
         defender: wounded_orc,
         damage: 6,
         critical: true,
       }),
       Slain(wounded_orc),
     ];
};

let%test "two things were slain and one blow was critical" = {
  let slain =
    List.filter(
      fun
      | Turn.Slain(_) => true
      | _ => false,
      Turn.log(final),
    );
  let criticals =
    List.filter(
      fun
      | Turn.Struck({critical: true, _}) => true
      | _ => false,
      Turn.log(final),
    );
  List.length(slain) == 2 && List.length(criticals) == 1;
};

let%test "nobody died and nothing was blocked" =
  !List.mem(Turn.Died, Turn.log(final))
  && !
       List.exists(
         fun
         | Turn.Blocked(_) => true
         | _ => false,
         Turn.log(final),
       );

/* --- the picture --- */

let%test "the final room draws as expected" =
  Render.describe(final) == "########\n"
  ++ "#.@....#\n"
  ++ "#......+\n"
  ++ "#..^...#\n"
  ++ "#.....>#\n"
  ++ "###*####\n"
  ++ "########\n"
  ++ "Turn 5  HP 4/10  Pack: a brass key\n";

let%test "the transcript ends with the orc falling" = {
  let lines = String.split_on_char('\n', transcript(final));
  List.nth(lines, List.length(lines) - 2) == "The orc falls.";
};
