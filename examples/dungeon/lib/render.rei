/* The characters things are drawn with. */
module Glyph: {
  /* '.' floor, '#' wall, '>' stairs, '\'' open door, '+' shut door, '*' locked door. */
  let tile: Dungeon.Tile.t => char;

  /* '@' the player, 'g' 'o' 's' goblin, orc, skeleton, '^' an armed trap, ',' a sprung one. */
  let creature: Creatures.t => char;
};

/* The room as lines of text, one per row, with creatures drawn over the tiles they stand on. */
let map: Turn.state => string;

/* One line: the turn count, the player's hit points, and what they carry. */
let status: Turn.state => string;

/* The map, then the status line, each line ending in a newline. */
let describe: Turn.state => string;

/* One event told in a sentence. */
let narrate: Turn.event => string;
