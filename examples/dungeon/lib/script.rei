/* The room the scripted game is played in: a 6x4 floor, a plain door east,
   a brass-locked door south, stairs in the far corner. */
let level: Dungeon.room;

/* The game at turn zero: the hero in the corner, a goblin and an orc in a row beside them,
   a trap in the middle of the floor. */
let start: Turn.state;

/* The five commands of the scripted game. */
let commands: list(Turn.command);

/* Plays commands in order from a state. */
let play: (list(Turn.command), Turn.state) => Turn.state;

/* The state after the five commands. */
let final: Turn.state;

/* The drawn room and status line, then every event of the game in words, one per line. */
let transcript: Turn.state => string;
