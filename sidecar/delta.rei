/* A change from one JSON object to another, small when little changed.

   The change is itself JSON:

     {"set":   {FIELD: VALUE, …},   fields that are new or have a new value
      "lists": {FIELD: [OP, …], …}, fields holding a list in both objects
      "drop":  [FIELD, …]}          fields that are gone

   A list's OPs spell out the new list in order: `[I, N]` stands for the
   N items of the old list starting at index I, and `{"add": [ITEM, …]}`
   for items the old list did not have. Parts with nothing in them are
   left out. The page applies the same format (`patched` in `web/app.js`). */

/** [diff(old, now)] is the change that turns [old] into [now]; [None]
    unless both are objects. */
let diff: (Yojson.Safe.t, Yojson.Safe.t) => option(Yojson.Safe.t);

/** [apply(old, change)] is [old] with the change made. Fields keep their
    order; new ones go last. */
let apply: (Yojson.Safe.t, Yojson.Safe.t) => Yojson.Safe.t;
