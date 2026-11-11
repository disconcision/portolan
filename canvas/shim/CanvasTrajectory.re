/* Shim for Hazel's CanvasTrajectory (record/replay through Hazel's tool
   handler, not ported): the hooks the copied files call. */
let on_change: ref(unit => unit) = ref(() => ());
let busy = (_: bool): unit => ();
