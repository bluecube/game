A beginning of a very simple game to learn Haskell.

![screenshot](https://github.com/bluecube/game/raw/master/screenshot.png)

TODO
====
(in no particlular order)

* Player
  * Displaying (special particle type, crosshair)
  * moving, shooting
* Projectiles
  * Explosion types
  * Particle update? (rockets)
* Materials
  * Solids
    * another peak in the low distance force interaction
  * Changeable friction for low distance interaction
* Menu
* Particle tree operations
  * Rebuilding particle tree [Done]
    * Once number of opened inner nodes increases too much? [Done]
    * in background?
    * Maybe use better heuristic for finding seeds when building [Partially]
  * Insert and remove particles.
