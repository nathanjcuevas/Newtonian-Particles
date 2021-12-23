# Newtonian-Particles

### Overview

Haskell program to simulate Newtonian motion of $m$ particles in a closed container. 

### How to Build

Can build two ways:

- Use cabal
  - Need to have cabal installed
  - example command: `cabal v2-run Newtonian-Particles -- ./presets/m100.csv ./config/config.csv -animate`
- Use ghc to directly compile Main in the `app/` directory (not recommended)