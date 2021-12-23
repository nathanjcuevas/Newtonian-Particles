# Newtonian-Particles

### Overview

This project explores simple numerical simulation with a graphics component in Haskell. The simulation consists of *m* particles (of even mass and size) that are enclosed in a rectangular 2-D container. The particles each have an initial position and velocity (call this the "state"), and are free to bounce around the container and each other. Each particle will adhere to basic Newtonian physics and will be influenced by gravitational forces, conservation of energy and conservation of linear momentum, friction and inelastic energy loss. Factors such as spin, angular momentum, aerodynamic drag will not be simulated here. The input of the program will be a csv file of the initial states of each particle, and the program will animate the result over time.

### How to Run

**building with cabal**

Although not required, it is recommended that cabal is used to build/run the project. Once cabal is installed, you can run the project by using the following command:

```shell
cabal v2-run Newtonian-Particles -- <path to initial condition> <path to config> [-animate]
```

**building with ghc**

A workaround to compiling the project without cabal is to just use ghc. To run the project using ghc:

```shell
cd app/
ghc Main.hs -package parallel -package gloss
./Main <path to initial condition> <path to config> [-animate]
```

Note that the above command specifies the `parallel` and `gloss` dependencies, but that may change over time. Running the build using cabal will always have the updated dependencies. 

To clean up any artifacts from the build process from the `app/` directory, run:

```shell
../scrpts/cleanAppDir.sh
```

### Program Arguments

This program requires the user to specify the path to the initial conditions csv file and the config csv file.

**config csv**

This program supports different simulation parameters:

| param name | default value | description                                   |
| ---------- | ------------- | --------------------------------------------- |
| `g`        | 9.81          | acceleration due to gravity                   |
| `alpha`    | 0.93          | elastic loss coefficient                      |
| `beta`     | 0.93          | friction loss coefficient                     |
| `width`    | 740           | width of the simulation container             |
| `height`   | 740           | height of the simulation container            |
| `radius`   | 3             | radius of the particle                        |
| `fps`      | 45            | steps per second that the simulation will run |
| `totTime`  | 60            | total time (in seconds) of the simulation     |

If an empty csv file is provided, or if certain params aren't specified in this file, then the program will resort to default values. Put all of the param values you want to update in a single line comma delimited format of the form `<param name 1>=<param value 1>,<param name 2>=<param value 2>,...`

example:

```
alpha0.82,totTime=120,g=20
```

**initial conditions csv**

This csv file should have 4 columns, with each column representing (in order) -- initial x position, initial y position, initial x velocity, initial y velocity. The center of the container is at (0,0) and the edges of the container can are determined by the `width` and `height` parameters. There should also be $m$ rows, where $m$ is the number of particles that is to be simulated. 

example for 7 particles:

```
0,0,150,-33
-100,0,0,-70
50,120,-30,50
150,0,80,-5
0,-200,15,100
120,100,140,0
-80,-100,0,14
```

**flags**

Currently, the only supported flag is `-animate` which will which will run an animation of the simulation in an external window. If no flag is specified, the program will simply print the final state of each of the particles after the simulation has completed. 

**preset example**

For a preset example run the following (assuming cabal is installed):

```
cabal v2-run Newtonian-Particles -- ./presets/m150.csv ./config/config.csv -animate
```



