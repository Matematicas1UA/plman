# PLMan
A Pacman-like game where the user controls the pacman with Prolog code. 

## Requirements
* Linux, Mac OSX or Windows.
* A bash shell (On Windows, [Cygwin](https://www.cygwin.com/) is recommended)
* [SWI-Prolog](http://www.swi-prolog.org/).

## Basic Use
Follow these steps after downloading PLMan.

1. Open a *bash terminal* and navigate into the PLMan folder (plman script should be there)
   1. If `plman` script does not have execution permission, use `chmod +x plman`
2. Grab a PLMan *map*. There are many examples included under the folder `maps/`
3. Create a minimal *solution* file for that map. You may use `example_solution.pl`
4. *Launch* PLMan: `./plman mapfile.pl solutionfile.pl`
   1. Use SPACE key to advace step by step.
   2. Use ESC key to end execution.

## Example of a minimal solution file

    %% Include PLMan API interface for developing map solutions
    :- use_module('pl-man-game/main').
    
    %% Define a simple fact that makes true the action of PLMan not moving.
    do(move(none)).

## Valid PLMan actions and sensors

Valid plman actions include these:

    move(DIR)  %% Move 1 step towards DIR.           Valid DIRs [ none, left, right, up, down ]
    get(DIR)   %% Gets an object.                    Valid DIRs [ here, left, right, up, down ]
    use(DIR)   %% Uses the object PLMan is holding.  Valid DIRs [ here, left, right, up, down ]
    drop(DIR)  %% Drops the object PLMan is holding. Vaild DIRs [ here, left, right, up, down ]

Also, PLMan has a number of given sensor that can be used to ask about the environment, reason, and deduce next action to be performed:

    see(normal, DIR, OBJ)  %% True if there is an object OBJ in the next cell in the direction DIR. 
                           %% Valid DIRs  [ here, up, down, left, right, up-left, up-right, down-left, down-right ]
    
    see(list, DIR, LIST)   %% Unifies with a LIST containing 1 object for each visible cell in the direction DIR. 
                           %% Valid DIRs are [ left, right, up, down ]
    
    havingObject                  %% True if PLMan is holding an object
    havingObject(appearance(APP)) %% True if the object that PLMan holds has the appearance OBJ
    havingObject(name(N))         %% True if the object PLMan holds has the name N
    
    hear(normal, SND)      %% True if PLMan hears the sound SND produced by a nearby entity. 
                           %% Sound messages depend on entities and may be different for each new map.
    hear(list, LIST)       %% Unifies wit a LIST of sounds produced by entities nearby.

## Screenshots
![PLMan Maze 211](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze211.png)
![PLMan List Sensor Range](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_list_sensor.png)
![PLMan Normal Sensor Range](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_normal_sensor.png)
![PLMan Maze 149](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze149.png)
![PLMan Maze 213](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze213.png)
![PLMan Maze 214](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze214.png)
![PLMan Maze 248](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze248.png)
![PLMan Maze 256](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze256.png)
![PLMan Maze 108](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze108.png)
![PLMan Maze 121](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze121.png)
![PLMan Maze 124](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze124.png)
![PLMan Maze 401](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze401.png)
![PLMan Maze 409](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze409.png)

## License

Copyright (C) 2007-2020 Francisco J. Gallego-Durán <fjgallego@ua.es>

PLMan is distributed under [GNU GPL v3 License](https://github.com/Matematicas1UA/plman/blob/master/LICENSE).
