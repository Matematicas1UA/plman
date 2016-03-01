# PLMan
A Pacman-like game where the user controls the pacman with Prolog code. 

## Requirements
* Linux, Mac OSX or Windows.
* A bash shell (On Windows, [Cygwin](https://www.cygwin.com/) is recommended)
* [SWI-Prolog](http://www.swi-prolog.org/).

## Basic Use
Follow these steps after downloading PLMan.

1. Open a *bash terminal* and navigate into the PLMan folder (plman script should be there)
2. Grab a PLMan *map*. There are many examples included under the folder maps/
3. Create a minimal *solution* file for that map. 
4. *Launch* PLMan: `./plman mapfile.pl solutionfile.pl mainrule` (`mainrule` is the rule that controls behaviour of pacman in your file).

## Example of a minimal solution file

    %% Include PLMan API interface for developing map solutions
    :- use_module('pl-man-game/main').
    
    %% Define a simple rule that tells PLMan to do noting at each turn
    my_rule :- doAction(move(none)).

# Screenshots
![PLMan Maze 211](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze211.png)
![PLMan List Sensor Range](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_list_sensor.png)
![PLMan Normal Sensor Range](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_normal_sensor.png)
![PLMan Code Example](https://github.com/Matematicas1UA/plman/blob/master/docs/scrshots/plman_maze140_code.png)
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
