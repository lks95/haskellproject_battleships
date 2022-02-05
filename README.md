# Haskell Project 2022

## Haskell Project for programming languages ​​and concepts
We are developing a Battleship game, in which it is possible to place ships of different sizes on a 10x10 coordinate System.
Each player can arrange the ships within the coordinate system as desired (vertically or horizontally).
A game has 2 Players who take turns, one after the other. Each turn, a Player can fire one shot. 
If he hits he got another shot which keeps on going until misses. The ships are placed in separated Grid Systems.
The Game is over as soon as all ships of one player have sunk.



## Team

| Name                | Student ID |
| ------------------- | ---------- |
| Tristan	          | s86903     |
| Carolyne            | s897540	   |
| Lukas		          | s86901	   |


## Milestones

| Date       | Milestone                             |
| ---------- | ------------------------------------- |
| 07.02.2022 | Projektpräsentation                   |


## Important Functions (brainstorming)
* Initializing the playing field (10x10 or 15x15)
* Placing Ships (Size?)
    * 1x Length 2
    * 2x Length 3
    * 1x Length 4
    * 1x Length 5
* Mark destroyed Ships (adjacent ships)
* Placing Ships right next to other ships is not possible
* Shoot down Ships
* Start the game (Main, Play-function)
* Functionality between 2 Players (Player1, Player2 &#8594; Names)
* Functionality to play against Computer (?)
* Function to put all created Ships on the playing field
* How will the Coordinates be safed? (1|1) for ex.? Similiar like Adressverwaltung, serperated txt file, mark hits in txt
* How are the Positions of the Ships depicted? Positions of ships shown via coordinates as well?
* It should print out following informations:
    * (1|1) was a hit!
    * (1|1) was no hit! (hit into the void)
    * (1|2) was a hit and the ship sunk!
    * Game ending:
        * Congratz <Player_name> won!
        * Show remaining, not sunk ships (Grid, visual? or just coordinates?)
        * "Wanna play again?" or "End Battleships Programm"
* Exception Handling for hits and placements of ships outside the coordinate system
* Exception Handling for too many ships
* Exception Handling for wrongly arranged ships (e.g. (1|1) and (2|2))
* Exception Handling for input format
* Exception Handling if a user already fired a shot at the coordinates e.g. "You already fired a shot at (1|1) and it was a miss!" or "You already fired a shot at (1|1) and it was a hit!!"

## Tech-Stack

* Haskell