
  Games for Single-User HP21xx HPBASIC
  ====================================
  
These games were gathered from various sources including Time Share BASIC
archives, newly written and other systems. Almost all of the vintage games
have been extensively modified to work with the single-user version of
HPBASIC, including eliminating string usage and converting to numerical
input, adding a prompt for a random seed number, eliminating terminal codes
and unsupported functions, adding subroutines for number prints to avoid
disrupting text spacing, and in general modifying the code to my liking.

The binaries are based on basic2.abs for 32KW+ systems and bas8ovl.abs for
8KW+ systems. Configuration is TTY=11, PTR=12, PTP=13, configured PTR/PTP is
not required to just run the programs but some sort of PTR emulator or other
loading device is needed to load the binaries.

Binaries with base names ending in "8k" are for 8KW machines, binary names
ending in "8" require an 8KW machine with an ANSI-capable terminal, binary
names ending in "ov" require a 32KW machine with an ANSI terminal, other
binaries are for a 32KW machine with no special terminal requirements.

Binaries are in the progbins directory, BASIC source is in the progsrc
directory. Only the programs in progbins are described, progsrc contains
other programs and code that might be interesting.


  BEASTI
  ======

This is an old game found on an Access image in account D600. The original
used HP terminal codes to overprint the board, first I modified it to work
with TSB-E with "hacked" terminal codes, then modified for plain HPBASIC,
made a trimmed down version for 8KW, which was then modified to make use
of the overlay calls to restore the screen codes.

BEASTIPT.txt - non-terminal HPBASIC version
BEASTI8K.txt - trimmed-down version for 8KW
BEASTIOV.txt - ANSI-terminal version for 8KW+ w/ overlay
beasti8.abs - 8KW+ binary with BEASTIOV.txt source
beastiov.abs - 32KW+ binary with BEASTIOV.txt source

TIME = 1
.---------------------.
:     0           0   :
:         0   0       :
:       +             :
:       0             :
:                     :
: 0                   :
:         0           :
: 0       *           :
:                     :
:               0 0   :
`---------------------'
MOVE :

+ is the beasti, 0 are organisms, * is the player. Moves are made by typing
2,4,6,8 on the numeric key pad, the goal of the game is to consume all the
organisms in 50 or less moves without being consumed by the beasti.


  GOLF
  ====

A conversion of the old GOLF game from "5-5-84ICL" account A000.

GOLFPT.txt - source code
golf.abs - 32KW+ binary

WELCOME TO THE TIES TIMESHARING 18 HOLE CHAMPIONSHIP COURSE
TO GET A DESCRIPTION OF CLUBS, ETC.
TYPE 0 FOR A CLUB NK. WHEN REQUESTED
WHAT IS YOUR HANDICAP ?-1
PGA RULES HANDICAP=0-30
WHAT IS YOUR HANDICAP ?5
OH-OH, A HOT SHOT!
DIFFICULTIES AT GOLF INCLUDE:
  0=HOOK, 1=SLICE, 2=POOR DISTANCE, 4=TRAP SHOTS, 5=PUTTING
WHICH IS YOUR WORST ?4
ENTER 1 IF READY TO GO ?1
YOU ARE AT TEE OFF HOLE 1, DISTANCE 361 YARDS PAR 4
ON YOUR RIGHT IS ADJACENT FAIRWAY.
ON YOUR LEFT IS ADJACENT FAIRWAY.
WHAT CLUB DO YOU WANT ?0
---------------------------------------------------------------------
HERE'S YOUR BAG OF CLUBS
WOODS (FULL SWING ONLY):  1 DRIVER  2 BRASSIE  3 SPOON
IRONS (FULL SWING ONLY):  12 TWO IRON   19 NINE IRON
IRONS (LESS THEN FULL SWING)...
22 TWO IRON - PARTIAL SWING
29 NINE IRON - PARTIAL SWING
WHEN YOU REACH THE GREEN IT WILL BE ASSUMED THAT YOU ARE
USING A PUTTER. THE PUTT POTENCY NO. REFERS TO THE STRENGTH
WITH WHICH THE BALL IS PUTTED. USE NUMBERS GREATER THAN
ZERO, INCREASING THE NUMBER FOR GREATER DISTANCE.
YOU WILL BE ASKED FOR 'PERCENT FULL SWING' ON CLUBS 22-29.
THIS SHOULD BE A NUMBER FROM 1 TO 99.
---------------------------------------------------------------------
WHAT CLUB DO YOU WANT ? 


  GOMOKU
  ======

A conversion of the old GOMOKU game from "5-5-84ICL" account A000.

GOMOKUPT.txt - source code for the full version
GOMOKU8K.txt - source code for a trimmed-down version
gomoku.abs - 32KW+ binary with GOMOKUPT.txt
gomoku8k.abs - 8KW+ binary with GOMOKU8K.txt

ENTER 1 FOR INSTRUCTIONS  ?1

GOMOKU IS A TRADITIONAL JAPANESE GAME PLAYED ON A 19X19 BOARD.
THE OBJECT IS TO OCCUPY FIVE ADJACENT POINTS IN A STRAIGHT LINE
(HORIZONTAL, VERTICAL, OR DIAGONAL) ANYWHERE ON THE BOARD.

THIS PROGRAM PLAYS GOMOKU ON A 9X9 BOARD.
EACH MOVE IS A TWO-DIGIT NUMBER - THE FIRST DIGIT IS THE ROW
AND THE SECOND IS THE COLUMN.

YOU CAN CHANGE THE CURRENT BOARD-PRINTING OPTION BY ADDING
A THIRD DIGIT TO THE MOVE NUMBER:

   0  SUPPRESS PRINTING ENTIRELY
   1  PRINT ONLY THE OCCUPIED POINTS
   2  PRINT THE ENTIRE BOARD (DEFAULT)

(FOR EXAMPLE, IF YOU MOVE  372  THEN YOUR MOVE IS
THE POINT AT ROW 3, COLUMN 7 AND THE ENTIRE BOARD WILL BE
PRINTED UNTIL YOU CHANGE THE PRINT OPTION.)

ON THE BOARD, O = YOU, X = ME, AND (IF OPT.2) . = EMPTY.


ENTER 1 TO MOVE FIRST  ?0
I MOVE TO 55

   1  2  3  4  5  6  7  8  9
1  .  .  .  .  .  .  .  .  .
2  .  .  .  .  .  .  .  .  .
3  .  .  .  .  .  .  .  .  .
4  .  .  .  .  .  .  .  .  .
5  .  .  .  .  X  .  .  .  .
6  .  .  .  .  .  .  .  .  .
7  .  .  .  .  .  .  .  .  .
8  .  .  .  .  .  .  .  .  .
9  .  .  .  .  .  .  .  .  .
WHAT IS YOUR MOVE  ?54
I MOVE TO 45

   1  2  3  4  5  6  7  8  9
1  .  .  .  .  .  .  .  .  .
2  .  .  .  .  .  .  .  .  .
3  .  .  .  .  .  .  .  .  .
4  .  .  .  .  X  .  .  .  .
5  .  .  .  O  X  .  .  .  .
6  .  .  .  .  .  .  .  .  .
7  .  .  .  .  .  .  .  .  .
8  .  .  .  .  .  .  .  .  .
9  .  .  .  .  .  .  .  .  .
WHAT IS YOUR MOVE  ? 

It sounds easy but it is not. The 8KW version has a "game" prompt
that can adjust the aggressiveness of algorithm, 1 is normal, lower
values cause it to spend more effort on blocking, higher values
make it concentrate more on getting pieces in a row, possibly
missing a block.


  HAMURABI
  ========

A conversion of the old HAMRBI game from "5-5-84ICL" account A000.
It's mostly like the original but I added some logic to properly
print single things.. 1 PERSON 5 PEOPLE 1 BUSHEL 5 BUSHELS etc.

HAMRBIPT.txt - source code
HAMRBI8K.txt - trimmed-down version (comments removed)
hamurabi.abs - 32KW+ binary with HAMRBIPT.txt
hamrbi8k.abs - 8KW+ binary with HAMRBI8K.txt

ENTER SEED NUMBER?42

HAMURABI - WHERE YOU GOVERN THE ANCIENT KINGDOM OF SUMERIA.
THE OBJECT IS TO FIGURE OUT HOW THE GAME WORKS!!
(IF YOU WANT TO QUIT, SELL ALL YOUR LAND.)

HAMURABI, I BEG TO REPORT THAT LAST YEAR

0 PEOPLE STARVED AND 5 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 100.

WE HARVESTED 3000 BUSHELS AT 3 BUSHELS PER ACRE.
RATS DESTROYED 200 BUSHELS LEAVING 2800 BUSHELS IN THE STOREHOUSES.

THE CITY OWNS 1000 ACRES OF LAND.
LAND IS WORTH 17 BUSHELS PER ACRE.

HAMURABI . . .

BUY HOW MANY ACRES? 


  LANDER
  ======

A conversion of the old LANDER game from the "HP BASIC PROGRAM LIBRARY".

LANDERPT.txt - source code
LANDER8K.txt - trimmed-down version (comments removed)
lander.abs - 32KW+ binary with LANDERPT.txt source
lander8k.abs - 8KW+ binary with LANDER8K.txt source

WELCOME TO THE PUC SCHOOL FOR BUDDING ASTRONAUTS!

YOU ARE AT THE CONTROLS OF A ROCKET LANDING VEHICLE.
INITIALLY YOU ARE A GIVEN DISTANCE ABOVE THE SURFACE MOVING
DOWNWARD (VELOCITY IS NEGATIVE). YOU CHOOSE THE AMOUNT OF FUEL
TO BE BURNED DURING THE NEXT ONE SECOND OF TIME.

   IF YOU BURN ZERO, THEN YOU WILL FALL FASTER BECAUSE OF GRAVITY.
   IF YOU BURN EXACTLY THAT REQUIRED TO OVERCOME GRAVITY, THEN
     YOUR VELOCITY WILL BE CONSTANT.
   IF YOU BURN MORE, THEN YOU WILL SLOW DOWN OR EVEN START TO MOVE
     UPWARD (VELOCITY IS POSITIVE)!

THE IDEA IS TO GET THE LANDER DOWN TO THE SURFACE,
LANDING WITH AS LITTLE VELOCITY AS POSSIBLE. THERE IS
MORE THAN ENOUGH FUEL, BUT BE CAREFUL NOT TO WASTE IT!

LANDING ON THE MOON IS EASIER, TRY THAT FIRST.

GOOD LUCK AND HAPPY LANDINGS!

LOCATION:            1)MOON      2)EARTH        ?1
INITIAL CONDITIONS:  1)STANDARD  2)OLD  3)NEW   ?1

INITIAL HEIGHT:      500  FEET
INITIAL VELOCITY:   -50   FEET/SEC
TOTAL FUEL SUPPLY:   120  UNITS
MAXIMUM BURN:        30   UNITS/SEC
AMOUNT OF BURN TO CANCEL GRAVITY:   5    UNITS/SEC

 TIME           HEIGHT        VELOCITY        FUEL          BURN

 0              500           -50             120           ? 
 
 
  REVERSI
  =======

The object of this game is to surround the opponent's pieces in any direction
and "reverse" them. The game was invented in the late 1800's, starting config
was not specified. The modern version with the initial 4 starting pieces was
formulated in 1971 by Goro Hasegawa and marketed under the name OTHELLO
(trademarked name owned by Pressman Toy Corp).

OTHPT.txt - a version I wrote with side-by-side boards
OTHELL8K.txt - an earlier trimmed-down version of my OTH program
OTHOV.txt - a ANSI version of my OTH program for the overlay
OTHOV8.txt - a trimmed-down ANSI version for the 8KW overlay
SMOTHE8K.txt - a conversion of OTHEL from an Access system account T000
SMOTHEOV.txt - OTHEL with the screen codes restored using the overlay
othpt.abs - 32KW+ binary with OTHPT.txt source
othov.abs - 32KW+ binary with OTHOV.txt source
oth8.abs - 8KW+ binary with OTHOV8.txt source
smothe8k.abs - 8KW+ binary with SMOTHE8K.txt source

SMOTHE8K...

The simplicity of this program is intriguing. The original program had HP
terminal codes to clear the screen and "animate" the board, the code as I
found it functioned after the usual conversions but just barely, apparently
it was a work-in-progress. Most of the logic mods were in the scoring and
bad move rejection code, added a FACTOR prompt to seed the random number
generator and control random move generation (original value was 10),
and changed the board display, adding a border and moving the Y index
numbers to the right to avoid bad formatting. The SMOTHEOV.txt contains
a version of the program with overlay calls for ANSI terminal codes.

== SMALL OTHEL ==
ENTER MOVES AS YX
FACTOR (1-10) ?5

    1 2 3 4 5 6 7 8
  +-----------------+
  : . . . . . . . . : 1
  : . . . . . . . . : 2
  : . . . . . . . . : 3
  : . . . X O . . . : 4
  : . . . O X . . . : 5
  : . . . . . . . . : 6
  : . . . . . . . . : 7
  : . . . . . . . . : 8
  +-----------------+
   YOUR TURN ?34

    1 2 3 4 5 6 7 8
  +-----------------+
  : . . . . . . . . : 1
  : . . . . . . . . : 2
  : . . . O . . . . : 3
  : . . . O O . . . : 4
  : . . . O X . . . : 5
  : . . . . . . . . : 6
  : . . . . . . . . : 7
  : . . . . . . . . : 8
  +-----------------+

    1 2 3 4 5 6 7 8
  +-----------------+
  : . . . . . . . . : 1
  : . . . . . . . . : 2
  : . . . O X . . . : 3
  : . . . O X . . . : 4
  : . . . O X . . . : 5
  : . . . . . . . . : 6
  : . . . . . . . . : 7
  : . . . . . . . . : 8
  +-----------------+
   YOUR TURN ?

OTHPT...

I wrote a few Reversi-playing programs for TSB-E in 1996 to experiment with
algorithms. OTHPT is a single-user conversion of one of the fancier versions
that printed side-by-side boards and had a lookahead option, which increases
strength but makes the program very slow when running on a real HP21xx.

COMPUTER IS X, YOU ARE O
ENTER MOVES AS YX (11-88)
TO PASS ENTER 0 FOR MOVE
LEVEL 0 OR 1?0

   1 2 3 4 5 6 7 8
 1                 1
 2                 2
 3                 3
 4       X O       4
 5       O X       5
 6                 6
 7                 7
 8                 8
   1 2 3 4 5 6 7 8

YOU: 2     ME: 2
MOVE ?34
                              MY MOVE IS 53

   1 2 3 4 5 6 7 8            1 2 3 4 5 6 7 8
 1                 1        1                 1
 2                 2        2                 2
 3       O         3        3       O         3
 4       O O       4        4       O O       4
 5       O X       5        5     X X X       5
 6                 6        6                 6
 7                 7        7                 7
 8                 8        8                 8
   1 2 3 4 5 6 7 8            1 2 3 4 5 6 7 8

YOU: 3     ME: 3
MOVE ?

OTHOV...

This was adapted from an earlier version of OTH, before adding lookahead etc.
The source for non-screen code version is in OTHELL8K.txt, this version has
an expanded board display with ANSI codes for clearing and homing the screen
and colorizing the pieces.

 === OTH-OV ===
 COMPUTER IS X, YOU ARE O
 ENTER MOVES AS YX (11-88)
 0 TO PASS, -1 TO REDISPLAY

 ENTER RANDOM SEED NUMBER ?22

[clears screen, O pieces are blue, X pieces are red]

     1   2   3   4   5   6   7   8
   .-------------------------------.
 1 |   |   |   |   |   |   |   |   | 1
   |---+---+---+---+---+---+---+---|
 2 |   |   |   |   |   |   |   |   | 2
   |---+---+---+---+---+---+---+---|
 3 |   |   |   |   |   |   |   |   | 3
   |---+---+---+---+---+---+---+---|
 4 |   |   |   | X | O |   |   |   | 4
   |---+---+---+---+---+---+---+---|
 5 |   |   |   | O | X |   |   |   | 5
   |---+---+---+---+---+---+---+---|
 6 |   |   |   |   |   |   |   |   | 6
   |---+---+---+---+---+---+---+---|
 7 |   |   |   |   |   |   |   |   | 7
   |---+---+---+---+---+---+---+---|
 8 |   |   |   |   |   |   |   |   | 8
   `-------------------------------'
     1   2   3   4   5   6   7   8

 YOU: 2     ME: 2
 MOVE ?
 
The 8KW version in oth8.abs/OTHOV8.txt is similar except it displays
the computer pieces  in brown rather than red, simplifies the prompts,
and doesn't tally the score at the end of the game (redundant).
 
 
  SEA BATTLE
  ==========

This program was originally written for a TSB system, converted to MSBASIC
for David Ahl's More Computer Games, the source I worked from was from an
archive of TRS80 programs. It has been extensively modified, I added commands
for pass time, toggle automap and to print a help sreen. In addition to the
usual mods, added a prompt for play level (3 is the stock parameters), altered
the ship and sea monster movement code and fixed a bug so the game terminates
after taking out the last enemy ship with a missile.

SEABATPT.txt - source code
seabat.abs - 32KW+ binary

*** SEA BATTLE ***
ENTER RANDOM SEED NUMBER ?33
DIFFICULTY LEVEL 1-EASY 2-MEDIUM 3-NORMAL ?2

YOU MUST DESTROY 12 ENEMY SHIPS TO WIN.
 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
 .    \S/                                                 .
 .     $                                                  .
 .           $                                            .
 .       \S/                                              .
 .                                                        .
 .        . !H!      *********         \S/               \S/
 .             -#-   ************ $              .        .
\S/         \S/   *********-#-******                      .
 .                ******\S/(X)   ***             $  $     .
 $                ******      ******                      .
 .          -#-      ******   ***                         .
 . \S/                  ***                  \S/          .
 .                       .                       $        .
 .                                                        .
 .                                                        .
 .             -#-            \S/                         .
 .                                                        $
 .                                                        .
 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
WHAT ARE YOUR ORDERS ?-1

THE COMMANDS ARE:
#0: NAVIGATION - MOVES YOUR SUBMARINE
#1: SONAR - OPTION 0 PRINTS MAP, 1 SENSES DIRECTIONS
#2: TORPEDO CONTROL - FIRES A TORPEDO
#3: MISSILE CONTROL - FIRES A POLARIS MISSLE
#4: MANUEVERING - SETS DEPTH
#5: STATUS/DAMAGE REPORT
#6: HEADQUARTERS - MUST BE IN DOCKING RANGE
#7: SABOTAGE - SEND MEN ON A SABOTAGE MISSION
#8: POWER CONVERSION - FUEL TO POWER AND VICE VERSA
#9: SURRENDER
#10: PASS TIME - USE THIS TO REPAIR DAMAGE
#11: TOGGLE AUTOMAP - IF DISABLED USE SONAR OPTION 0
#12: PRINT SYMBOLS AND DIRECTIONS

WHAT ARE YOUR ORDERS ?12

MAP SYMBOLS...
***   DRY LAND
(X)   YOUR SUBMARINE
!H!   YOUR HEADQUARTERS
\S/   ENEMY SHIP
-#-   SEA MONSTER
 $    UNDERWATER MINE
 .    UNKNOWN
DIRECTIONS...
8 1 2
 \'/    WHEN NAVIGATING IT TAKES 100 UNITS OF
7-*-3   POWER TO MOVE ONE POSITION.  MISSILES
 /.\    REQUIRE 75 LBS FUEL FOR EACH POSITION.
6 5 4

WHAT ARE YOUR ORDERS ?


TREK
====

This is a conversion of the STTR1 program from the "HP BASIC PROGRAM LIBRARY",
aka Star Trek (trademark Paramount Pictures). This program spawned numerous
variants for many systems, in fairly stock to highly modified forms. This
particular conversion mostly preserves the stock gameplay, coordinates ended
up coming out X,Y rather than the original Y,X notation, minor changes were
made to the display and computer functions to assist in counting distances.
The code is kind of a mess... it was the first program I converted to the
single-user HPBASIC and at the time I didn't know much about it.

TREKPT.txt - source code
trek.abs - 32KW+ binary

                          STAR TREK

ENTER 1 OR 2 FOR INSTRUCTIONS (ENTER 2 TO PAGE) ?1

     INSTRUCTIONS:

<*> = ENTERPRISE
+++ = KLINGON
>!< = STARBASE
 *  = STAR

COMMAND 0 = WARP ENGINE CONTROL
  'COURSE IS IN A CIRCULAR NUMERICAL         4    3    2
  VECTOR ARRANGEMENT AS SHOWN.                `.  :  .'
  INTERGER AND REAL VALUES MAY BE               `.:.'
  USED.  THEREFORE COURSE 1.5 IS             5---<*>---1
  HALF WAY BETWEEN 1 AND 2.                     .':`.
                                              .'  :  `.
  A VECTOR OF 9 IS UNDEFINED, BUT            6    7    8
  VALUES MAY APPROACH 9.
                                               COURSE
  ONE 'WARP FACTOR' IS THE SIZE OF
  ONE QUADRANT.  THEREFORE TO GET FROM
  QUADRANT 5,6 TO 5,5 YOU WOULD USE COURSE 3, WARP
  FACTOR 1. COORDINATES ARE SPECIFIED USING X,Y NOTATION
  WITH X 1-8 FROM LEFT-RIGHT AND Y 1-8 FROM TOP-BOTTOM.

COMMAND 1 = SHORT RANGE SENSOR SCAN
  PRINTS THE QUADRANT YOU ARE CURRENTLY IN, INCLUDING
  STARS, KLINGONS, STARBASES, AND THE ENTERPRISE; ALONG
  WITH OTHER PERTINATE INFORMATION.

COMMAND 2 = LONG RANGE SENSOR SCAN
  SHOWS CONDITIONS IN SPACE FOR ONE QUADRANT ON EACH SIDE
  OF THE ENTERPRISE IN THE MIDDLE OF THE SCAN.  THE SCAN
  IS CODED IN THE FORM XXX, WHERE THE UNITS DIGIT IS THE
  NUMBER OF STARS, THE TENS DIGIT IS THE NUMBER OF STAR-
  BASES, THE HUNDREDS DIGIT IS THE NUMBER OF KLINGONS.

COMMAND 3 = PHASER CONTROL
  ALLOWS YOU TO DESTROY THE KLINGONS BY HITTING HIM WITH
  SUITABLY LARGE NUMBERS OF ENERGY UNITS TO DEPLETE HIS
  SHIELD POWER.  KEEP IN MIND THAT WHEN YOU SHOOT AT
  HIM, HE GONNA DO IT TO YOU TOO.

COMMAND 4 = PHOTON TORPEDO CONTROL
  COURSE IS THE SAME AS USED IN WARP ENGINE CONTROL
  IF YOU HIT THE KLINGON, HE IS DESTROYED AND CANNOT FIRE
  BACK AT YOU.  IF YOU MISS, HE WILL SHOOT HIS PHASERS AT
  YOU.
   NOTE: THE LIBRARY COMPUTER (COMMAND 7) HAS AN OPTION
   TO COMPUTE TORPEDO TRAJECTORY FOR YOU (OPTION 2).

COMMAND 5 = SHIELD CONTROL
  DEFINES NUMBER OF ENERGY UNITS TO ASSIGN TO SHIELDS
  ENERGY IS TAKEN FROM TOTAL SHIP'S ENERGY.

COMMAND 6 = DAMAGE CONTROL REPORT
  GIVES STATE OF REPAIRS OF ALL DEVICES.  A STATE OF REPAIR
  LESS THAN ZERO SHOWS THAT THAT DEVICE IS TEMPORARALY
  DAMAGED.

COMMAND 7 = LIBRARY COMPUTER
  THE LIBRARY COMPUTER CONTAINS THREE OPTIONS:
    OPTION 0 = CUMULATIVE GALACTIC RECORD
     SHOWS COMPUTER MEMORY OF THE RESULTS OF ALL PREVIOUS
     LONG RANGE SENSOR SCANS
    OPTION 1 = STATUS REPORT
     SHOWS NUMBER OF KLINGONS, STARDATESC AND STARBASES
     LEFT.
    OPTION 2 = PHOTON TORPEDO DATA
     GIVES TRAJECTORY AND DISTANCE BETWEEN THE ENTERPRISE
     AND ALL KLINGONS IN YOUR QUADRANT


ENTER SEED NUMBER ?22
INITIALIZING...

YOU MUST DESTROY 17 KINGONS IN 30 STARDATES WITH 1 STARBASE

-=--=--=--=--=--=--=--=-
          *           *
                         STARDATE  2500
                      *  CONDITION GREEN
                      *  QUADRANT  5,4
                         SECTOR    8,7
 *                       ENERGY    3000
                   * <*> SHIELDS   0
       *  *              PHOTON TORPEDOES 10
-=--=--=--=--=--=--=--=-
COMMAND ?2
LONG RANGE SENSOR SCAN FOR QUADRANT 5,4
-------------------
 8    : 2    : 8
-------------------
 105  : 8    : 302
-------------------
 2    : 3    : 6
-------------------
COMMAND ?


TREKD1
======

This is a conversion of a program was found in the "5-5-84ICL" archive
in account G000, filename "TREKD", comments call it "TREK2".

TREKD1.txt - source code
trekd1.abs - 32KW+ binary

+++ STAR TREK +++
DO YOU WANT INSTRUCTIONS (1=YES OR 0=NO) ?1
STAR TREK #2 INSTRUCTIONS:
YOU CAN DETERMINE THE DIFFICULTY OF THE GAME BY THE NUMBER OF
KLINGONS AGAINST YOU.
   1-3  MIDSHIPMAN (PLEBE 1ST CLASS)
   4-5  IN TRAINING
   6-8  COMMANDER
   9-10 CAPTAIN
  11-15  YOU'RE BUCKING FOR ADMIRAL
THE COMMANDS ARE:
-1 TO -15 TO FIRE PHASERS AT SHIPS 1-15
 0  END THE GAME
 1  POWER TO PHASERS
 2  POWER TO SHIELDS
 3  MOVE
 5  COOL POWER REACTORS
WARNING> DO NOT LET TEMP GO ABOVE 99% (OR ELSE ***BOOM***)
 THE KLINGONS START OUT HEADING TOWARD YOU AT VARYING DISTANCES
 FROM YOU.  THE SENSOR SCAN WILL SHOW THERE X,Y COORDINATES RE-
 LATIVE TO YOU (YOU ARE ALWAYS CONSIDERED TO BE AT 0,0), THEIR
 ASSIGNED NUMBER, THEIR DISTANCE FROM YOU AND THEIR SHIELD
 AND PHASER POWER.  THE FORM IS:
    11 /  11  KLINGON  1 AT 17    9 1
    IF YOU WISH TO MOVE (TO OVERTAKE OR ESCAPE FROM THE KLINGONS)
 YOU CAN DO SO BY USING COMMAND 3.  THIS WILL ALLOW YOU TO SPEED
 UP OR SLOW DOWN BY SIX UNITS PER POSITION (X AND Y) EACH TURN.
 YOUR DEGREE OF MOVEMENT IS SUBTRACTED FROM THE KLINGONS X,Y
 COORDINATES TO GIVE YOU THEIR RELATIVE POSITIONS.
    WHEN YOU FIRE AT A KLINGON YOUR PHASERS CONTINUE FIRING UNTIL
 THE KLINGON IS DESTROYED OR YOUR PHASERS ARE EXHAUSTED.  AFTER
 THEY HAVE FIRED YOU WILL BE TOLD YOUR REMAINING PHASER POWER AND
 YOU CAN THEN INPUT ANOTHER COMMAND.
   THE KLINGONS WILL FIRE AT YOU WHEN THEY ARE TWELVE TO SIX-
 TEEN UNITS AWAY FROM YOU.  THE KLINGONS CANNOT SLOW DOWN
 IMMEDIATELY SO THEY WILL PROBABLY GET AS CLOSE AS FIVE UNITS FROM
 YOU.  YOU SHOULD WAIT UNTIL THEY ARE CLOSER (INSTEAD OF FIRING
 AT LONG RANGE) TO CONSERVE PHASER POWER.
INPUT # OF SHIPS
?5
PHASERS AT  100  %
SHIELDS AT  66   %
TEMP OVERLOAD AT  0    %
SENSOR SWEEP
 10   /-20    KLINGON 1    AT 23    10    10
 10   /-21    KLINGON 2    AT 24    10    10
 28   /-18    KLINGON 3    AT 34    10    10
-18   /-14    KLINGON 4    AT 23    10    10
-17   / 26    KLINGON 5    AT 32    10    10
COMMAND ?


TTY TREK
========

This is a conversion of TTYTREK, found in the HP2000family Yahoo group.
It resembles the original concept but in this version the Klingons move
from quadrant to quadrant and have a home base in quadrant 1,1.

TTYTRKPT.txt - source code
ttytrek.abs - 32KW+ binary

WELCOME TO TTY TREK 3
1)LIST ALL 2)PAGE INSTRUCTIONS, OR ENTER SEED ?1

INSTRUCTIONS FOR TTY TREK 3

THE GALAXY IS DIVIDED INTO 64 QUADRANTS ARRANGED ON AN
8 BY 8 GRID, EACH QUADRANT IS SUBDIVIDED INTO 64 SECTORS
WHICH CAN CONTAIN STARS, BASES, KLINGONS AND THE ENTERPRISE.
AS TIME PASSES MESSAGES ARE PRINTED INDICATING BASES UNDER
ATTACK, IF KLINGONS ARE PRESENT THEY FIRE AND DRAIN YOUR
SHIELDS. THE KLINGONS MOVE AROUND AND REGENERATE AND HAVE
BEEN KNOWN TO CLOAK. AS COMMANDER OF THE ENTERPRISE YOU
MUST RID THE GALAXY OF KLINGONS AND THEIR BASE.

THIS VERSION OF TTY TREK HAS BEEN ADAPTED FOR THE
CONFINES OF A MORE PRIMITIVE CONTROL SYSTEM THAT DOES
NOT PERMIT ALPHABETIC ENTRY, THUS COMMANDS MUST BE SPECIFIED
USING NUMERIC CODES. ENTER COMMAND 0 FOR HELP, ENTERING
0 A SECOND TIME CAUSES TIME TO PASS.

WHEN PROMPTED ENTER A SEED VALUE TO INFLUENCE THE INITIAL
CONDITIONS, THEN ENTER THE DESIRED LEVEL OF DIFFICULTY.

THE FOLLOWING COMMANDS CAN BE GIVEN:

1 - MOVE. MOVEMENT IS SPECIFIED AS DIR,WARP WHERE DIR IS A
DIRECTION FROM 1-9 AND WARP IS THE NUMBER OF QUADRANTS TO
TRAVERSE. SPECIFY FRACTIONAL WARP TO MOVE WITHIN A QUADRANT.
DIRECTIONS ARE AS FOLLOWS...

  4   3   2
    \ | /
  5--'O'--1
    / | \
  6   7   8

2 - PHASORS. ENTER HOW MUCH ENERGY TO FIRE AT ENEMY SHIPS.
3 - TORPEDO. ENTER A DIRECTION NUMBER TO FIRE A TORPEDO.
4 - ENERGY ALLOCATE. ENTER HOW MUCH TO DIVERT TO SHIELDS.

5 - LONG RANGE SCAN OF SURROUNDING QUADRANTS. DIGITS INDICATE
THE NUMBER OF ENEMY SHIPS, B FOR STAR BASE, E FOR ENEMY BASE.
EMPTY SPACE EXCEPT FOR STARS IS INDICATED BY ..
SINGLE PERIODS INDICATE THE EDGE OF THE GALAXY.

6 - SHORT RANGE SCAN OF CURRENT QUADRANT. THE + SYMBOLS ARE
STARS, 'O' IS THE ENTERPRISE, /^\ ARE KLINGONS, >!< IS A BASE
WHICH YOU CAN DOCK WITH TO REPLENISH ENERGY AND TORPEDOES.
THE ENEMY BASE IS INDICATED BY A BLOCK OF *** SYMBOLS.
PERIODS INDICATE EMPTY SPACE.

7 - REPORT OF VITAL STATS AND STUFF.
8 - TOGGLES AUTOSCAN OFF AND ON. WHEN ON, A SHORT-RANGE SCAN
IS AUTOMATICALLY PERFORMED AFTER MOVING. DEFAULT IS ON.
9 - GALAXY RECORD, SAME FORMAT AS LONG RANGE SCAN.

INITIALLY YOU HAVE 1000 UNITS OF ENERGY AND 10 TORPEDOS, DOCK
WITH A STARBASE (BY COLLIDING WITH IT) FOR 2000 MORE UNITS OF
ENERGY AND 5 MORE TORPEDOS. HINTS... WHEN SPECIFYING DIRECTION
USE FP NUMBERS FOR FRACTIONAL DIRECTIONS, FOR EXAMPLE 2 CLICKS
TO THE RIGHT AND 4 CLICKS UP IS A DIRECTION OF 2.5 AND 3 CLICKS
TO THE LEFT AND 1 CLICK DOWN IS A DIRECTION OF 5.33. FOR WARP
SPECIFY .125 FOR EACH SECTOR DISTANCE, REMEMBER THAT DIAGONALS
ARE LONGER, 1 QUADRANT LEFT AND 1 UP IS A DISTANCE OF 1.414.

ENTER SEED ?11
DIFFICULTY FACTOR(1-10) ?5
KINGON ATTACKING BASE AT 1,4
 .  .  .  .  .  .  .  +
 .  .  .  .  .  .  .  .
 .  + 'O' .  .  .  +  .  HULL TEMP  55
 .  .  .  .  .  .  .  .  ENERGY     1000
 .  .  .  .  .  .  .  .  PHOTONS    10
 .  .  +  .  .  .  +  .  SHIELDS    0
 .  .  .  .  .  .  .  .  QUADRANT   5,5
 +  .  .  .  .  .  .  .
?5
.. .. ..
.. .. ..
.. .. ..
?9
E3 .3 .. .. .. .. .. ..
.3 .3 .. .. .. .. B. ..
.. .. .. .. .. .. .. ..
B1 B. .. .. .. .. .. ..
.. .. .. ..>..<.. .. B.
.. .. .. .. .. .. .. ..
.. .. .. .. .. .. .. ..
.. B. .. .. B. .. B. ..
?


WUMPUS
======

A conversion of the WUMPUS program from "5-5-84ICL" account A000.

WUMPUSPT.txt - source for normal conversion
WUMPUS8K.txt - trimmed-down version
WUMPUSOV.txt - with overlay calls for alphabetic input
wumpusov.abs - 32KW+ binary with WUMPUSOV.txt source
wumpus8k.abs - 8KW+ binary with WUMPUS8K.txt source

The OV version...

ENTER SEED NUMBER?11
INSTRUCTIONS? (Y/N) Y

WELCOME TO 'HUNT THE WUMPUS'
  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM
HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A
DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW
WHAT A DODECAHEDRON IS, ASK SOMEONE)

    HAZARDS:
 BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM
    IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)
 SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU
    GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER
    ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)

    WUMPUS:
 THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER
 FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY
 HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING
 HIS ROOM OR YOUR SHOOTING AN ARROW.
    IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM
 OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU
 ARE, HE EATS YOU UP (& YOU LOSE!)

    YOU:
 EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW
  MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)
  ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT.
  EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING
  THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.
  IF THE ARROW CAN'T GO THAT WAY(IE NO TUNNEL) IT MOVES
  AT RANDOM TO THE NEXT ROOM.
     IF THE ARROW HITS THE WUMPUS, YOU WIN.
     IF THE ARROW HITS YOU, YOU LOSE.

    WARNINGS:
     WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,
    THE COMPUTER SAYS:
 WUMPUS-  'I SMELL A WUMPUS'
 BAT   -  'BATS NEARBY'
 PIT   -  'I FEEL A DRAFT'

HUNT THE WUMPUS

I FEEL A DRAFT
YOU ARE IN ROOM  2
TUNNELS LEAD TO  1     3     10

SHOOT OR MOVE? (S/M) M
WHERE TO?3
YYYIIIIEEEE . . . FELL IN PIT
HA HA HA - YOU LOSE!
SAME SET-UP? (Y/N) 


-----------------------------------------------------------------------
Terry Newton (wtn90125@yahoo.com)
Last mod 12/21/2010

