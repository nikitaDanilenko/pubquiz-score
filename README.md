# pubquiz-score
This is a small Haskell program that generates pages to visualise the score for teams participating in a pub quiz.

# Setup

The program uses standard Haskell without additional libraries and can be compiled directly.
There are scripts provided for Linux (`install.sh`) and Windows (`install.bat`), 
both of which create an executable called "pubquizScore" in the project directory.

# Usage

You can run the program without any parameters, but three parameters are implicit:

1. The file `colors.txt` contains the colors that will be used for the individual groups.
   You can add additional colors in the RGB format.
   The colors are cycled, i.e. you do not need to worry about supplying a sufficient number,
   but this also means that colors might be reused.

1. The file `labels.txt` contains the names for the various parts of the quiz,
   including the quiz title.
   This file can be modified to change the names as you desire, 
   but the overall structure needs to remain the same (in particular there needs to be exactly one line).

1. The file `rounds.txt` contains the points for the individual rounds, 
   as well as the codes which will be used for the groups.
   The structure of the file is as follows:

        code1 ... codeN
        1 : p11 ... p1G
        ...
        L : pL1 ... pLG
   
   The codes are arbitrary alphanumerical sequences that should be pairwise different.
   The round ratings are written as the round number followed by any separator symbol (e.g. ":")
   and then all the points of a round.

After updating one of the files (usually `rounds.txt`) 
all pages are regenerated according to the new settings.

