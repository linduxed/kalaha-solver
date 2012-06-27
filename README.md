kalaha-solver
=============

A program that calculates the optimal first set of moves in a game of Kalaha.

Game description
----------------

The general idea is that you've got a board with fourteen pots. First a row of six, being owned by one player, then another row of six owned by the other player and in between these rows are two pots called "stores", one belonging to each player. Each of the two rows start filled with a set amount of marbles.  
The objective of the game is to end up with more marbles in your store than your opponent by the end of the game.

A full description of the game can be found on (Wikipedia)[http://en.wikipedia.org/wiki/Kalah].

This program works for the version of Kalah that's the most played in Sweden, which has a major rule change. The difference is that if your last marble in your hand lands in a non-empty pot, you get to pick up the contents of that pot (including that marble you held) and continue the movement.  
Yes, this makes a game that is already horribly favoured to the starting player even worse, but hey, I don't make the rules.

Also, when you land in an empty pot, you don't get the last marble, or the marbles from the opposing pot, the turn just ends.
