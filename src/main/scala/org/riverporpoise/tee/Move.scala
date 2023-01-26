package org.riverporpoise.tee

/**
 * A legal state change to the game board. Consists of a peg jumping another peg, in a straight line, to land in an
 * unoccupied hole.
 * 
 * @param startingCoordinates an integer tuple of row, index-within-row. zero-ordered & non-negative.
 * @author dhorlick
 */
class Move(val direction : Direction, val startingCoords : (Int, Int))
{
	override def toString =
	{
		startingCoords + "->" + direction
	}
}
