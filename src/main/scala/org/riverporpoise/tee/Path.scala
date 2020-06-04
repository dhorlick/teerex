package org.riverporpoise.tee

/**
 * A two-dimensional vector of direction and quantity on the game board.
 *
 * Can be used to represent the line segment connecting to spaces on the game board.
 *
 * @author dhorlick
 */
class Path(val rowsDown : Int, val indicesRightWithinRow : Int)
{
	def +(coord : Coordinates) : Coordinates =
	{
		new Coordinates(rowsDown + coord.rowIndex, indicesRightWithinRow + coord.indexWithinRow)
	}
}