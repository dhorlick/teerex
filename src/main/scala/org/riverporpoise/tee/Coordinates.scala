package org.riverporpoise.tee

import org.riverporpoise.tee.ProjectableInt._

/**
 * Identifies a particular position on the game board. Each position can be occupied by a peg, a hold, or a void.
 *
 * By long-standing alebit somewhat inconvenient tradition, coordinate indices count up from one (i.e. are "one-ordered".)
 *
 * @author dhorlick
 */
class Coordinates(val rowIndex : Int, val indexWithinRow : Int)
{
	/**
	 * Converts the coordinates as a unique-ish integer.
	 */
	def toZeroOrderedIndex() : Int =
	{
		var tally = 0

		var loop = 1

		while (loop <= rowIndex - 1)
		{
			tally += loop;
			loop += 1
		}

		tally += indexWithinRow;

		return tally - 1;
	}
	
	def +(path : Path) =
	{
		new Coordinates(rowIndex + path.rowsDown, indexWithinRow + path.indicesRightWithinRow)
	}

	def -(path : Path) =
	{
		new Coordinates(rowIndex - path.rowsDown, indexWithinRow - path.indicesRightWithinRow)
	}

	def -(coord : Coordinates) =
	{
		new Coordinates(rowIndex - coord.rowIndex, indexWithinRow - coord.indexWithinRow)
	}

	def *(scalar : Int) =
	{
		new Coordinates(scalar * rowIndex, scalar * indexWithinRow)
	}

	override def equals(other: Any) =
	{
		val otherCoord = other.asInstanceOf[Coordinates]
		if (rowIndex == otherCoord.rowIndex && indexWithinRow == otherCoord.indexWithinRow)
			true
		else
			false
	}

	def valid : Boolean =
	{
		if (rowIndex >= 1 && indexWithinRow >= 1 && indexWithinRow <= rowIndex)
			true
		else
			false
	}
	
	override def toString : String =
	{
		"(" + rowIndex + ", " + indexWithinRow + ")"
	}
}
