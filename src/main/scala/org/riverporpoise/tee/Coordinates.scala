package org.riverporpoise.tee

import org.riverporpoise.tee.ProjectableInt._

/**
 * Identifies a particular position on the game board. Each position can hold a peg or remain empty, in which case
 * it is known as a "hole".
 *
 * @author dhorlick
 */
class Coordinates(val rowIndex : Int, val indexWithinRow : Int)
{
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

object Coordinates
{
	def coordinatize(targetPlaceIndex : Int) : Coordinates
	=
	{
		if (targetPlaceIndex < 0)
		{
			/**
			 *  Not legal for the purposes of the game, but it will simplify the type arithemtic if we
			 *  allow it in principle.
			 */
			return -1 * Coordinates.coordinatize(-1 * targetPlaceIndex)
		}

		var tally, rowLoop = 0

		do
		{
			tally += rowLoop
			rowLoop += 1
		}
		while ((targetPlaceIndex + 1 > tally+rowLoop))

		new Coordinates(rowLoop, targetPlaceIndex + 1 - tally)
	}
}