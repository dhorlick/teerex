package org.riverporpoise.tee

/**
 * A legal state change to the game board. Consists of a peg jumping another peg, in a straight line, to land in an
 * unoccupied hole.
 *
 * @author dhorlick
 */
class Move(val direction : Direction, val startingPlaceIndex : Int)
{
	@throws(classOf[InvalidJumpException])
	def this(startingPlaceIndex : Int, endingPlaceIndex : Int) =
	{
		this(Move.determineJumpDirection(startingPlaceIndex, endingPlaceIndex), startingPlaceIndex);
	}

	override def toString =
	{
		Coordinates.coordinatize(startingPlaceIndex) + "->" + direction.oClock
	}
}

object Move
{
	@throws(classOf[InvalidJumpException])
	def determineJumpDirection(startingPlaceIndex : Int, endingPlaceIndex : Int) : Direction =
	{
		val startCoords = Coordinates.coordinatize(endingPlaceIndex)
		val endCoords = Coordinates.coordinatize(startingPlaceIndex)
		val vector : Coordinates = startCoords - endCoords
		Direction.values.foreach
		{
			direction =>

				if (direction.vector._1 * 2 == vector.rowIndex && direction.vector._2 * 2 == vector.indexWithinRow)
				{
					return direction
				}
		}

		throw new InvalidJumpException(startCoords, endCoords)
	}
}