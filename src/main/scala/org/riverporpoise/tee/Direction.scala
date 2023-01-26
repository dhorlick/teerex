package org.riverporpoise.tee

/**
 * A direction on the game board.
 *
 * For the sake of recognizability, these are named for cardinal directions.
 *
 * @param vector the direction expressed as a zero-ordered slope (rise over run)
 *
 * @author dhorlick
 */
case class Direction(acronym : String, vector : (Int, Int))
{
	def *(distance : Int) : Path =
	{
		new Path(distance * vector._1, distance * vector._2)
	}

	override def toString =
	{
		acronym
	}
}

object Direction
{
	val NORTHEAST = new Direction("NE", (-1,1))
	val EAST = new Direction("E", (0,2))
	val SOUTHEAST = new Direction("SE", (1,1))
	val SOUTHWEST = new Direction("SW", (1,-1))
	val WEST = new Direction("W", (0,-2))
	val NORTHWEST = new Direction("NW", (-1,-1))

	def values =
	{
		List(NORTHEAST, EAST, SOUTHEAST, SOUTHWEST, WEST, NORTHWEST)
	}
}