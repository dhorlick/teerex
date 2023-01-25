package org.riverporpoise.tee

/**
 * A direction on the game board.
 *
 * For the sake of recognizability, these are named for directions on a clock face, although in truth the correlation is
 * somewhat forced.
 *
 * @param vector the direction expressed as a zero-ordered slope (rise over run)
 *
 * @author dhorlick
 */
case class Direction(oClock : Int, vector : (Int, Int))  // TODO change oClock to a subset of the "cardinal" directions
{
	def *(distance : Int) : Path =
	{
		new Path(distance * vector._1, distance * vector._2)
	}

	override def toString =
	{
		oClock + " o'clock"
	}
}

object Direction
{
	val oneOClock = new Direction(1, (-1,1))
	val threeOClock = new Direction(3, (0,2))
	val fiveOClock = new Direction(5, (1,1))
	val sevenOClock = new Direction(7, (1,-1))
	val nineOClock = new Direction(9, (0,-2))
	val elevenOClock = new Direction(11, (-1,-1))

	def values =
	{
		List(oneOClock, threeOClock, fiveOClock, sevenOClock, nineOClock, elevenOClock)
	}
}