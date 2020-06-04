package org.riverporpoise.tee

/**
 * A direction on the game board.
 *
 * For the sake of recognizability, these are named for directions on a clock face, although in truth the correlation is
 * somewhat forced.
 *
 * @author dhorlick
 */
case class Direction(oClock : Int, vector : (Int, Int))
{
	def *(distance : Int) : Path =
	{
		new Path(distance * vector._1, distance * vector._2)
	}
}

object Direction
{
	val oneOClock = new Direction(1, (-1,0))
	val threeOClock = new Direction(3, (0,1))
	val fiveOClock = new Direction(5, (1,1))
	val sevenOClock = new Direction(7, (1,0))
	val nineOClock = new Direction(9, (0,-1))
	val elevenOClock = new Direction(11, (-1,-1))

	def values =
	{
		List(oneOClock, threeOClock, fiveOClock, sevenOClock, nineOClock, elevenOClock)
	}
}