package org.riverporpoise.tee

/**
 * A wrapper that permits Ints to be used in vector algebra with {@link Direction} and {@link Coordinates}.
 *
 * @author dhorlick
 */
class ProjectableInt(val distance : Int)
{
	def *(direction : Direction) : Path =
	{
		direction * distance
	}

	def *(coord : Coordinates) : Coordinates =
	{
		coord * distance
	}
}

object ProjectableInt
{
	implicit def int2ProjectableInt(distance : Int) = new ProjectableInt(distance)
}