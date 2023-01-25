package org.riverporpoise.tee

import org.junit._
import Assert._

/**
 * @author dhorlick
 */
@Test
class MoveTest
{
	@Test
	def test() =
	{
		val move = new Move(Direction.fiveOClock, (5,5))
		require(move.direction == Direction.fiveOClock)
	}
}