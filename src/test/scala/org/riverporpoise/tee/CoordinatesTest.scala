package org.riverporpoise.tee

import org.junit._
import Assert._

/**
 * @author dhorlick
 */

@Test
class CoordinatesTest
{
	@Test
	def test() =
	{
		val coord = new Coordinates(1,2)
		assertEquals(coord, coord)
		assertNotSame(coord, new Coordinates(1,3))
		assertEquals(0, new Coordinates(1, 1).toZeroOrderedIndex())
		assertEquals(5, new Coordinates(3, 3).toZeroOrderedIndex())
		assertTrue(new Coordinates(2,2).valid)
		assertFalse(new Coordinates(2,3).valid)
	}
}