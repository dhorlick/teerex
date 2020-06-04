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

		assertEquals(new Coordinates(1, 1), Coordinates.coordinatize(0))
		assertEquals(new Coordinates(2, 1), Coordinates.coordinatize(1))
		assertEquals(new Coordinates(2, 2), Coordinates.coordinatize(2))
		assertEquals(new Coordinates(3, 1), Coordinates.coordinatize(3))
		assertEquals(new Coordinates(3, 2), Coordinates.coordinatize(4))
		assertEquals(new Coordinates(3, 3), Coordinates.coordinatize(5))
		assertEquals(new Coordinates(4, 1), Coordinates.coordinatize(6))
		assertEquals(new Coordinates(5, 5), Coordinates.coordinatize(14))
		assertEquals(new Coordinates(6, 1), Coordinates.coordinatize(15))
	}
}