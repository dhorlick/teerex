package org.riverporpoise.tee

import org.junit._
import Assert._

@Test
class MutableFieldTest {

    @Test
    def test() =
	{
		var field = new MutableField(5)
		println (field.toString())

		val bigInt1 = field.bigInt()
		assertEquals(15, field.places.size)
		assertEquals(None, field.places(0))
		assertEquals(14, field.pegCount)
		assertEquals(State.Indeterminate, field.state())
		val firstMove = new Move(direction = Direction.elevenOClock, startingPlaceIndex = 5)
		field.perform(firstMove)
		val bigInt2 = field.bigInt()
		assertNotSame(bigInt1, bigInt2)
		assertEquals(State.Indeterminate, field.state())
		assertEquals(13, field.pegCount)
		field.unperform(firstMove)
		val bigInt3 = field.bigInt()
		assertEquals(bigInt1, bigInt3)
		assertEquals(14, field.pegCount)
		assertEquals(State.Indeterminate, field.state())
		
		val solutionSpace = field.solve()
		assertNotNull(solutionSpace)
		assertNotSame(0, solutionSpace.winsByDepth.size)
	}
}


