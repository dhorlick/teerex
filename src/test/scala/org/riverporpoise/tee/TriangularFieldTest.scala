package org.riverporpoise.tee

import org.junit._
import Assert._

class TriangularFieldTest
{
	@Test
    def testObject() =
    {
        assertEquals(1, TriangularField.numberOfPlaces(1))
		assertEquals(3, TriangularField.numberOfPlaces(2))
		assertEquals(15, TriangularField.numberOfPlaces(5))
		assertEquals(1, TriangularField.widthFromNumberOfPlaces(1))
		assertEquals(2, TriangularField.widthFromNumberOfPlaces(3))
		assertEquals(5, TriangularField.widthFromNumberOfPlaces(15))
    }

	@Test
	def testMove() =
	{
		var field = new TriangularField(5)
		println (field.toString())

		val bigInt1 = field.bigInt()
		assertEquals(14, field.pegCount)
		assertEquals(State.Indeterminate, field.state())

		field.arbitraryGoodMove
		val teeGrid = field.buildTeeGrid()
		assertNotNull(teeGrid)
		val translated = teeGrid.translate(new Path(1, 0))
		assertNotNull(translated)
		assertNotSame(teeGrid.iterator.next(), translated.iterator.next())
	}

    @Test
    def testPerform() =
	{
		var field = new TriangularField(5)
		println (field.toString())

		val bigInt1 = field.bigInt()
		assertEquals(14, field.pegCount)
		assertEquals(State.Indeterminate, field.state())
		val firstMove = new Move(direction = Direction.NORTHEAST, (2,2))
		field.perform(firstMove)
		val bigInt2 = field.bigInt()
		assertNotSame(bigInt1, bigInt2)
		assertEquals(State.Indeterminate, field.state())
		assertEquals(field + "", 13, field.pegCount)
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