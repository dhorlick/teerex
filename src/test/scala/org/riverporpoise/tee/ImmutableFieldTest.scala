package org.riverporpoise.tee

import org.junit._
import Assert._

/**
  * @author dhorlick
  */
@Test
class ImmutableFieldTest
{
	@Test
	def test() : Unit =
	{
		var field = new ImmutableField(5)
		println (field.toString())

		val bigInt1 = field.bigInt()
		assertEquals(15, field.places.size)
		assertEquals(None, field.places(0))
		assertEquals(14, field.pegCount)
		assertEquals(State.Indeterminate, field.state())

		field.arbitraryGoodMove
		val teeGrid = field.toTeeGrid()
		assertNotNull(teeGrid)
		val translated = teeGrid.translate(new Path(1, 0))
		assertNotNull(translated)
		assertNotSame(teeGrid.iterator.next(), translated.iterator.next())
	}
}