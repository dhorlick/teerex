package org.riverporpoise.tee

import org.junit._
import Assert._

/**
  * @author dhorlick
  */
class FieldTest
{
	@Test
	def test() =
	{
		assertEquals(1, Field.numberOfPlaces(1))
		assertEquals(3, Field.numberOfPlaces(2))
		assertEquals(15, Field.numberOfPlaces(5))
		assertEquals(1, Field.widthFromNumberOfPlaces(1))
		assertEquals(2, Field.widthFromNumberOfPlaces(3))
		assertEquals(5, Field.widthFromNumberOfPlaces(15))
	}
}