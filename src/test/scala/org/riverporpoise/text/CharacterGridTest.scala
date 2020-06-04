package org.riverporpoise.text

import org.junit._
import Assert._

/**
  * @author dhorlick
  */
@Test
class CharacterGridTest
{
	@Test
	def test = 
	{
		assertEquals(0, CharacterGrid.maxLength(Array("")))
		assertEquals(5, CharacterGrid.maxLength(Array("one", "seven", "fish")))

		val string = "The\nquick\nbrown fox\njumps\nover the\nlazy dog."
		assertEquals(9, CharacterGrid.maxLength(string.split('\n')))
		val cg = new CharacterGrid(string)
		assertNotNull(cg)
		assertEquals(6, cg.rowCount)
		
		assertNotNull(cg.toString())
		assertTrue(new CharacterGrid(0, 0).empty)
	}
}