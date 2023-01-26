package org.riverporpoise.tee

import org.riverporpoise.text.CharacterGrid
import collection.mutable.ListBuffer

/**
 * A two-dimensional plane populated by tees, holes or spaces.
 *
 * @author dhorlick
 */
class TeeGrid(private var initialRowCount : Int = 8,
			  private var initialIndicesWithinRowCount : Int = 8)
		extends Grid[TeeGridElement.TeeGridElement] (
			initialRowCount = initialRowCount,
			initialIndicesWithinRowCount = initialIndicesWithinRowCount,
			filler = TeeGridElement.space)
{
	def toCharacterGrid =
	{
		val characterGrid = new CharacterGrid(initialRowCount, initialIndicesWithinRowCount)

		zipWithIndex.foreach
		{
			rowAndRowIndex =>

				val row = rowAndRowIndex._1
				val rowIndex = rowAndRowIndex._2

				row.zipWithIndex.foreach
				{
					myElementAndIndexWithinRow =>

						val myElement = myElementAndIndexWithinRow._1
						val indexWithinRow = myElementAndIndexWithinRow._2
						
						characterGrid.set(rowIndex, indexWithinRow, myElement.toString.charAt(0))
				}
		}

		characterGrid
	}
}
