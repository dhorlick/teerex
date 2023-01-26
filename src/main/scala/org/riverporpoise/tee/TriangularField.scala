package org.riverporpoise.tee

import collection.mutable.ListBuffer
import results.{MoveSpace, SolutionSpace}
import text.WrappingTileSpace
import org.riverporpoise.text.CharacterGrid

/**
 * The game board is typically 5 spaces "wide", but this model supports variable width.
 * <pre>
 *      o
 *     * *
 *    * * *
 *   * * * *
 *  * * * * *
 * </pre>
 */
class TriangularField(override val width : Int = Field.defaultWidth) extends Field(width)
{
	def numberOfPlaces() : Int =
	{
		TriangularField.numberOfPlaces(width)
	}

    def buildTeeGrid(expandToFit : Boolean = false) : TeeGrid =
	{
		val seq = new ListBuffer[Option[Peg]]
		seq.append(None)
		
		for (i <- 2 to numberOfPlaces)
		{
			seq.append(new Some(Field.peg))
		}
		
		val teeGrid = new TeeGrid(width, (width * 2)-1)

		var i = 0
		
		for (loopRowIndex <- 1 to width)
		{
			for (loopIndexWithinRow <- 1 to loopRowIndex)
			{
				teeGrid.set(loopRowIndex-1, (width - loopRowIndex) + ((loopIndexWithinRow-1) * 2),
					if (seq(i)==None) TeeGridElement.hole else TeeGridElement.tee)
				i = i + 1
			}
		}

		require(teeGrid.rowCount==width, "tee grid's height should equals the field's width… " +teeGrid.rowCount+"!="+width)

		val oneHalf = BigDecimal(1)/BigDecimal(2)

		teeGrid
	}
}

object TriangularField
{
    def numberOfPlaces(width : Int) : Int =
	{
		width * (width + 1) / 2
	}

	def widthFromNumberOfPlaces(numberOfPlaces : Int) : Int =
	{
		/**    x ( x + 1 )
		 * y = -----------
		 *          2
		 *
		 * Asked Wolfram Alpha to solve 2y = x ( x + 1 ) for x
		 *      _______
		 *     √ 8y + 1 - 1
		 * x = ------------
		 *          2
		 */

		val result = ( math.pow ( (8 * numberOfPlaces + 1) , 0.5 ) - 1) / 2
		var rounded = math.round(result).asInstanceOf[Int] : Int

		if (rounded==result)
			rounded
		else
			throw new IllegalArgumentException("Invalid number of places: "+numberOfPlaces)
	}

	def main(args: Array[String]) =
	{
		new TriangularField().solveThenDocument()
	}
}