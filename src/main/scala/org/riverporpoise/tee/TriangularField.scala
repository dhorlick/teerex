package org.riverporpoise.tee

import org.riverporpoise.geometry.DecimalCoordinates
import org.riverporpoise.geometry.Polygon
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

		val triangle = new Polygon(
			List(
				new DecimalCoordinates(BigDecimal((width * 2)-1) / BigDecimal(2), - oneHalf),
				new DecimalCoordinates(-1, BigDecimal(width)),
				new DecimalCoordinates((width * 2)-1+1, BigDecimal(width))
			)
		)
		teeGrid.add(triangle, expandToFit)

		teeGrid
	}

	def performAndRecord(moves : Seq[Move]) : WrappingTileSpace =
	{
		val tileSpace = new WrappingTileSpace()
		tileSpace.add(new CharacterGrid(this))

		moves.foreach
		{
			move =>

				perform(move)
				val pane = new CharacterGrid(this)
				pane.addBelow(move.toString)
				
				tileSpace.add(pane)
		}

		tileSpace
	}

	def unperform(moves : Seq[Move]) : Unit =
	{
		moves.reverse.foreach
		{
			move =>

				unperform(move)
		}
	}

	override def solutionSpace : SolutionSpace =
	{
		new SolutionSpace(false)
	}

	def arbitraryGoodMove : Option[Move] =
	{
		arbitraryGoodMove(new ListBuffer[BigInt], true)
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
		val mutableField = new TriangularField()
		val solutionSpace = mutableField.solve()
		println("Results: ")
		println(solutionSpace.toString())
		println()

		solutionSpace.bestWins.zipWithIndex.foreach
		{
			gameSpaceAndIndex =>

				val gameSpace = gameSpaceAndIndex._1
				val i = gameSpaceAndIndex._2

				print("Unique Solution #")
				println(i+1)
				println
				val moves = gameSpace.inventoryMoves
				println(mutableField.performAndRecord(moves))
				mutableField.unperform(moves)

				println()
				
				if (i<solutionSpace.bestWins.length-1)
					println()
		}
	}
}