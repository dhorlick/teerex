package org.riverporpoise.tee

import collection.mutable.ListBuffer
import ProjectableInt._
import compat.Platform
import results.{MoveSpace, SolutionSpace}
import text.WrappingTileSpace
import org.riverporpoise.geometry.{DecimalCoordinates, Polygon}

/**
 * Models the game board for that dinnertime triangle game with all the tees (called "pegs" in the code).
 * Alternating rows are offset by one from their predecessors.
 *
 * You drag and drop the tees, jumping a neighbor to land in an empty space. The goal is to have one tee remaining.
 *
 * The game starts with all but one hole filled with a tee.
 *
 * The game board is typically 5 spaces "wide", but this model supports variable width.
 * <pre>
 *      o
 *     * *
 *    * * *
 *   * * * *
 *  * * * * *
 * </pre>
 * Please note that not all widths may result in a winnable game, and that widths greater than 6 take a long time
 * to exhaustively solve on 2012 hardware.
 *
 * @author dhorlick
 */
abstract class Field(val width : Int)
{
	def places : Seq[Option[Peg]]
	def perform(move : Move) : Field
	def unperform(move : Move) : Field
	def resolve(index : Int) : Option[Peg]
	def playAll(moveSpace : MoveSpace) : Unit
	def solutionSpace : SolutionSpace
	def arbitraryGoodMove : Option[Move]

	def numberOfPlaces() : Int =
	{
		Field.numberOfPlaces(width)
	}

	def availableMoves : Seq[Move] =
	{
		val availableMoves = new ListBuffer[Move] ()

		places.zipWithIndex.foreach
		{
			case (place, placeIndex) =>

				if (place != None)
				{
					for (direction <- Direction.values)
					{
						val oneAway: Coordinates = Field.project(placeIndex = placeIndex, direction = direction, distance = 1)
						val twoAway: Coordinates = Field.project(placeIndex = placeIndex, direction = direction, distance = 2)

						if (valid(oneAway) && valid(twoAway)
							&& resolve(oneAway) != None && resolve(twoAway) == None)
						{
							availableMoves.append(new Move(direction = direction, startingPlaceIndex = placeIndex))
						}
					}
				}
		}

		availableMoves
	}

	def state() : State =
	{
	 	if (pegCount==1)
		 	return State.Won
		else if (availableMoves.size==0)
			return State.Lost
		else
			return State.Indeterminate
	}
	
	def pegCount : Int =
	{
	    countWhatever((place:Option[Peg]) ⇒ place != None)
	}

	def holeCount : Int =
	{
		countWhatever((place:Option[Peg]) ⇒ place == None)
	}

	private def countWhatever(f: (Option[Peg]) ⇒ Boolean) =
	{
		var count = 0

		places.foreach
		{
			place : Option[Peg] =>

				if (f(place))
					count += 1
		}

		count
	}

	def resolve(coords : Coordinates) : Option[Peg] =
	{
		require(valid(coords), "Invalid coordinate.")
		resolve(coords.toZeroOrderedIndex())
	}

	def valid(coords : Coordinates) : Boolean =
	{
		if (!coords.valid)
			false
		else if (coords.rowIndex > width || coords.indexWithinRow > width)
			false
		else
			true
	}

	def performAndRecord(moves : Seq[Move]) : WrappingTileSpace

	def solve() : SolutionSpace =
	{
		val startTime = Platform.currentTime
		val mySolutionSpace = solutionSpace

		playAll(mySolutionSpace)
		val durationInMillis = Platform.currentTime - startTime
		println("Took "+(durationInMillis/1000L)+"."+(durationInMillis % 1000L)+" seconds to solve.")
		
		mySolutionSpace
	}

	/**
	  * Uniquely represents the current peg arrangement as a BigInt.
	  */
	def bigInt() : BigInt =
	{
		val desc = new StringBuilder(1+places.size);
		desc.append("1");
		// since we don't want to lose information from leading zero digits

		places.foreach
		{
			place =>

				if (place==None)
					desc.append("0")
				else
					desc.append("1")
		}

		BigInt(desc.toString(), 2)
	}

	override def hashCode() : Int =
	{
		bigInt.intValue
	}

	def toTeeGrid(expandToFit : Boolean = false) : TeeGrid =
	{
		val teeGrid = new TeeGrid(width, (width * 2)-1)

		var i = 0
		
		for (loopRowIndex <- 1 to width)
		{
			for (loopIndexWithinRow <- 1 to loopRowIndex)
			{
				teeGrid.set(loopRowIndex-1, (width - loopRowIndex) + ((loopIndexWithinRow-1) * 2),
					if (places(i)==None) TeeGrid.Element.hole else TeeGrid.Element.tee)
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

	protected def arbitraryGoodMove(fieldStatesInvestigated : ListBuffer[BigInt], undoAfterwards : Boolean) : Option[Move] =
	{
		val moves = availableMoves

		Field.log("availableMoves.size = "+availableMoves.size)

		if (moves.length==0)
			return None;
		else
		{
			moves.foreach
			{
				move =>

					try
					{
						val resultField : Field = perform(move)
						val outcome = resultField.state()

						if (outcome==State.Won)
						{
							return new Some(move)
						}
						else
						{
							val integerized = resultField.bigInt()
							if (!fieldStatesInvestigated.contains(integerized))
							{
								Field.logTimelessly(this)

								Field.log("Performed: "+move)

								fieldStatesInvestigated.append(integerized)
							}

							if (outcome==State.Indeterminate)
							{
								if (resultField.arbitraryGoodMove(fieldStatesInvestigated, undoAfterwards)!=None)
									return new Some(move)
							}
						}
					}
					finally
					{
						if (undoAfterwards)
						{
							unperform(move)
						}
					}
			}

			return None
		}
	}

	override def toString() =
	{
		toTeeGrid().toCharacterGrid.toString
	}
}

object Field
{
	val peg = new Peg()
	val defaultWidth : Int = 5

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

	/**
	 * The coordinate distance spatial units away from the coordinate corresponding to placeIndex in the
	 * provided direction.
	 */
	def project(placeIndex : Int, direction : Direction, distance : Int) : Coordinates =
	{
		val coords = Coordinates.coordinatize(placeIndex)
		coords + distance * direction
	}

	def indexfy(coord : Coordinates) : Int =
	{
		coord.toZeroOrderedIndex()
	}

	def log(message : Any) =
	{
		if (MutableField.logging)
		{
			print(System.currentTimeMillis())
			print(": ")
			println(message)
		}
	}

	def logTimelessly(message : Any)
	{
		if (MutableField.logging)
		{
			println(message)
		}
	}
}