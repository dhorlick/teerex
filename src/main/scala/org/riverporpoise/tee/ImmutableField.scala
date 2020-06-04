package org.riverporpoise.tee

import org.riverporpoise.text.CharacterGrid
import results.{SolutionSpace, MoveSpace}
import text.WrappingTileSpace
import collection.mutable.ListBuffer


/**
 * An implementation of Field that cannot be changed. Moves produce new Field instances.
 *
 * Immutability facilitates concurrency.
 *
 * @author dhorlick
 */
class ImmutableField(val seq : Seq[Option[Peg]]) extends Field(Field.widthFromNumberOfPlaces(seq.size))
{
	def this(width : Int = Field.defaultWidth) =
	{
		this(List(None) ++ List.fill(Field.numberOfPlaces(width)-1)(new Some(Field.peg)))
	}

	override def places : Seq[Option[Peg]] =
	{
		seq
	}

	override def resolve(index : Int) : Option[Peg] =
	{
		seq(index)
	}

	def perform(move : Move) : ImmutableField =
	{
		val startingPlace = resolve(move.startingPlaceIndex)
		require(startingPlace!=None)
		val destinationCoord = Field.project(move.startingPlaceIndex, move.direction, 2)
		val destinationIndex = destinationCoord.toZeroOrderedIndex()
		require(resolve(destinationIndex)==None, "Destination: "+destinationCoord+" is already occupied by: "+resolve(destinationIndex))
		val betweenCoord = Field.project(move.startingPlaceIndex, move.direction, 1)
		val betweenIndex = Field.indexfy(betweenCoord)

		require(!betweenIndex.equals(destinationIndex))
		require(!betweenIndex.equals(move.startingPlaceIndex))

		if(resolve(betweenIndex)==None)
			throw new UnoccupiedJumpSpaceException(betweenCoord)

		new ImmutableField(places.zipWithIndex.map
		{
			myTuple : (Option[Peg], Int) =>

				myTuple._2 match
				{
					case `destinationIndex` => startingPlace
					case `betweenIndex` => None
					case move.startingPlaceIndex => None
					case _ => myTuple._1
				}
		})
	}

	def unperform(move : Move) : ImmutableField =
	{
		require(resolve(move.startingPlaceIndex) == None)
		val betweenCoord = Field.project(move.startingPlaceIndex, move.direction, 1)
		val betweenIndex = Field.indexfy(betweenCoord)
		require(resolve(betweenIndex)==None)
		val destinationCoord = Field.project(move.startingPlaceIndex, move.direction, 2)
		val destinationIndex = destinationCoord.toZeroOrderedIndex()

		val resolvedDestinationPlace = resolve(destinationIndex)
		require(resolvedDestinationPlace!=None)

		new ImmutableField(places.zipWithIndex.map
		{
			myTuple : (Option[Peg], Int) =>

				myTuple._2 match
				{
					case `destinationIndex` => None
					case `betweenIndex` => new Some(Field.peg)
					case move.startingPlaceIndex => resolvedDestinationPlace
					case _ => myTuple._1
				}
		})
	}

	def playAll(moveSpace : MoveSpace) : Unit =
	{
		val solutionSpace = moveSpace.solutionSpace
		val moves = availableMoves
		
		Field.log("availableMoves.size = "+availableMoves.size)

		if (moves.length==0)
			moveSpace.prune()
		else
		{
			moves.par.foreach
			{
				move =>

					val result = perform(move)
					if (solutionSpace.registerFieldState(result))
					{
						Field.logTimelessly(this)

						Field.log("Performed: "+move)

						val subGameSpace = moveSpace.add(move, result.state())
						// we could pinch a few bytes by not recording nodes associated with losses,
						// but the resulting increase in modeling complexity probably isn't worth it.

						Field.log(subGameSpace.outcome)

						subGameSpace.outcome match
						{
							case State.Won =>
							case State.Lost =>
							case State.Indeterminate =>
								result.playAll(subGameSpace)

							case _ => throw new UnsupportedOperationException("No support for outcome: "+subGameSpace.outcome)
						}

					}

					// No need to unperform moves… just discard the corresponding immutable fields.
			}
		}
	}

	def arbitraryGoodMove : Option[Move] =
	{
		arbitraryGoodMove(new ListBuffer[BigInt], false)
	}

	def performAndRecord(moves : Seq[Move]) : WrappingTileSpace =
	{
		val tileSpace = new WrappingTileSpace()
		tileSpace.add(new CharacterGrid(this))
		
		var current : Field = this

		moves.foreach
		{
			move =>

				current = current.perform(move)
				val pane = new CharacterGrid(current)
				pane.addBelow(move.toString)
				tileSpace.add(pane)
		}

		tileSpace
	}

	override def solutionSpace : SolutionSpace =
	{
		new SolutionSpace(true)
	}
}

object ImmutableField
{
	def fromBigInt(bigInt : BigInt) : ImmutableField =
	{
		val bitString = bigInt.toString(2).substring(1)
		new ImmutableField(bitString.map
		{
			myChar : Char =>

				myChar match
				{
					case '0' => None
					case '1' => new Some(Field.peg)
					case _ => throw new IllegalStateException("non-binary digit="+myChar)
				}
		})
	}

	def main(args: Array[String]) =
	{
		val immutableField = new ImmutableField()
		val solutionSpace = immutableField.solve()
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
				println(immutableField.performAndRecord(gameSpace.inventoryMoves))

				println()

				if (i<solutionSpace.bestWins.length-1)
					println()
		}
	}
}