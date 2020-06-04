package org.riverporpoise.tee

import collection.mutable.ListBuffer
import results.{SolutionSpace, MoveSpace}
import text.WrappingTileSpace
import org.riverporpoise.text.CharacterGrid

/**
 * An implementation of Field that can be changed. Moves alter instances.
 *
 * Mutable data structures can be useful when memory is scarce.
 *
 * @author dhorlick
 */
class MutableField(width : Int = Field.defaultWidth) extends Field(width)
{
	private val seq = new ListBuffer[Option[Peg]]

	seq.append(None)

	for (i <- 2 to numberOfPlaces)
	{
		seq.append(new Some(Field.peg))
	}

	override def places : Seq[Option[Peg]] =
	{
	    seq.toList
	}

	override def resolve(index : Int) : Option[Peg] =
	{
		seq(index)
	}
	
	override def perform(move : Move) : MutableField =
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

		seq(destinationIndex) = startingPlace
		seq(betweenIndex) = None
		seq(move.startingPlaceIndex) = None

		this
	}

	override def unperform(move : Move) : MutableField =
	{
		require(resolve(move.startingPlaceIndex) == None)
		val betweenCoord = Field.project(move.startingPlaceIndex, move.direction, 1)
		val betweenIndex = Field.indexfy(betweenCoord)
		require(resolve(betweenIndex)==None)
		val destinationCoord = Field.project(move.startingPlaceIndex, move.direction, 2)
		val destinationIndex = destinationCoord.toZeroOrderedIndex()
		
		val resolvedDestinationPlace = resolve(destinationIndex)
		require(resolvedDestinationPlace!=None)
		
		seq(move.startingPlaceIndex) = resolvedDestinationPlace
		seq(betweenIndex) = new Some(Field.peg)
		seq(destinationIndex) = None

		this
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
			moves.foreach
			{
				move =>

					perform(move)
					if (solutionSpace.registerFieldState(this))
					{
						Field.logTimelessly(this)

						Field.log("Performed: "+move)

						val subGameSpace = moveSpace.add(move, state())
							// we could probably pinch a few bytes by not recording node associated with losses,
							// but the resulting increase in modeling complexity probably isn't worth it.

						Field.log(subGameSpace.outcome)

						subGameSpace.outcome match
						{
							case State.Won =>
							case State.Lost =>
							case State.Indeterminate =>
								playAll(subGameSpace)

							case _ => throw new UnsupportedOperationException("No support for outcome: "+subGameSpace.outcome)
						}

					}

					Field.log("unperforming: "+move)
					unperform(move)
			}
		}
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

object MutableField
{
	val logging = false

	def main(args: Array[String]) =
	{
		val mutableField = new MutableField()
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