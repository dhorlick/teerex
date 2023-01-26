package org.riverporpoise.tee

import org.riverporpoise.text.CharacterGrid
import collection.mutable.ListBuffer
import compat.Platform
import results.{MoveSpace, SolutionSpace}
import text.WrappingTileSpace
import scala.beans.BeanProperty

/**
 * Models the game board for that dinnertime triangle game with all the tees (called "pegs" in the code).
 * Alternating rows are offset by one from their predecessors.
 *
 * You drag and drop the tees, jumping a neighbor to land in an empty space. The goal is to have one tee remaining.
 *
 * The game starts with all but one hole filled with a tee.
 *
 * Please note that not all widths may result in a winnable game, and that widths greater than 6 take a long time
 * to exhaustively solve on 2012 hardware.
 *
 * Earlier version of TeeRex provided mutable and immutable Field subclasses. But to simplify things, we now just have mutable implementations.
 *
 * @author dhorlick
 */
abstract class Field(val width : Int)  // TODO model height, as well
{
	def buildTeeGrid(expandToFit : Boolean = false) : TeeGrid

	val teeGrid = buildTeeGrid()

	def availableMoves : Seq[Move] =
	{
		val availableMoves = new ListBuffer[Move] ()

		var y = 0
		for (y <- 0 to teeGrid.rowCount - 1)
		{
			var x = 0
			for (x <- 0 to teeGrid.indicesWithinRowCount - 1)
			{
				if (teeGrid.get(y, x) == TeeGridElement.tee)
				{
					for (direction <- Direction.values)
					{
						val oneAway: (Int, Int) = (y + direction.vector._1, x + direction.vector._2)
						val twoAway: (Int, Int) = (y + (2 * direction.vector._1), x + (2 * direction.vector._2))

						if (valid(oneAway) && valid(twoAway) 
								&& teeGrid.get(oneAway._1, oneAway._2) == TeeGridElement.tee 
								&& teeGrid.get(twoAway._1, twoAway._2) == TeeGridElement.hole)
						{
							availableMoves.append(new Move(direction = direction, (y, x)))
						}
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
	    countWhatever(TeeGridElement.tee)
	}

	def holeCount : Int =
	{
		countWhatever(TeeGridElement.hole)
	}

	private def countWhatever(teeGridElement: TeeGridElement.TeeGridElement) =
	{
		var count = 0
		
		for (y <- 0 to teeGrid.rowCount - 1)
		{
			var x = 0
			for (x <- 0 to teeGrid.indicesWithinRowCount - 1)
			{
				if (teeGrid.get(y, x) == teeGridElement)
				{
					count += 1
				}
			}
		}

		count
	}

	def valid(zeroOrderedCoords: (Int, Int)) : Boolean =
	{
		if ((zeroOrderedCoords._1 >= 0)
			&& (zeroOrderedCoords._2 >= 0)
			&& (zeroOrderedCoords._1 < teeGrid.rowCount)
			&& (zeroOrderedCoords._2 < teeGrid.indicesWithinRowCount))
		{
			true	
		}
		else
		{
			false
		}
	}

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
		val desc = new StringBuilder(1+teeGrid.size);
		desc.append("1");
		// since we don't want to lose information from leading zero digits

		teeGrid.foreach
		{
			row => row.foreach
			{ 
				place => if (place == TeeGridElement.space)
				{
					desc.append("00")
				}
				else if (place == TeeGridElement.hole)
				{
					desc.append("01")
				}
				else if (place == TeeGridElement.tee)
				{
					desc.append("11")
				}
				else 
				{
					throw new UnsupportedOperationException("Unsupported place: " + place)
				}
			}
		}
		BigInt(desc.toString(), 2)
	}

	override def hashCode() : Int =
	{
		bigInt.intValue
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
						perform(move)
						val outcome = state()

						if (outcome==State.Won)
						{
							return new Some(move)
						}
						else
						{
							val integerized = bigInt()
							if (!fieldStatesInvestigated.contains(integerized))
							{
								Field.logTimelessly(this)

								Field.log("Performed: "+move)

								fieldStatesInvestigated.append(integerized)
							}

							if (outcome==State.Indeterminate)
							{
								if (arbitraryGoodMove(fieldStatesInvestigated, undoAfterwards)!=None)
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

	def perform(move : Move) =
	{
		val oneAway: (Int, Int) = (move.startingCoords._1 + move.direction.vector._1, move.startingCoords._2 + move.direction.vector._2)
		val twoAway: (Int, Int) = (move.startingCoords._1 + 2 * (move.direction.vector._1), move.startingCoords._2 + 2 * (move.direction.vector._2))
		
		var problem : Option[String] = None

		if (!valid(move.startingCoords))
		{
			problem = Option("Launch spot does not exist")
		}
		if (!valid(oneAway))
		{
			problem = Option("Jump spot does not exist")
		}
		if (!valid(twoAway))
		{
			problem = Option("Target does not exist")
		}
		if (teeGrid.get(move.startingCoords._1, move.startingCoords._2) != TeeGridElement.tee)
		{
			problem = Option("Launch spot is not inhabited by a tee")
		}
		if (teeGrid.get(oneAway._1, oneAway._2) != TeeGridElement.tee)
		{
			problem = Option("Jump spot is not inhabited by a tee")
		}
		if (teeGrid.get(twoAway._1, twoAway._2) != TeeGridElement.hole)
		{
			problem = Option("Target is not inhabited by a hole")
		}
		if (problem != None)
		{
			throw new InvalidJumpException(move.startingCoords, twoAway, this, problem.get)
		}

		teeGrid.set(move.startingCoords._1, move.startingCoords._2, TeeGridElement.hole)
		teeGrid.set(oneAway._1, oneAway._2, TeeGridElement.hole)
		teeGrid.set(twoAway._1, twoAway._2, TeeGridElement.tee)
	}

	def unperform(move : Move) =
	{
		val oneAway: (Int, Int) = (move.startingCoords._1 + move.direction.vector._1, move.startingCoords._2 + move.direction.vector._2)
		val twoAway: (Int, Int) = (move.startingCoords._1 + 2 * (move.direction.vector._1), move.startingCoords._2 + 2 * (move.direction.vector._2))
		
		var problem : Option[String] = None

		if (!valid(move.startingCoords))
		{
			problem = Option("Launch spot does not exist")
		}
		if (!valid(oneAway))
		{
			problem = Option("Jump spot does not exist")
		}
		if (!valid(twoAway))
		{
			problem = Option("Target does not exist")
		}
		if (teeGrid.get(move.startingCoords._1, move.startingCoords._2) != TeeGridElement.hole)
		{
			problem = Option("Launch spot is not inhabited by a hole")
		}
		if (teeGrid.get(oneAway._1, oneAway._2) != TeeGridElement.hole)
		{
			problem = Option("Jump spot is not inhabited by a hole")
		}
		if (teeGrid.get(twoAway._1, twoAway._2) != TeeGridElement.tee)
		{
			problem = Option("Target is not inhabited by a tee")
		}
		if (problem != None)
		{
			throw new InvalidJumpException(move.startingCoords, twoAway, this, problem.get)
		}
		
		teeGrid.set(move.startingCoords._1, move.startingCoords._2, TeeGridElement.tee)
		teeGrid.set(oneAway._1, oneAway._2, TeeGridElement.tee)
		teeGrid.set(twoAway._1, twoAway._2, TeeGridElement.hole)
	}

	/**
	 * Will play-thru all interesting solutions and record the results to moveSpace
	 * and moveSpace.solutionSpace. 
	 */
	def playAll(moveSpace : MoveSpace) : Unit =
	{
		val solutionSpace = moveSpace.solutionSpace
		val moves = availableMoves
		
		Field.log("availableMoves.size = "+availableMoves.size)
		
		if (moves.length==0)
		{
			Field.log("No available moves, pruning movespace.")
			moveSpace.prune()
		}
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
								Field.log("won!")
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

	def arbitraryGoodMove : Option[Move] =
	{
		arbitraryGoodMove(new ListBuffer[BigInt], true)
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

	def solutionSpace : SolutionSpace =
	{
		new SolutionSpace(false)
	}

	def unperform(moves : Seq[Move]) : Unit =
	{
		moves.reverse.foreach
		{
			move =>

				unperform(move)
		}
	}

	def solveThenDocument() =
	{
		val solutionSpace = solve()
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
				println(performAndRecord(moves))
				unperform(moves)

				println()
				
				if (i<solutionSpace.bestWins.length-1)
					println()
		}
	}

	override def toString() =
	{
		teeGrid.toCharacterGrid.toString
	}
}

object Field
{
	val peg = new Peg()
	val defaultWidth : Int = 5
	@BeanProperty var logging = false

	def log(message : Any) =
	{
		if (logging)
		{
			print(System.currentTimeMillis())
			print(": ")
			println(message)
		}
	}

	def logTimelessly(message : Any)
	{
		if (logging)
		{
			println(message)
		}
	}
}