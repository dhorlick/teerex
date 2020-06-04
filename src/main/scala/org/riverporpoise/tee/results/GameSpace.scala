package org.riverporpoise.tee.results

import org.riverporpoise.tee.{State, Move}
import collection.mutable.{Buffer}


/**
 * Represents the evolving space of possible games. Will eventually be trimmed to leave only the solutions.
 *
 * @author dhorlick
 */
class GameSpace(val move: Move, val parent: MoveSpace, val outcome: State, concurrent : Boolean = false) extends MoveSpace(concurrent : Boolean)
{
	def prune(): Unit =
	{
		if (nodes.size == 0)
		{
			parent.nodes.-=(this)
		}
		else
		{
			nodes.foreach
			{
				node =>
					node.prune()
			}
		}
	}

	def solutionSpace: SolutionSpace =
	{
		parent.solutionSpace
	}

	def harvestLeaves(collated: Buffer[Buffer[GameSpace]], depth: Int, f: GameSpace => Boolean): Unit =
	{
		if (nodes.size == 0 && f(this))
		{
			while (collated.size < depth)
			{
				collated.append(buildArrayBuffer)
			}

			val seq = collated(depth - 1)
			seq.append(this)
		}
		else
		{
			nodes.foreach
			{
				node =>
					node.harvestLeaves(collated, depth + 1, f)
			}
		}
	}

	def inventoryMoves: Buffer[Move] =
	{
		val moves = buildArrayBuffer : Buffer[Move]

		var cursor = this: GameSpace
		moves.append(move)

		while (cursor.parent.isInstanceOf[GameSpace])
		{
			cursor = cursor.parent.asInstanceOf[GameSpace]
			moves.prepend(cursor.move)
		}

		moves
	}

	override def toString(): String =
	{
		var response: String = ""

		response += move.toString()

		response += " "
		response += super.toString()

		response
	}
}