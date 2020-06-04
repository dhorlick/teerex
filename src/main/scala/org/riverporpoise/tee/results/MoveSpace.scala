package org.riverporpoise.tee.results

import org.riverporpoise.tee.{State, Move}
import collection.mutable._

/**
 * @author dhorlick
 */
abstract class MoveSpace(val concurrent : Boolean = false)
{
	val nodes: Buffer[GameSpace] = buildArrayBuffer

	def buildArrayBuffer[A] : Buffer[A] =
	{
		if (concurrent)
			new ArrayBuffer[A] () with SynchronizedBuffer[A]
		else
			new ArrayBuffer[A] ()
	}
	
	def buildSet[A] : Set[A] =
	{
		if (concurrent)
			new HashSet[A] () with SynchronizedSet[A]
		else
			new HashSet[A] ()
	}
	
	/**
	 * Removes this node if empty. If this node is not empty, empty descendant nodes will be pruned.
	 */
	def prune(): Unit

	def add(move: Move, state: State): GameSpace =
	{
		val subSolutionSpace = new GameSpace(move, this, state, concurrent)
		nodes.append(subSolutionSpace)
		subSolutionSpace
	}

	def solutionSpace: SolutionSpace

	def countLeaves(f: GameSpace => Boolean): Int =
	{
		if (nodes.size == 0)
		{
			if (f(this.asInstanceOf[GameSpace])) 1 else 0
		}
		else
		{
			var leaves = 0

			nodes.foreach
			{
				node =>

					leaves += node.countLeaves(f)
			}

			leaves
		}
	}

	override def toString(): String =
	{
		var response: String = "[ "
		nodes.foreach
		{
			gameSpace =>

				response += gameSpace.toString()
				response += " "
		}

		response += " ]"

		response
	}
}