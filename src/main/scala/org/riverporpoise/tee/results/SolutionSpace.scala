package org.riverporpoise.tee.results

import org.riverporpoise.tee.{State, Field}
import collection.mutable.{Buffer, HashSet, ListBuffer}


/**
 * @author dhorlick
 */
class SolutionSpace(concurrent : Boolean) extends MoveSpace(concurrent : Boolean)
{
	private val fieldStatesInvestigated = buildSet[BigInt]

	def prune: Unit =
	{
		nodes.foreach
		{
			node =>
				node.prune()
		}
	}

	/**
	 * @return true, if this was not previously registered
	 */
	def registerFieldState(field: Field): Boolean =
	{
		val bigInt = field.bigInt()
		if (fieldStatesInvestigated.contains(bigInt))
			false
		else
		{
			fieldStatesInvestigated += bigInt
			true
		}
	}

	override def solutionSpace: SolutionSpace =
	{
		this
	}

	def winsByDepth: Seq[Seq[GameSpace]] =
	{
		val collated : Buffer[Buffer[GameSpace]] = buildArrayBuffer

		nodes.foreach
		{
			node =>
				node.harvestLeaves(collated, 1, _.outcome == State.Won)
		}

		collated
	}

	def bestWins: Seq[GameSpace] =
	{
		val wins : Seq[Seq[GameSpace]] = winsByDepth.filter
		{
			seq =>
				seq.size > 0
		}

		if (wins.size>0)
			wins(0)
		else
			List[GameSpace] ()
	}

	override def toString(): String =
	{
		var response: String = ""

		response += "wins = "
		response += countLeaves(_.outcome == State.Won)
		response += ", losses = "
		response += countLeaves(_.outcome == State.Lost)
		response += ", fieldStatesInvestigated.size = "
		response += fieldStatesInvestigated.size

		response
	}
}