package org.riverporpoise.tee

/**
  * Characterizes the instantaneous state of a TeeRex game.
  *
  * @author dhorlick
  */
class State(val description: String)
{
	override def toString =
	{
		description
	}
}

object State
{
	val Won = new State("Won")
	val Lost = new State("Lost")
	val Indeterminate = new State("Indeterminate")
}