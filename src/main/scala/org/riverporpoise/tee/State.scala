package org.riverporpoise.tee

/**
  * Characterizes the instantaneous state of a TeeRex game.
  *
  * @author dhorlick
  */
class State
{	
}

object State
{
	val Won = new State()
	val Lost = new State()
	val Indeterminate = new State()
}