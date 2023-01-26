package org.riverporpoise.tee

object TeeGridElement extends Enumeration
{
    type TeeGridElement = Value
	val tee = Value("*")  // peg
	val hole = Value("o")   // hole
	val space = Value(" ")   // void
}