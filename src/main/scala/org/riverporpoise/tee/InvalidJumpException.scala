package org.riverporpoise.tee

/**
 * Note that in some cases the exception may be associated with a REVERSE jump. 
 *
 * @author dhorlick
 */
case class InvalidJumpException(val startCoords : (Int, Int), val endCoords : (Int, Int), val field: Field, val problem: String)
		extends Exception("No allowable jump correponds to start "+startCoords+" and end "+endCoords+ "; " + problem + "\n" + field)
{
}