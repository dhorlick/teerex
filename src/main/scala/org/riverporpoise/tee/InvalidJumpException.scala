package org.riverporpoise.tee

/**
 * @author dhorlick
 */
case class InvalidJumpException(val startCoords : Coordinates, val endCoords : Coordinates)
		extends Exception("No allowable jump correponds to start "+startCoords+" and end "+endCoords)
{
}