package org.riverporpoise.tee

/**
 * @author dhorlick
 */
case class UnoccupiedJumpSpaceException(jumpSpace : Coordinates)
		extends Exception("The jump space "+jumpSpace+" is unoccupied")
{
}