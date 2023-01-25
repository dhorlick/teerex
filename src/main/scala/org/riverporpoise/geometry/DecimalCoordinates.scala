package org.riverporpoise.geometry

import org.riverporpoise.tee.Path

/**
 * A position on a two-dimensional plane.
 *
 * @author dhorlick
 * @todo Let's find some way to merge this with the other Coordinates class; too confusing to have one ordered x,y and another that's ordered y,x
 */
class DecimalCoordinates(val x : BigDecimal, val y: BigDecimal)
{
	/**
	 * @return a translated copy of these decimal coordinates.
	 */
	def translate(path: Path): DecimalCoordinates =
	{
		new DecimalCoordinates(x + path.indicesRightWithinRow, y + path.rowsDown)
	}
}