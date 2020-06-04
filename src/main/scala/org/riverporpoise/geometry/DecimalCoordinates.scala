package org.riverporpoise.geometry

import org.riverporpoise.tee.Path

/**
 * A position on a two-dimensional plane.
 *
 * @author dhorlick
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