package org.riverporpoise.geometry

/**
 * A two-dimensional shape with three sides.
 *
 * @author dhorlick
 */
class Triangle(a: DecimalCoordinates, b: DecimalCoordinates, c: DecimalCoordinates)
		extends Polygon(List(a, b, c))
{
}