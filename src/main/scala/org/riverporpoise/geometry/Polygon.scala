package org.riverporpoise.geometry

import org.riverporpoise.tee.Path

/**
 * A many-sided, two-dimensional shape.
 *
 * @author dhorlick
 */
class Polygon(val vertices : List[DecimalCoordinates]) extends Iterable[DecimalCoordinates]
{
	def iterator : Iterator[DecimalCoordinates] =
	{
		vertices.iterator
	}

	def verticesCount =
	{
		vertices.size
	}

	/**
	 * @return a translated copy of this polygon.
	 */
	def translate(path: Path): Polygon =
	{
		new Polygon(vertices.map
			{
				vertex =>

					vertex.translate(path)
			}
		)
	}
}