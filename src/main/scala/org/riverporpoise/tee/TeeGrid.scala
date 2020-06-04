package org.riverporpoise.tee

import org.riverporpoise.text.CharacterGrid
import collection.mutable.ListBuffer
import org.riverporpoise.geometry.{DecimalCoordinates, Polygon}

/**
 * A two-dimensional plane populated by tees, holes or spaces. Facilitates the rendering of a game board.
 *
 * @author dhorlick
 */
class TeeGrid(private var initialRowCount : Int = 8,
			  private var initialIndicesWithinRowCount : Int = 8)
		extends Grid[TeeGrid.Element.Value] (
			initialRowCount = initialRowCount,
			initialIndicesWithinRowCount = initialIndicesWithinRowCount,
			filler = TeeGrid.Element.space)
{
	val polygons : ListBuffer[Polygon] = new ListBuffer[Polygon]
	
	def toCharacterGrid =
	{
		val characterGrid = new CharacterGrid(initialRowCount, initialIndicesWithinRowCount)

		zipWithIndex.foreach
		{
			rowAndRowIndex =>

				val row = rowAndRowIndex._1
				val rowIndex = rowAndRowIndex._2

				row.zipWithIndex.foreach
				{
					myElementAndIndexWithinRow =>

						val myElement = myElementAndIndexWithinRow._1
						val indexWithinRow = myElementAndIndexWithinRow._2
						
						characterGrid.set(rowIndex, indexWithinRow, myElement.toString.charAt(0))
				}
		}

		characterGrid
	}

	def add(polygon : Polygon, expandToFit : Boolean = false)
	{
		if (expandToFit)
		{
			polygon.foreach
			{
				vertex =>

					if (vertex.y >0 && vertex.x > 0)
						expandIfNecessary(vertex.y.toInt, vertex.x.toInt)
			}
		}

		polygons.append(polygon)
	}

	/**
	 * @return a translated copy of this tee grid.
	 */
	override def translate(path : Path) : TeeGrid =
	{
		val translated = new TeeGrid(rowCount, indicesWithinRowCount)

		Grid.translate(this, translated, path)

		polygons.foreach
		{
			polygon =>

				translated.add(polygon.translate(path))
		}

		translated
	}

	def translateToQuadrantOne : TeeGrid =
	{
		var lowestX : Int = 0
		var lowestY : Int = 0

		polygons.foreach
		{
			polygon =>

				polygon.vertices.foreach
				{
					vertex =>

						if (vertex.x < lowestX)
							lowestX = math.floor(vertex.x.doubleValue()).intValue
						if (vertex.y < lowestY)
							lowestY = math.floor(vertex.y.doubleValue()).intValue
				}
		}

		if (lowestX < 0 || lowestY < 0)
			translate(new Path(-1*lowestY, -1*lowestX))
		else
			this
	}
}

object TeeGrid
{
	object Element extends Enumeration
	{
		val tee = Value("*")
		val hole = Value("o")
		val space = Value(" ")
	}
}