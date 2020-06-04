package text

import org.riverporpoise.text.CharacterGrid

/**
 * Concatenates text graphics for output to a console.
 *
 * @author dhorlick
 */
class WrappingTileSpace(val screenWidth : Int = 80)
{
	require(screenWidth > 0, "Screen width must be a non-zero positive integer: "+screenWidth)
	
	private val content = new CharacterGrid
	private var strip = new CharacterGrid
	
	def add(addition : CharacterGrid) =
	{
		if (strip.indicesWithinRowCount + addition.indicesWithinRowCount > screenWidth)
		{
			periodicMaintenance
		}

		strip.addToRight(addition)
	}
	
	private def bankStripIfPopulated() : Boolean =
	{
		if (!strip.empty)
		{
			content.addBelow(strip)
			strip = new CharacterGrid()
			true
		}
		else
		{
			false
		}
	}

	def periodicMaintenance =
	{
		if (bankStripIfPopulated())
		{
			content.addBelow(" ")
		}
	}

	def rowCount =
	{
		periodicMaintenance
		content.rowCount
	}

	def indicesWithinRowCount =
	{
		periodicMaintenance
		content.indicesWithinRowCount
	}

	override def toString() =
	{
		periodicMaintenance
		content.toString()
	}
}