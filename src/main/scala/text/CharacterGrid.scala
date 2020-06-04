package org.riverporpoise.text

import org.riverporpoise.tee.{Grid, Coordinates}

/**
 * @author dhorlick
 */
class CharacterGrid(private var initialRowCount : Int = 0,
					private var initialIndicesWithinRowCount : Int = 0)
		extends Grid[Char] (initialRowCount = initialRowCount,
							initialIndicesWithinRowCount = initialIndicesWithinRowCount,
							filler = ' ')
{
	def this(strings : Array[String]) =
	{
		this(strings.length, CharacterGrid.maxLength(strings))
		set(new Coordinates(0,0), strings)
	}

	def this(text : String) =
	{
		this(text.split('\n'))
	}

	def this(any : Any) =
	{
		this(any.toString)
	}

	def set(coords : Coordinates = new Coordinates(0, 0), other : CharacterGrid) : Unit =
	{
		require(coords.rowIndex>=0, "coords.rowIndex cannot be negative: "+coords.rowIndex)
		require(coords.indexWithinRow>=0, "coords.indexWithinRow cannot be negative: "+coords.indexWithinRow)

		for (oldRow <- 0 to other.rowCount-1)
		{
			for (oldIndexWithinRow <- 0 to other.indicesWithinRowCount-1)
			{
				set(oldRow + coords.rowIndex, oldIndexWithinRow + coords.indexWithinRow, other.get(oldRow, oldIndexWithinRow))
			}
		}
	}

	def set(coords : Coordinates, text : String) : Unit =
	{
		set(coords, text.split('\n'))
	}

	def set(coords : Coordinates, strings : Array[String]) : Unit =
	{
		var rowIndex = coords.rowIndex
		var indexWithinRow = coords.indexWithinRow

		require(rowIndex>=0, "Row cannot be negative: "+rowIndex)
		require(indexWithinRow>=0, "Index within row cannot be negative: "+indexWithinRow)

		strings.foreach
		{
			line =>

				indexWithinRow = coords.indexWithinRow

				line.foreach
				{
					myCharacter =>
						set(rowIndex, indexWithinRow, myCharacter)
						indexWithinRow += 1
				}

				rowIndex += 1
		}
	}

	def addToRight(x : Any) : Unit  =
	{
		addToRight(x.toString)
	}

	def addToRight(text : String) : Unit  =
	{
		val targetIndexWithinRow =
			if (indicesWithinRowCount == 0)
				0
			else
				indicesWithinRowCount + 2

		set(new Coordinates(0, targetIndexWithinRow), text)
	}

	def addBelow(string : String) : Unit  =
	{
		set(new Coordinates(rowCount, 0), string)
	}

	def addBelow(other : CharacterGrid) : Unit  =
	{
		set(new Coordinates(rowCount, 0), other)
	}
	
	def empty : Boolean =
	{
		(rowCount==0 && indicesWithinRowCount==0)
	}
	
	override def toString() : String =
	{
		val stringBuilder = new StringBuilder
		
		zipWithIndex.foreach
		{
			rowAndRowIndex =>

				val row = rowAndRowIndex._1
				val rowIndex = rowAndRowIndex._2
				
				row.foreach
				{
					myChar =>

						stringBuilder.append(myChar)
				}

				if (rowIndex < rowCount-1)
					stringBuilder.append('\n')
		}

		stringBuilder.toString()
	}
}

object CharacterGrid 
{
	def maxLength(strings : Array[String]) : Int =
	{
		if (strings.length==0)
		{
			0
		}
		else
		{
			var maxLength = 0
			
			strings.foreach 
			{
				string =>
					
					if (string.length() > maxLength)
					{
						maxLength = string.length
					}
			}

			maxLength
		}
	}
}