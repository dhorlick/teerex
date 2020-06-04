package org.riverporpoise.tee

import org.riverporpoise.tee.Grid._

/**
 * A two-dimensional plane that can be populated as directed.
 *
 * @author dhorlick
 */
class Grid[T: Manifest] (private val initialRowCount : Int = 0,
								  private val initialIndicesWithinRowCount : Int = 0,
								  val filler : T)
		extends Iterable[Iterator[T]]
{
	val initialArrayWidth = 8

	private var grid : Array[Array[T]] = Array.fill (
		if (initialRowCount > initialArrayWidth) initialRowCount else initialArrayWidth,
		if (initialIndicesWithinRowCount > initialArrayWidth) initialIndicesWithinRowCount else initialArrayWidth
	) (filler)

	private var theRowCount = initialRowCount
	private var theIndicesWithinRowCount = initialIndicesWithinRowCount

	def get(rowIndex : Int, indexWithinRow : Int) : T =
	{
		require(rowIndex < theRowCount)
		require(indexWithinRow < theIndicesWithinRowCount)
		
		grid(rowIndex)(indexWithinRow)
	}

	def rowCount : Int =
	{
		theRowCount
	}
	
	def indicesWithinRowCount : Int =
	{
		theIndicesWithinRowCount
	}
	
	protected def expandIfNecessary(rowIndex: Int, indexWithinRow: Int) =
	{
		require(rowIndex >= 0, "Row index cannot be negative: " + rowIndex)
		require(indexWithinRow >= 0, "Index within row cannot be negative: " + indexWithinRow)

		if (rowCount < rowIndex + 1 || indicesWithinRowCount < indexWithinRow + 1)
		{
			val oldRowCount = rowCount
			val oldIndicesWithinRowCount = indicesWithinRowCount

			val newRowCount =
				if (rowCount < rowIndex + 1)
					rowIndex + 1
				else
					rowCount

			val newIndicesWithinRowCount =
				if (indicesWithinRowCount < indexWithinRow + 1)
					indexWithinRow + 1
				else
					indicesWithinRowCount

			val oldGrid = grid

			grid = Array.fill(newRowCount, newIndicesWithinRowCount)(filler)
			theRowCount = newRowCount
			theIndicesWithinRowCount = newIndicesWithinRowCount

			if (oldRowCount > 0 && oldIndicesWithinRowCount > 0)
			{
				// Copy old to new

				for (oldRowIndex <- 0 to (oldRowCount - 1))
				{
					for (oldIndexWithinRow <- 0 to (oldIndicesWithinRowCount - 1))
					{
						grid(oldRowIndex)(oldIndexWithinRow) = oldGrid(oldRowIndex)(oldIndexWithinRow)
					}
				}
			}
		}
	}

	def set(rowIndex : Int, indexWithinRow : Int, myElement : T) : Unit =
	{
		expandIfNecessary(rowIndex, indexWithinRow)
		grid(rowIndex)(indexWithinRow) = myElement
	}

	override def iterator : Iterator[Iterator[T]] =
	{
		new Iterator[Iterator[T]] ()
		{
			val gridIterator = grid.iterator
			var currentRow : Array[T] = null
			var currentElement : Iterator[T] = null
			var rowIndex = 0

			def hasNext() : Boolean = 
			{
				rowIndex < theRowCount && gridIterator.hasNext
			}
			
			def next() : Iterator[T] =
			{
				if (rowIndex + 1 > theRowCount)
					throw new NoSuchElementException("No rowIndex="+(rowIndex+1))

				currentRow = gridIterator.next()

				currentElement = new Iterator[T]
				{
					val gridIteratorIterator = currentRow.iterator
					var indexWithinRow = 0

					def hasNext() : Boolean =
					{
						indexWithinRow < theIndicesWithinRowCount && gridIteratorIterator.hasNext
					}
					
					def next() : T =
					{
						if (indexWithinRow + 1 > theIndicesWithinRowCount)
							throw new NoSuchElementException("No indexWithinRow="+(indexWithinRow+1))

						val result : T = gridIteratorIterator.next()
						
						indexWithinRow = indexWithinRow + 1

						result
					}
				}

				rowIndex = rowIndex + 1

				currentElement
			}
		}
	}

	def translate(path : Path) : Grid[T] =
	{
		val translated = new Grid[T] (rowCount, indicesWithinRowCount, filler)

		Grid.translate(this, translated, path)

		translated
	}
}

object Grid
{
	def translate[T](sourceGrid : Grid[T], targetGrid : Grid[T], path : Path) : Unit =
	{
		for (rowIndex <- 0 to sourceGrid.rowCount-1)
		{
			for (indexWithinRow <- 0 to sourceGrid.indicesWithinRowCount-1)
			{
				targetGrid.set(rowIndex + path.rowsDown, indexWithinRow + path.indicesRightWithinRow, sourceGrid.get(
						rowIndex, indexWithinRow))
			}
		}
	}
}
