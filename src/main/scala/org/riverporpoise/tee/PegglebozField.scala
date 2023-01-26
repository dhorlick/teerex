package org.riverporpoise.tee

class PegglebozField(override val width : Int = 7) extends Field(width)
{
	def buildTeeGrid(expandToFit : Boolean = false) : TeeGrid =
	{
		val triangularField = new TriangularField()
		val pegglebozGrid = new TeeGrid(7, (7 * 2)-1)
		Grid.translate(triangularField.buildTeeGrid(), pegglebozGrid, new Path(1, 2))
		pegglebozGrid.set(0, 5, TeeGridElement.tee)
		pegglebozGrid.set(0, 7, TeeGridElement.tee)
		pegglebozGrid.set(1, 6, TeeGridElement.tee)  // (by default it's modeled as a hole)
		pegglebozGrid.set(3, 6, TeeGridElement.hole)  // (by default it's modeled as a tee)
		pegglebozGrid.set(5, 0, TeeGridElement.tee)
		pegglebozGrid.set(5, 12, TeeGridElement.tee)
		pegglebozGrid.set(6,  1, TeeGridElement.tee)
		pegglebozGrid.set(6, 11, TeeGridElement.tee)
		pegglebozGrid
	}
}

object PegglebozField
{
	def main(args: Array[String]) =
	{
		new PegglebozField().solveThenDocument()
	}
}