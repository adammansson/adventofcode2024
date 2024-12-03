//> using toolkit 0.6.0

object Day01:
	val path = os.pwd / ".." / "input" / "day01" / "input1.txt"
	val lines = os.read.lines(path).toVector
	val groups =
		lines
		.map(_.split(" ").filterNot(_.isEmpty).map(_.toInt))
		.map(xs => (xs(0), xs(1)))
		.unzip

	def partOne(): Unit =
		val (leftList, rightList) = (groups._1.sorted, groups._2.sorted)
		val pairs = leftList.zip(rightList)
		val distances = pairs.map((lhs, rhs) => (rhs - lhs).abs)
		val result = distances.sum
		println(result)

	def partTwo(): Unit = 
		val (leftList, rightList) = groups
		val similarityScore = leftList.map(e => e * rightList.count(_ == e))
		val result = similarityScore.sum
		println(result)

	@main def run(): Unit =
		partOne()
		partTwo()