//> using toolkit 0.6.0

object Day02:
	val path = os.pwd / "input" / "day02" / "example1.txt"
	val lines = os.read.lines(path).toVector
	val reports =
		lines
		.map(_.split(" ").map(_.toInt).toVector)
		.toVector

	def isSafe(report: Vector[Int]): Boolean =
		val increasing = report.indices.drop(1).forall(i => report(i - 1) < report(i))
		val decreasing = report.indices.drop(1).forall(i => report(i - 1) > report(i))
		val diff = report.sliding(2).forall(xs => 
			val d = (xs(0) - xs(1)).abs
			1 <= d && d <= 3
		)
		(increasing || decreasing) && diff

	def partOne(): Unit =
		val result = reports.map(isSafe).count(_ == true)
		println(result)

	def isSafeDampened(report: Vector[Int]): Boolean =
		isSafe(report) || report.indices.exists(i => isSafe(report.patch(i, Nil, 1)))

	def partTwo(): Unit =
		val result = reports.map(isSafeDampened).count(_ == true)
		println(result)

	@main def run(): Unit =
		partOne()
		partTwo()
