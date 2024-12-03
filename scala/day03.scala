//> using toolkit 0.6.0

import scala.util.matching.Regex

object Day03:
	val path = os.pwd / ".." / "input" / "day03" / "input1.txt"
	val lines = os.read.lines(path).toVector
	val line = lines.mkString

	def partOne(): Unit =
		val result =
			"mul\\(([0-9]+),([0-9]+)\\)".r
			.findAllMatchIn(line)
			.foldLeft(0)((acc, m) => acc + m.group(1).toInt * m.group(2).toInt)

		println(result)

	def partTwo(): Unit =
		val result =
			"mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)".r
			.findAllMatchIn(line)
			.foldLeft((true, 0))((t, s) => s match
				case _ if s.matched == "do()" => (true, t._2)
				case _ if s.matched == "don't()" => (false, t._2)
				case m =>
					if t._1 then
						(t._1, t._2 + m.group(1).toInt * m.group(2).toInt)
					else
						(t._1, t._2)
			)

		println(result._2)

	@main def run(): Unit =
		partOne()
		partTwo()
