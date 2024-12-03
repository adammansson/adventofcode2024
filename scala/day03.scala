//> using toolkit 0.6.0

import scala.util.matching.Regex

object Day03:
	val path = os.pwd / ".." / "input" / "day03" / "input1.txt"
	val lines = os.read.lines(path).toVector

	val numberPattern: Regex = "mul\\([0-9]+,[0-9]+\\)".r

	def performMul(s: String): Int =
		s.drop(4).dropRight(1).split(",").map(_.toInt).reduce((acc, i) => acc * i)

	def partOne(): Unit =
		val line = lines.mkString
		val result = numberPattern.findAllIn(line).map(performMul).sum
		println(result)

	def solveTwo(line: String, enabled: Boolean, acc: Int): Int =
		numberPattern.findFirstIn(line) match
			case Some(mulmatch) => 
				if line.startsWith(mulmatch) then
					if enabled then
						solveTwo(line.drop(mulmatch.length), enabled, acc + performMul(mulmatch))
					else
						solveTwo(line.drop(mulmatch.length), enabled, acc)
				else if line.startsWith("do()") then
					solveTwo(line.drop("do()".length), true, acc)
				else if line.startsWith("don't()") then
					solveTwo(line.drop("dont'()".length), false, acc)
				else
					solveTwo(line.tail, enabled, acc)

			case None => acc

	def partTwo(): Unit =
		var line = lines.mkString
		val result = solveTwo(line, true, 0)
		println(result)

	@main def run(): Unit =
		partOne()
		partTwo()