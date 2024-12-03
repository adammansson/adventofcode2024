//> using toolkit 0.6.0

import scala.util.matching.Regex

object Day03:
	val path = os.pwd / "input" / "day03" / "input1.txt"
	val lines = os.read.lines(path).toVector

	val numberPattern: Regex = "mul+\\([0-9]+,[0-9]+\\)".r

	def performMul(s: String): Int =
		s.drop(4).dropRight(1).split(",").map(_.toInt).reduce((acc, i) => acc * i)

	def partOne(): Unit =
		val line = lines.mkString
		val result = numberPattern.findAllIn(line).map(performMul).sum
		println(result)
	
	def partTwo(): Unit =
		var line = lines.mkString
		var enabled = true
		var result = 0

		var running = true
		while running do
			numberPattern.findFirstIn(line) match
				case Some(mulmatch) => 
					if line.startsWith(mulmatch) then
						if enabled then
							result += performMul(mulmatch)
					else if line.startsWith("do()") then
						enabled = true
					else if line.startsWith("don't()") then
						enabled = false
					line = line.tail
				case None =>
					running = false

		println(result)

	@main def run(): Unit =
		partOne()
		partTwo()