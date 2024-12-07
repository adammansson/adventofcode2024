//> using toolkit 0.6.0

object Day07:
    val path = os.pwd / ".." / "input" / "day07" / "input1.txt"
    val input = os.read.lines(path).toVector
    val equations = input.map(line =>
        val xs = line.split(":").toVector
        (xs(0).toLong, xs(1).split(" ").filter(_.nonEmpty).map(_.toLong).toVector)
    )

    def checkEquation(lhs: Long, rhs: Vector[Long], withConcat: Boolean = false): Boolean =
        if rhs.length == 1 then
            lhs == rhs(0)
        else
            checkEquation(lhs, (rhs(0) + rhs(1)) +: rhs.drop(2), withConcat) ||
            checkEquation(lhs, (rhs(0) * rhs(1)) +: rhs.drop(2), withConcat) ||
            (withConcat && checkEquation(lhs, (rhs(0).toString + rhs(1).toString).toLong +: rhs.drop(2), withConcat))

    def calibrationResult(withConcat: Boolean = false): Long =
        equations
        .filter(equation => checkEquation(equation._1, equation._2, withConcat))
        .map(equation => equation._1).sum

    def partOne(): Long =
        calibrationResult()

    def partTwo(): Long =
        calibrationResult(withConcat = true)

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
