//> using toolkit 0.6.0

case class Group(xA: Long, yA: Long, xB: Long, yB: Long, xPrize: Long, yPrize: Long)

object Day13:
    val path = os.pwd / ".." / "input" / "day13" / "input1.txt"
    val input = os.read.lines(path).mkString("\n").split("\n\n").map(_.split("\n").toVector).toVector

    val buttonPattern = raw"Button [AB]: X\+(\d+), Y\+(\d+)".r
    val prizePattern = raw"Prize: X=(\d+), Y=(\d+)".r
    val groups: Vector[Group] = 
        input.map(group =>
            val buttonA = buttonPattern.findFirstMatchIn(group(0)).get
            val buttonB = buttonPattern.findFirstMatchIn(group(1)).get
            val prize = prizePattern.findFirstMatchIn(group(2)).get
            Group(buttonA.group(1).toLong,
                  buttonA.group(2).toLong,
                  buttonB.group(1).toLong,
                  buttonB.group(2).toLong,
                  prize.group(1).toLong,
                  prize.group(2).toLong)
        )

    def partOne(): Int =
        var totalMinTokens = 0
        for group <- groups do
            var minTokens = Int.MaxValue
            for a <- 0 to 100 do
                for b <- 0 to 100 do
                    val xDest = a * group.xA + b * group.xB
                    val yDest = a * group.yA + b * group.yB
                    if xDest == group.xPrize && yDest == group.yPrize then
                        val tokens = 3 * a + b
                        if tokens < minTokens then
                            minTokens = tokens

            if minTokens != Int.MaxValue then
                totalMinTokens += minTokens

        totalMinTokens

    def isWhole(d: Double): Boolean = 
        (d - d.round).abs < math.pow(2, -7) // ????? weird, wrong answer for 2^-6

    def partTwo(): Long =
        groups
        .map{ case Group(xA, yA, xB, yB, xPrize, yPrize) =>
            Group(xA, yA, xB, yB, xPrize + 10000000000000L, yPrize + 10000000000000L)
        }.foldLeft(0L)((acc, group) => group match
            case Group(xA, yA, xB, yB, xPrize, yPrize) =>

                // a = number of A presses
                // b = number of B presses
                // xA * a + xB * b = xPrize
                // yA * a + yB * b = yPrize
                // find the integer solution to the intersection between these two lines
                // there is only one solution, because of the degree

                val numerator = (xPrize / xB.toDouble - yPrize / yB.toDouble)
                val denominator = (xA / xB.toDouble - yA / yB.toDouble)
                val a = numerator / denominator
                val b = (xPrize - xA * a) / xB.toDouble
                acc + (if isWhole(a) && isWhole(b) then 3 * a.round + b.round else 0)
        )

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
