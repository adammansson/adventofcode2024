//> using toolkit 0.6.0

object Day13:
    val path = os.pwd / ".." / "input" / "day13" / "input1.txt"
    val input = os.read.lines(path).mkString("\n").split("\n\n").map(_.split("\n").toVector).toVector

    val buttonPattern = raw"Button [AB]: X\+(\d+), Y\+(\d+)".r
    val prizePattern = raw"Prize: X=(\d+), Y=(\d+)".r
    val groups: Vector[((Int, Int), (Int, Int), (Int, Int))] = 
        input.map(group =>
            val buttonA = buttonPattern.findFirstMatchIn(group(0)).get
            val buttonB = buttonPattern.findFirstMatchIn(group(1)).get
            val prize = prizePattern.findFirstMatchIn(group(2)).get
            (
                (buttonA.group(1).toInt, buttonA.group(2).toInt),
                (buttonB.group(1).toInt, buttonB.group(2).toInt),
                (prize.group(1).toInt, prize.group(2).toInt)

            )
        )
        
    def partOne(): Int =
        var globalMinTokens = 0
        for group <- groups do
            var minTokens = Int.MaxValue
            for a <- 0 to 100 do
                for b <- 0 to 100 do
                    val X = a * group._1._1 + b * group._2._1
                    val Y = a * group._1._2 + b * group._2._2
                    if X == group._3._1 && Y == group._3._2 then
                        val tokens = 3 * a + b
                        if tokens < minTokens then
                            minTokens = tokens

            if minTokens != Int.MaxValue then
                globalMinTokens += minTokens

        globalMinTokens

    def isWhole(d: Double): Boolean = 
        (d - d.round).abs < math.pow(2, -7) // ????? weird, wrong answer for 2^-6

    def partTwo(): Long =
        groups.map(t => (t._1, t._2, (t._3._1 + 10000000000000L, t._3._2 + 10000000000000L)))
        .foldLeft(0L)((acc, group) => 
            val xA = group._1._1
            val yA = group._1._2
            val xB = group._2._1
            val yB = group._2._2
            val xPrize = group._3._1
            val yPrize = group._3._2

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
            acc + (if isWhole(a) && isWhole(b) then (3 * a.round + b.round) else 0)
        )

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
