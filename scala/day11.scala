//> using toolkit 0.6.0

import scala.collection.mutable

object Day11:
    val path = os.pwd / ".." / "input" / "day11" / "input1.txt"
    var input = os.read.lines(path).mkString.split(" ").map(_.toLong).toVector

    def replacements(nbr: Long): Vector[Long] =
        val digits = nbr.toString
        val nbrDigits = digits.length

        if nbrDigits % 2 == 0 then
            Vector(
                digits.take(nbrDigits / 2).toLong,
                digits.drop(nbrDigits / 2).toLong
                )
        else if nbr == 0 then
            Vector(1L)
        else
            Vector(nbr * 2024L)

    def partOne(): Int =
        (0 until 25).foldLeft(input)((acc, _) => acc.flatMap(replacements)).size

    def partTwo(): Long =
        val current = mutable.Map[Long, Long]()
        val seen = mutable.Map[Long, Vector[Long]]()

        for nbr <- input do
            current += nbr -> 1

        for i <- 0 until 75 do
            val next = mutable.Map[Long, Long]()

            for (nbr, count) <- current do
                if !seen.contains(nbr) then
                    seen(nbr) = replacements(nbr)

                seen(nbr)
                .foreach(otherNbr =>
                    next(otherNbr) = next.getOrElse(otherNbr, 0L) + count
                )

            current.clear()
            current.addAll(next)

        current.values.sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
