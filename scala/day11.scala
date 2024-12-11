//> using toolkit 0.6.0

import scala.collection.mutable

object Day11:
    val path = os.pwd / ".." / "input" / "day11" / "input1.txt"
    var input = os.read.lines(path).mkString.split(" ").map(_.toLong).toVector

    def replacements(nbr: Long): Vector[Long] =
        val digits = nbr.toString
        val nbrDigits = digits.length

        if nbrDigits % 2 == 0 then
            Vector(digits.take(nbrDigits / 2).toLong,
                   digits.drop(nbrDigits / 2).toLong)
        else if nbr == 0 then
            Vector(1L)
        else
            Vector(nbr * 2024L)

    def partOne(): Int =
        (0 until 25).foldLeft(input)((acc, _) => acc.flatMap(replacements)).size

    def partTwo(): Long =
        val current = mutable.Map[Long, Long]()
        val seen = mutable.Map[Long, Vector[Long]]()

        current.addAll(input.map(_ -> 1))

        for i <- 0 until 75 do
            val next = mutable.Map[Long, Long]()

            for (nbr, count) <- current do
                if !seen.contains(nbr) then
                    seen(nbr) = replacements(nbr)

                seen(nbr)
                .foreach(replacement =>
                    next(replacement) = next.getOrElse(replacement, 0L) + count
                )

            current.clear()
            current.addAll(next)

        current.values.sum

    def withReplacements(current: Map[Long, Long], replacements: Vector[Long], count: Long): Map[Long, Long] =
        replacements
        .foldLeft(current)((acc, replacement) => acc + (replacement -> (acc.getOrElse(replacement, 0L) + count)))

    def partTwoRecursive(blinks: Int, state: (Map[Long, Long], Map[Long, Vector[Long]])): Long =
        blinks match
            case 0 =>
                state._1.values.sum
            case _ =>
                partTwoRecursive(
                    blinks - 1,
                    state._1.foldLeft(Map.empty[Long, Long], state._2)((acc, entry) =>
                        acc._2.get(entry._1) match
                            case Some(replacements) =>
                                 (withReplacements(acc._1, replacements, entry._2),
                                  acc._2)
                            case None => 
                                (withReplacements(acc._1, replacements(entry._1), entry._2),
                                 acc._2 + (entry._1 -> replacements(entry._1)))
                    )
                )

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
        println(partTwoRecursive(75, (input.map(_ -> 1L).toMap, Map.empty)))
