//> using toolkit 0.6.0

import scala.collection.mutable
import scala.compiletime.ops.long

object Day19:
    val path = os.pwd / ".." / "input" / "day19" / "input1.txt"
    val input: Vector[String] = os.read.lines(path).toVector
    val available: Set[String] = input.head.split(", ").toSet
    val desired: Vector[String] = input.drop(2)

    val waysset = mutable.Map[String, Long]("" -> 1)
    def wayspossible(s: String): Long =
        if !waysset.contains(s) then
            waysset += s -> s.indices.map(_ + 1).reverse.foldLeft(0L)((acc, n) =>
                if available.contains(s.take(n)) then acc + wayspossible(s.drop(n)) else acc)
        waysset(s)

    val desiredways = desired.map(wayspossible)

    def partOne(): Int =
        desiredways.count(_ > 0)

    def partTwo(): Long =
        desiredways.sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
