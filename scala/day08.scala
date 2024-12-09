//> using toolkit 0.6.0

import scala.collection.mutable

object Day08:
    val path = os.pwd / ".." / "input" / "day08" / "input1.txt"
    val input = os.read.lines(path).map(_.toVector).toVector

    def partOne(): Int =
        val antennas = mutable.Map[Char, Set[(Int, Int)]]()
        val antiNodes = mutable.Set[(Int, Int)]()

        input.zipWithIndex.foreach((row, y) => 
            row.zipWithIndex.foreach((c, x) => 
                if c != '.' then
                    antennas(c) = antennas.getOrElse(c, Set()) ++ Set((x, y))
            )
        )

        antennas.foreach((_, s) =>
            s.foreach(pos =>
                s.filterNot(_ == pos)
                .foreach(otherPos => 
                    val dx = otherPos._1 - pos._1
                    val dy = otherPos._2 - pos._2

                    antiNodes.add((pos._1 - dx), (pos._2 - dy))
                    antiNodes.add((pos._1 + 2 * dx), (pos._2 + 2 * dy))
                )
            )
        )

        antiNodes
        .filter(pos => 
            pos._2 >= 0 && pos._2 < input.length &&
            pos._1 >= 0 && pos._1 < input(0).length
        ).size

    def partTwo(): Long =
        val antennas = mutable.Map[Char, Set[(Int, Int)]]()
        val antiNodes = mutable.Set[(Int, Int)]()

        input.zipWithIndex.foreach((row, y) => 
            row.zipWithIndex.foreach((c, x) => 
                if c != '.' then
                    antennas(c) = antennas.getOrElse(c, Set()) ++ Set((x, y))
            )
        )

        antennas.foreach((_, s) =>
            s.foreach(pos =>
                s.filterNot(_ == pos)
                .foreach(otherPos => 
                    val dx = otherPos._1 - pos._1
                    val dy = otherPos._2 - pos._2

                    var tPos = pos
                    while tPos._2 - dy >= 0 && tPos._2 - dy < input.length &&
                          tPos._1 - dx >= 0 && tPos._1 - dx < input(0).length do
                        antiNodes.add((tPos._1 - dx), (tPos._2 - dy))
                        tPos = (tPos._1 - dx, tPos._2 - dy)

                    tPos = pos
                    while tPos._2 + dy >= 0 && tPos._2 + dy < input.length &&
                          tPos._1 + dx >= 0 && tPos._1 + dx < input(0).length do
                        antiNodes.add((tPos._1 + dx), (tPos._2 + dy))
                        tPos = (tPos._1 + dx, tPos._2 + dy)
                )
            )
        )

        antiNodes.size

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
