//> using toolkit 0.6.0

import scala.collection.mutable

object Day10:
    val path = os.pwd / ".." / "input" / "day10" / "input1.txt"
    val input = os.read.lines(path).map(_.toVector.map(_.toInt - '0')).toVector
    val trailheads =
        input
        .zipWithIndex
        .filter((row, _) => row.contains(0))
        .flatMap((row, y) =>
            row
            .zipWithIndex
            .filter((n, _) => n == 0)
            .map((_, x) => (x, y))
        )

    def trailheadScore(map: Vector[Vector[Int]], reached: mutable.Set[(Int, Int)], x: Int, y: Int): Int =
        var counter = 0

        for tpl <- Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) do
            val nx = tpl._1
            val ny = tpl._2

            if map.indices.contains(ny) && map.head.indices.contains(nx) then
                if map(ny)(nx) == map(y)(x) + 1 then
                    if map(ny)(nx) == 9 && !reached.contains((nx, ny)) then
                        counter += 1
                        reached.add((nx, ny))
                    else
                        counter += trailheadScore(map, reached, nx, ny)
        counter

    def trailheadRating(map: Vector[Vector[Int]], x: Int, y: Int): Int =
        var counter = 0

        for tpl <- Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) do
            val nx = tpl._1
            val ny = tpl._2

            if map.indices.contains(ny) && map.head.indices.contains(nx) then
                if map(ny)(nx) == map(y)(x) + 1 then
                    if map(ny)(nx) == 9 then
                        counter += 1
                    else
                        counter += trailheadRating(map, nx, ny)
        counter

    def partOne(): Int =
        trailheads.map(th => 
            val reached = mutable.Set[(Int, Int)]()
            trailheadScore(input, reached, th._1, th._2)
        ).sum

    def partTwo(): Int =
        trailheads.map(th => 
            trailheadRating(input, th._1, th._2)
        ).sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
