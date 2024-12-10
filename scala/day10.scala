//> using toolkit 0.6.0

import scala.collection.immutable

case class Pos(x: Int, y: Int):
    def +(dir: Dir): Pos =
        Pos(x + dir.dx, y + dir.dy)

case class Dir(dx: Int, dy: Int)

object Dir:
    def up: Dir = Dir(0, -1)
    def down: Dir = Dir(0, 1)
    def left: Dir = Dir(-1, 0)
    def right: Dir = Dir(1, 0)
    def orthogonal: Vector[Dir] = Vector(up, down, left, right)

class Matrix[T](val data: Vector[Vector[T]]):
    def apply(pos: Pos): T =
        data(pos.y)(pos.x)

    override def toString(): String =
        data.map(_.mkString).mkString("\n")

object Day10:
    val path = os.pwd / ".." / "input" / "day10" / "example2.txt"
    var input = os.read.lines(path).map(_.toVector.map(_.toInt - '0')).toVector
    input = Vector.fill(input.head.length)(-1) +: input :+ Vector.fill(input.head.length)(-1)
    input = input.map(row => -1 +: row :+ -1)

    val map: Matrix[Int] = Matrix(input)
    val trailheads: Vector[Pos] =
        map
        .data
        .zipWithIndex
        .filter((row, _) => row.contains(0))
        .flatMap((row, y) =>
            row
            .zipWithIndex
            .filter((n, _) => n == 0)
            .map((_, x) => Pos(x, y))
        )

    def trailheadScore(trailhead: Pos): Int =
        var queue = immutable.Queue[Pos](trailhead)
        var found = immutable.Set[Pos]()

        while queue.nonEmpty do
            val (pos, newQueue) = queue.dequeue
            queue = newQueue

            for dir <- Dir.orthogonal do
                if map(pos + dir) == map(pos) + 1 then
                    if map(pos + dir) == 9 then
                        found = found.incl(pos + dir)
                    else
                        queue = queue.enqueue(pos + dir)

        found.size

    def trailheadRating(pos: Pos, rating: Int): Int =
        Dir
        .orthogonal
        .filter(dir => map(pos + dir) == map(pos) + 1)
        .map(dir => if map(pos + dir) == 9 then 1 else trailheadRating(pos + dir, rating))
        .sum + rating

    def partOne(): Int =
        trailheads
        .map(trailhead => trailheadScore(trailhead))
        .sum

    def partTwo(): Int =
        trailheads
        .map(trailhead => trailheadRating(trailhead, 0))
        .sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
