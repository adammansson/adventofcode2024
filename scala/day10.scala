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
    val path = os.pwd / ".." / "input" / "day10" / "input1.txt"
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

    def trailheadScore(pos: Pos, found: Set[Pos]): Set[Pos] =
        found.concat(
            Dir
            .orthogonal
            .filter(dir => map(pos + dir) == map(pos) + 1)
            .flatMap(dir => if map(pos + dir) == 9 then Set(pos + dir) else trailheadScore(pos + dir, found))
        )

    def trailheadRating(pos: Pos, rating: Int): Int =
        rating + 
            Dir
            .orthogonal
            .filter(dir => map(pos + dir) == map(pos) + 1)
            .map(dir => if map(pos + dir) == 9 then 1 else trailheadRating(pos + dir, rating))
            .sum

    def partOne(): Int =
        trailheads
        .flatMap(trailhead => trailheadScore(trailhead, Set()))
        .size

    def partTwo(): Int =
        trailheads
        .map(trailhead => trailheadRating(trailhead, 0))
        .sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
