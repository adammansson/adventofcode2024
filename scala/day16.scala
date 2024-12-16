//> using toolkit 0.6.0

import scala.collection.mutable

case class Pos(x: Int, y: Int):
    def +(dir: Dir): Pos =
        Pos(x + dir.dx, y + dir.dy)

enum Dir(val dx: Int, val dy: Int):
    case North extends Dir(0, -1)
    case South extends Dir(0, 1)
    case West extends Dir(-1, 0)
    case East extends Dir(1, 0)

    def clockwise: Dir =
        this match
            case North => West
            case South => East
            case West => South
            case East => North

    def counterClockwise: Dir =
        this match
            case North => East
            case South => West
            case West => North
            case East => South

    def opposite: Dir =
        this match
            case North => South
            case South => North
            case West => East
            case East => West

object Dir:
    def all: Vector[Dir] = Vector(North, South, West, East)

class Matrix[T](val data: Vector[Vector[T]]):
    def apply(pos: Pos): T =
        data(pos.y)(pos.x)

    def find(e: T): Pos =
        Pos(data(data.indexWhere(_.contains(e))).indexOf(e),
            data.indexWhere(_.contains(e)))

    override def toString(): String =
        data.map(_.mkString).mkString("\n")

object Day16:
    val path = os.pwd / ".." / "input" / "day16" / "input1.txt"
    val input: Array[Array[Char]] = os.read.lines(path).map(_.toCharArray).toArray

    def partOne(): Long =
        val map = Matrix(input.map(_.toVector).toVector)
        val start: Pos = map.find('S')
        val end: Pos = map.find('E')

        val lowest = mutable.Map[(Pos, Dir), Long]()
        val queue = mutable.Queue[(Pos, Dir, Long)]((start, Dir.East, 0))

        while queue.nonEmpty do
            val (pos, dir, points) = queue.dequeue
            if lowest.getOrElse((pos, dir), Long.MaxValue) > points then
                lowest((pos, dir)) = points
                if map(pos + dir) != '#' then
                    queue.enqueue((pos + dir, dir, points + 1))
                queue.enqueue((pos, dir.clockwise, points + 1000))
                queue.enqueue((pos, dir.counterClockwise, points + 1000))

        Dir.all.map(dir => lowest((end, dir))).min

    @main def run(): Unit =
        println(partOne())
