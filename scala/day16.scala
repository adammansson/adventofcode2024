//> using toolkit 0.6.0

import scala.collection.mutable

object Day16:
    case class Complex(re: Int, im: Int):
        def +(other: Complex): Complex =
            Complex(re + other.re, im + other.im)

        def *(other: Complex): Complex =
            Complex(re * other.re - im * other.im,
                    re * other.im + im * other.re)

    class Matrix[T](val data: Vector[Vector[T]]):
        def apply(c: Complex): T =
            data(c.im)(c.re)

        def find(e: T): Complex =
            Complex(data(data.indexWhere(_.contains(e))).indexOf(e),
                data.indexWhere(_.contains(e)))

        override def toString(): String =
            data.map(_.mkString).mkString("\n")

    val path = os.pwd / ".." / "input" / "day16" / "input1.txt"
    val input: Vector[Vector[Char]] = os.read.lines(path).map(_.toVector).toVector
    val dirs = Seq(Complex(1, 0), Complex(-1, 0), Complex(0, 1), Complex(0, -1))
    val map = Matrix(input)
    val start: Complex = map.find('S')
    val end: Complex = map.find('E')

    val seen: Map[(Complex, Complex), Long] =
        val currentlySeen = mutable.Map[(Complex, Complex), Long]()
        val queue = mutable.Queue[(Complex, Complex, Long)]((start, Complex(1, 0), 0))
        while queue.nonEmpty do
            val (pos, dir, points) = queue.dequeue
            if currentlySeen.getOrElse((pos, dir), Long.MaxValue) > points then
                currentlySeen((pos, dir)) = points
                if map(pos + dir) != '#' then
                    queue.enqueue((pos + dir, dir, points + 1))
                queue.enqueue((pos, dir * Complex(0, 1), points + 1000))
                queue.enqueue((pos, dir * Complex(0, -1), points + 1000))

        currentlySeen.toMap

    def bestdir(pos: Complex): Complex =
        val min = dirs.map(dir => seen(pos, dir)).min
        dirs.find(dir => seen(pos, dir) == min).get

    def partOne(): Long =
        seen((end, bestdir(end)))

    def partTwo(): Long =
        val bestpath = mutable.Set[Complex]()
        val backtrack = mutable.Queue[(Complex, Complex)]((end, bestdir(end)))
        while backtrack.nonEmpty do
            val (pos, prevdir) = backtrack.dequeue
            bestpath.add(pos)
            if pos != start then
                val dir = bestdir(pos) * Complex(-1, 0)
                backtrack.enqueue((pos + dir, dir))

                if dir != prevdir &&
                   map(pos + prevdir) != '#' &&
                   seen(pos + dir, dir * Complex(-1, 0)) + 1000 == seen(pos + prevdir, prevdir * Complex(-1, 0)) then
                    backtrack.enqueue((pos + prevdir, prevdir))

        bestpath.size

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
