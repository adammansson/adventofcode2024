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
    val map = Matrix(input)
    val start: Complex = map.find('S')
    val end: Complex = map.find('E')

    val bests: Map[Complex, Map[Complex, Long]] =
        val currentBests = mutable.Map[Complex, mutable.Map[Complex, Long]]()
        val queue = mutable.Queue[(Complex, Complex, Long)]((start, Complex(1, 0), 0))
        while queue.nonEmpty do
            val (pos, dir, points) = queue.dequeue

            if !currentBests.contains(pos) then
                currentBests(pos) = mutable.Map[Complex, Long]()

            if currentBests(pos).getOrElse(dir, Long.MaxValue) > points then
                currentBests(pos) += dir -> points
                if map(pos + dir) != '#' then
                    queue.enqueue((pos + dir, dir, points + 1))
                queue.enqueue((pos, dir * Complex(0, 1), points + 1000))
                queue.enqueue((pos, dir * Complex(0, -1), points + 1000))

        currentBests.map((k, v) => k -> v.toMap).toMap

    def partOne(): Long =
        bests(end).minBy(_._2)._2

    def bestdir(pos: Complex): Complex =
        bests(pos).minBy(_._2)._1

    def partTwo(): Long =
        val bestpath = mutable.Set[Complex]()
        val backtrack = mutable.Queue[(Complex, Complex)]((end, bestdir(end)))
        while backtrack.nonEmpty do
            val (pos, prevdir) = backtrack.dequeue
            bestpath.add(pos)
            if pos != start then
                val dir = bestdir(pos) * Complex(-1, 0)
                backtrack.enqueue((pos + dir, dir))

                if dir != prevdir && map(pos + prevdir) != '#' &&
                   bests(pos + dir)(dir * Complex(-1, 0)) + 1000 == bests(pos + prevdir)(prevdir * Complex(-1, 0)) then
                    backtrack.enqueue((pos + prevdir, prevdir))

        bestpath.size

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
