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
    val input: Array[Array[Char]] = os.read.lines(path).map(_.toCharArray).toArray

    def partOne(): Long =
        val map = Matrix(input.map(_.toVector).toVector)
        val start: Complex = map.find('S')
        val end: Complex = map.find('E')

        val lowest = mutable.Map[(Complex, Complex), Long]()
        val queue = mutable.Queue[(Complex, Complex, Long)]((start, Complex(1, 0), 0))

        while queue.nonEmpty do
            val (pos, dir, points) = queue.dequeue
            if lowest.getOrElse((pos, dir), Long.MaxValue) > points then
                lowest((pos, dir)) = points
                if map(pos + dir) != '#' then
                    queue.enqueue((pos + dir, dir, points + 1))
                queue.enqueue((pos, dir * Complex(0, 1), points + 1000))
                queue.enqueue((pos, dir * Complex(0, -1), points + 1000))


        lowest.foldLeft(Long.MaxValue)((acc, kv) =>
            if kv._1._1 == end && acc > kv._2 then kv._2 else acc)

    @main def run(): Unit =
        println(partOne())
