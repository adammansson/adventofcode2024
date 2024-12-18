//> using toolkit 0.6.0

import scala.collection.mutable

object Day18:
    case class Complex(re: Int, im: Int):
        def +(other: Complex): Complex =
            Complex(re + other.re, im + other.im)

        def *(other: Complex): Complex =
            Complex(re * other.re - im * other.im,
                    re * other.im + im * other.re)

    val path = os.pwd / ".." / "input" / "day18" / "input1.txt"
    val input: Vector[String] = os.read.lines(path).toVector

    val start = Complex(0, 0)
    val end = Complex(70, 70)
    val positions: Vector[Complex] = input.map(_.split(",") match
        case Array(re, im) => Complex(re.toInt, im.toInt)
    )

    def path(map: Array[Array[Char]], start: Complex, end: Complex): Map[Complex, Complex] =
        val previous = mutable.Map[Complex, Complex]()
        val queue = mutable.Queue[Complex](start)

        while queue.nonEmpty do
            val pos = queue.dequeue

            if pos != end then
                Seq(Complex(1, 0), Complex(-1, 0), Complex(0, 1), Complex(0, -1)).foreach(dir =>
                    val newpos = pos + dir

                    if map.indices.contains(newpos.im) && map.head.indices.contains(newpos.re) &&
                       map(newpos.im)(newpos.re) != '#' && !previous.contains(newpos) then
                        queue.enqueue(newpos)
                        previous(newpos) = pos
                )

        previous.toMap

    def partOne(): Int =
        val map: Array[Array[Char]] = Array.fill(end.im + 1, end.re + 1)('.')

        positions.take(1024).foreach(pos =>
            map(pos.im)(pos.re) = '#'
        )
        
        val previous = path(map, start, end)

        var steps = 0
        var pos = end
        while pos != start do
            pos = previous(pos)
            steps += 1

        steps

    def partTwo(): Complex =
        val map: Array[Array[Char]] = Array.fill(end.im + 1, end.re + 1)('.')

        positions.dropWhile(pos =>
            map(pos.im)(pos.re) = '#'
            path(map, start, end).contains(end)
        ).head

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
