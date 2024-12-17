//> using toolkit 0.6.0

object Day15:
    case class Complex(re: Int, im: Int):
        def +(other: Complex): Complex =
            Complex(re + other.re, im + other.im)

        def *(other: Complex): Complex =
            Complex(re * other.re - im * other.im,
                    re * other.im + im * other.re)

    val path = os.pwd / ".." / "input" / "day15" / "input1.txt"
    val input: Vector[Vector[String]] =
        os.read.lines(path).mkString("\n").split("\n\n").map(_.split("\n").toVector).toVector

    val matrix: Vector[Vector[Char]] = input.head.map(_.toVector).toVector
    val moves: Vector[Complex] =
        input.last.mkString.map(c => c match
            case 'v' => Complex(0, 1)
            case '^' => Complex(0, -1)
            case '>' => Complex(1, 0)
            case '<' => Complex(-1, 0)).toVector

    def solve(matrix: Vector[Vector[Char]]): Long =
        val map: Array[Array[Char]] = matrix.map(_.toArray).toArray

        def canMove(pos: Complex, dir: Complex): Boolean =
            val newpos = pos + dir

            (map(newpos.im)(newpos.re) == '.') ||
            (map(newpos.im)(newpos.re) == 'O' && canMove(newpos, dir)) ||
            (dir.im == 0 && (map(newpos.im)(newpos.re) == '[' || map(newpos.im)(newpos.re) == ']') && canMove(newpos, dir)) ||
            (dir.im != 0 && map(newpos.im)(newpos.re) == '[' && canMove(newpos, dir) && canMove(newpos + Complex(1, 0), dir)) ||
            (dir.im != 0 && map(newpos.im)(newpos.re) == ']' && canMove(newpos, dir) && canMove(newpos + Complex(-1, 0), dir))

        def move(pos: Complex, dir: Complex): Unit =
            val newpos = pos + dir
            val curr = map(pos.im)(pos.re)
            val dest = map(newpos.im)(newpos.re)

            if dest == '.' then
                map(newpos.im)(newpos.re) = curr
                map(pos.im)(pos.re) = '.'
            else
                move(newpos, dir)
                if dir.im != 0 && dest == '[' then
                    move(newpos + Complex(1, 0), dir)
                if dir.im != 0 && dest == ']' then
                    move(newpos + Complex(-1, 0), dir)

                map(pos.im)(pos.re) = map(newpos.im)(newpos.re)
                map(newpos.im)(newpos.re) = curr

        var pos = Complex(map(map.indexWhere(_.contains('@'))).indexOf('@'),
                          map.indexWhere(_.contains('@')))

        moves.foreach(dir =>
            if canMove(pos, dir) then
                move(pos, dir)
                pos = pos + dir)

        map
        .zipWithIndex
        .flatMap((row, im) => row.zipWithIndex.map((c, re) => (c, Complex(re, im))))
        .foldLeft(0)((acc, e) => e match
            case ('O' | '[', c) => (c.re + c.im * 100) + acc
            case _ => acc)

    def partOne(): Long =
        solve(matrix)

    def partTwo(): Long =
        solve(matrix.map(_.flatMap(_ match
            case '#' => Array('#', '#')
            case 'O' => Array('[', ']')
            case '.' => Array('.', '.')
            case '@' => Array('@', '.'))))

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
