//> using toolkit 0.6.0

object Day04:
    val path = os.pwd / ".." / "input" / "day04" / "input1.txt"
    val input = os.read.lines(path).map(_.toVector).toVector
    var matrix = input

    val paddingAmount = 3
    val paddingRow = Vector.fill(matrix(0).length)('#')
    val paddingRows = Vector.fill(paddingAmount)(paddingRow)

    matrix = paddingRows ++ matrix ++ paddingRows
    matrix = matrix.map(row =>
        Vector.fill(paddingAmount)('#') ++ row ++ Vector.fill(paddingAmount)('#')
    )

    def partOne(): Unit =
        var counter = 0

        for row <- matrix.indices.drop(paddingAmount).dropRight(paddingAmount)
            col <- matrix(0).indices.drop(paddingAmount).dropRight(paddingAmount) do
                if matrix(row)(col) == 'X' then
                    for drow <- -1 to 1
                        dcol <- -1 to 1 do
                            if matrix(row + drow)(col + dcol) == 'M' &&
                               matrix(row + drow + drow)(col + dcol + dcol) == 'A' &&
                               matrix(row + drow + drow + drow)(col + dcol + dcol + dcol) == 'S' then
                                counter += 1
            
        println(counter)

    def partTwo(): Unit = 
        var counter = 0

        for row <- matrix.indices.drop(paddingAmount).dropRight(paddingAmount)
            col <- matrix(0).indices.drop(paddingAmount).dropRight(paddingAmount) do
                if matrix(row)(col) == 'A' then
                    val ul = matrix(row - 1)(col - 1)
                    val ur = matrix(row - 1)(col + 1)
                    val dl = matrix(row + 1)(col - 1)
                    val dr = matrix(row + 1)(col + 1)

                    if ((ul == 'M' && dr == 'S') || (ul == 'S' && dr == 'M')) &&
                       ((ur == 'M' && dl == 'S') || (ur == 'S' && dl == 'M')) then
                        counter += 1

        println(counter)
    

    def partOneAlternative(): Unit =
        def countXMAS(xs: Vector[Char]): Int =
            xs.mkString.sliding(4).count(window => window == "XMAS" || window == "SAMX")

        def countHorizontal(xs: Vector[Vector[Char]]): Int =
            xs.map(countXMAS).sum

        def countDiagonal(xs: Vector[Vector[Char]], padFn: (Vector[Char], Int) => Vector[Char]): Int =
            input.sliding(4).map(_.zipWithIndex.map((x, i) => padFn(x, i))).map(xs => countHorizontal(xs.transpose)).sum

        val result = 
            countHorizontal(input) +
            countHorizontal(input.transpose) +
            countDiagonal(input, (x, i) => x.drop(i) ++ Vector.fill(i)('#')) +
            countDiagonal(input, (x, i) => Vector.fill(i)('#') ++ x.dropRight(i))

        println(result)

    @main def run(): Unit =
        partOne()
        partTwo()
        partOneAlternative()
