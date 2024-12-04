//> using toolkit 0.6.0

object Day04:
    val path = os.pwd / ".." / "input" / "day04" / "input1.txt"
    var input = os.read.lines(path).map(_.toVector).toVector

    val paddingAmount = 3
    val paddingRow = Vector.fill(input(0).length)('#')
    val paddingRows = Vector.fill(paddingAmount)(paddingRow)

    input = paddingRows ++ input ++ paddingRows
    input = input.map(row => "###".toVector ++ row ++ "###".toVector)
    val matrix = input

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

    @main def run(): Unit =
        partOne()
        partTwo()
