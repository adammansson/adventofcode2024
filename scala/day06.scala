//> using toolkit 0.6.0

object Day06:
    val path = os.pwd / ".." / "input" / "day06" / "input1.txt"
    val input = os.read.lines(path).map(_.toVector).toVector

    def turnRight(dir: (Int, Int)): (Int, Int) =
        if dir == (0, -1) then (1, 0)
        else if dir == (1, 0) then (0, 1)
        else if dir == (0, 1) then (-1, 0)
        else (0, -1)

    def partOne(): Int =
        var map = input
        var paddingRow = Vector.fill(map(0).length)('!')
        map = paddingRow +: map :+ paddingRow
        map = map.map(row => '!' +: row :+ '!')

        val startingY = map.indexWhere(_.contains('^'))
        val startingX = map(startingY).indexOf('^')

        var pos = (startingX, startingY)
        map = map.updated((pos._2), map(pos._2).updated((pos._1), 'X'))
        var dir = (0, -1)

        val visited = scala.collection.mutable.Map[(Int, Int), Set[(Int, Int)]]()

        var running = true
        while running do
            visited(pos) = visited.getOrElse(pos, Set()) + dir

            map(pos._2 + dir._2)(pos._1 + dir._1) match
                case '!' =>
                    running = false
                case '#' =>
                    dir = turnRight(dir)
                case '.' =>
                    pos = (pos._1 + dir._1, pos._2 + dir._2)
                case _ =>

        visited.size

    def foundLoop(map: Vector[Vector[Char]], startPos: (Int, Int), startDir: (Int, Int)): Boolean =
        var pos = startPos
        var dir = startDir
        val visited = scala.collection.mutable.Map[(Int, Int), Set[(Int, Int)]]()

        var isLoop = false
        var running = true
        while running do
            if visited.getOrElse(pos, Set()).contains(dir) then
                isLoop = true
                running = false
            else
                visited(pos) = visited.getOrElse(pos, Set()) + dir

                map(pos._2 + dir._2)(pos._1 + dir._1) match
                    case '!' =>
                        running = false
                    case '#' =>
                        dir = turnRight(dir)
                    case '.' =>
                        pos = (pos._1 + dir._1, pos._2 + dir._2)

        isLoop

    def partTwo(): Int = 
        var map = input
        var paddingRow = Vector.fill(map(0).length)('!')
        map = paddingRow +: map :+ paddingRow
        map = map.map(row => '!' +: row :+ '!')

        val startingY = map.indexWhere(_.contains('^'))
        val startingX = map(startingY).indexOf('^')

        val startPos = (startingX, startingY)
        map = map.updated((startPos._2), map(startPos._2).updated((startPos._1), '.'))
        val startDir = (0, -1)

        var counter = 0
        for y <- map.indices.drop(1).dropRight(1)
            x <- map.head.indices.drop(1).dropRight(1)
            pos = (x, y) do
            val loopMap = map.updated((pos._2), map(pos._2).updated((pos._1), '#'))

            if pos != startPos && foundLoop(loopMap, startPos, startDir) then
                counter += 1
        counter

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
