//> using toolkit 0.6.0

import scala.collection.mutable

object Day09:
    val path = os.pwd / ".." / "input" / "day09" / "input1.txt"
    val input = os.read.lines(path).mkString.toVector.filter(_.isDigit).map(_.toInt - '0')

    def partOne(): Long =
        val diskMap = input.toArray

        var leftId = 1
        var rightId = diskMap.length / 2
        var leftIndex = 1
        var rightIndex = diskMap.length - 1
        var position = diskMap.head
        var checksum = 0L

        while leftIndex < rightIndex do
            while diskMap(leftIndex) > 0 && diskMap(rightIndex) > 0 do
                checksum += rightId * position
                position += 1
                diskMap(leftIndex) -= 1
                diskMap(rightIndex) -= 1

            if diskMap(leftIndex) == 0 then
                leftIndex += 1

                while diskMap(leftIndex) > 0 do
                    checksum += leftId * position
                    position += 1
                    diskMap(leftIndex) -= 1

                leftId += 1
                leftIndex += 1

            if diskMap(rightIndex) == 0 then
                rightIndex -= 2
                rightId -= 1

        checksum

    def partTwo(): Long =
        val diskMap = input.toArray

        var id = diskMap.length / 2
        var rightIndex = diskMap.length - 1
        var checksum = 0L

        while rightIndex > 0 do
            var searching = true
            var leftIndex = 1

            while searching && leftIndex < rightIndex do
                if diskMap(leftIndex) >= diskMap(rightIndex) then
                    var position = input.take(leftIndex + 1).sum - diskMap(leftIndex)
                    while diskMap(rightIndex) > 0 do
                        checksum += id * position
                        position += 1
                        diskMap(leftIndex) -= 1
                        diskMap(rightIndex) -= 1
                    searching = false
                else
                    leftIndex += 2

            var position = input.take(rightIndex).sum
            while diskMap(rightIndex) > 0 do
                checksum += id * position
                position += 1
                diskMap(rightIndex) -= 1

            id -= 1
            rightIndex -= 2

        checksum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
