//> using toolkit 0.6.0

import scala.collection.mutable

object Day25:
    val path: os.Path = os.pwd / ".." / "input" / "day25" / "input1.txt"
    val input: String = os.read(path)

    val schematics: Vector[Vector[String]] =
        input
        .split("\n\n")
        .map(_.split("\n").toVector)
        .toVector

    val locks: Vector[Vector[Int]] =
        schematics
        .filter(schematic => schematic.head.forall(_ == '#'))
        .map(schematic => schematic.drop(1).dropRight(1).transpose.map(col => col.count(_ == '#')))

    val keys: Vector[Vector[Int]] =
        schematics
        .filter(schematic => schematic.head.forall(_ == '.'))
        .map(schematic => schematic.drop(1).dropRight(1).transpose.map(col => col.count(_ == '#')))

    def partOne(): Int =
        locks.foldLeft(0)((acc, lock) => 
            acc + keys.count(key => lock.indices.forall(i => lock(i) + key(i) <= lock.length)))

    @main def run(): Unit =
        println(partOne())
