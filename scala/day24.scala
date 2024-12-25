//> using toolkit 0.6.0

import scala.util.matching.Regex
import scala.collection.mutable

object Day24:
    case class Gate(kind: String, lhs: String, rhs: String, out: String)

    val path: os.Path = os.pwd / ".." / "input" / "day24" / "input1.txt"
    val input: String = os.read(path)

    val wirePattern: Regex = raw"([a-z0-9]+): ([0-9]+)".r
    val gatePattern: Regex = raw"([a-z0-9]+) (AND|OR|XOR) ([a-z0-9]+) -> ([a-z0-9]+)".r

    val startWires: Map[String, Int] =
        wirePattern
        .findAllMatchIn(input)
        .map(m => m.group(1) -> m.group(2).toInt)
        .toMap

    val startGates: Vector[Gate] =
        gatePattern
        .findAllMatchIn(input)
        .map(m => Gate(m.group(2), m.group(1), m.group(3), m.group(4)))
        .toVector

    def gateOutput(kind: String, lhsValue: Int, rhsValue: Int): Int =
        kind match
            case "AND" => if lhsValue == 1 && rhsValue == 1 then 1 else 0
            case "OR" => if lhsValue == 1 || rhsValue == 1 then 1 else 0
            case "XOR" => if lhsValue != rhsValue then 1 else 0

    def partOne(): Long =
        val wires: mutable.Map[String, Int] = mutable.Map[String, Int]()
        wires ++= startWires
        val gates: Vector[Gate] = startGates

        val zs: Vector[String] = gates.map(_.out).filter(_.startsWith("z"))

        while !zs.forall(z => wires.contains(z)) do
            for gate <- gates do
                (wires.get(gate.lhs), wires.get(gate.rhs)) match
                    case (Some(lhsValue), Some(rhsValue)) => 
                        wires += gate.out -> gateOutput(gate.kind, lhsValue, rhsValue)
                    case _ =>

        wires
            .toVector
            .filter{ case (wire, _) => wire.startsWith("z") }
            .sortBy{ case (wire, _) => wire }
            .map{ case (_, out) => out }
            .zipWithIndex
            .foldLeft(0L){ case (acc, (out, i)) => acc + out * math.pow(2, i).toLong }

    @main def run(): Unit =
        println(partOne())
