//> using toolkit 0.6.0

object Day05:
    val path = os.pwd / ".." / "input" / "day05" / "input1.txt"
    val input = os.read.lines(path).toVector
    val sections = input.splitAt(input.indexOf(""))

    val rules =
        sections(0)
        .map(_.split("\\|").map(_.toInt).toVector)

    val updates =
        sections(1)
        .filter(_.nonEmpty)
        .map(_.split(",").map(_.toInt).toVector)

    val correctlyOrderedUpdates: Vector[Vector[Int]] =
        updates.filter(update => 
            update.zipWithIndex.forall((number, i) => 
                val shouldBeBefore = rules.filter(_(0) == number).map(_(1)).toSet
                val shouldBeAfter = rules.filter(_(1) == number).map(_(0)).toSet
                val before = update.drop(i)
                val after = update.take(i)

                !(before.exists(b => shouldBeAfter.contains(b)) ||
                  after.exists(a => shouldBeBefore(a)))
            )
        )

    def partOne(): Int =
        correctlyOrderedUpdates
        .map(xs => xs(xs.length / 2))
        .sum

    def partTwo(): Int =
        val incorrectlyOrderedUpdates =
            updates.filter(update => !correctlyOrderedUpdates.contains(update))

        incorrectlyOrderedUpdates
        .map(update => 
            var correct = Vector[Int]()
            var remaining = update

            while correct.length != update.length do
                for number <- remaining do
                    val shouldBeAfter =
                        rules
                        .filter(_(1) == number)
                        .map(_(0))
                        .filterNot(number => correct.contains(number))
                        .toSet

                    if (shouldBeAfter.intersect(remaining.toSet).isEmpty) then
                        correct = correct :+ number

                remaining = remaining.filterNot(number => correct.contains(number))

            correct
        ).map(xs => xs(xs.length / 2)).sum

    @main def run(): Unit =
        println(partOne())
        println(partTwo())
