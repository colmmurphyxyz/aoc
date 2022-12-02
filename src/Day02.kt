fun main() {

    val pointsOnPlay: (Char) -> Int = { c: Char ->
        if (c.code !in 88..90) {
            throw IllegalArgumentException("input must be a character in (X, Y, Z)")
        }
        c.code - 87
    }

    fun part1(): Int {
        val input = readInput("Day02")

        val keyLosesToVal = mapOf<Char, Char>(
            'A' to 'Y',
            'B' to 'Z',
            'C' to 'X'
        )

        var score = 0
        // for each 'round', simulate the result
        for (round in input) {
            val opponentChoice = round[0]
            val playerChoice = round[2]
            if (keyLosesToVal[opponentChoice] == playerChoice) {        // player wins
                score += 6
            } else if (opponentChoice.code + 23 == playerChoice.code) { // draw
                score += 3
            } // else, player loses
            score += pointsOnPlay(playerChoice)
        }

        return score
    }

    fun part2(): Int {
        val input = readInput("Day02")
        var score = 0
        for (round in input) {
            val opponentChoice = round[0]
            val neededOutcome = round[2]
            when (neededOutcome) {
                /*
                rock -> 1
                paper -> 2
                scissors -> 3
                 */
                'X' -> { // must lose
                    var s = (opponentChoice.code - 64) + 2
                    if (s > 3) s %= 3
                    score += s
                    println("need to lose, opponent chose $opponentChoice, picking $s")
                }
                'Y' -> { // must draw
                    score += 3
                    score += opponentChoice.code - 64
                    println("need to draw, opponent chose $opponentChoice, picking ${opponentChoice.code - 64}")
                }
                'Z' -> { // must win
                    score += 6
                    var s = (((opponentChoice.code - 64) + 1))
                    if (s > 3) s -= 3
                    score += s
                    println("need to win, opponent chose $opponentChoice, picking $s")
                }
                else -> throw IllegalArgumentException()
            }
        }

        return score
    }

    println("Part 2 answer: ${part2()}")
    println("Part 1 answer: ${part1()}")
}