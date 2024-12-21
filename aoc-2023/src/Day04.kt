fun main() {
    fun part1(input: List<String>): Int {
        var sum = 0
        val trimmed: List<List<String>> = input.map { it -> it.substringAfter(":").trim().split("\\s".toRegex()) }
        for (card in trimmed) {
//            println(card.joinToString("+"))
            val pipeIndex = card.indexOf("|")
            val winningNumbers = card.slice(0 until pipeIndex).filter { it != ""}.map { it -> it.toInt() }.toSet()
//            println(winningNumbers.joinToString("#"))
            val cardNumbers = card.slice(pipeIndex + 1 .. card.lastIndex).filter { it != ""}.map { it.toInt() }.toSet()
//            println(winningNumbers.joinToString("~"))
            val numbers = (winningNumbers intersect cardNumbers).size
            sum += 1 shl (numbers - 1)
        }
        return sum
    }

    fun part2(input: List<String>): Int {
        val numberOfCards = IntArray(input.size) { _ -> 1}
        val trimmed: List<List<String>> = input.map { it -> it.substringAfter(":").trim().split("\\s".toRegex()) }
        trimmed.forEachIndexed { cardNumber, card ->
            val pipeIndex = card.indexOf("|")
            val winningNumbers = card.slice(0 until pipeIndex).filter { it != ""}.map { it -> it.toInt() }.toSet()
            val cardNumbers = card.slice(pipeIndex + 1 .. card.lastIndex).filter { it != ""}.map { it.toInt() }.toSet()
            val n = (winningNumbers intersect cardNumbers).size
            repeat (numberOfCards[cardNumber]) {
                for (i in 1..n) {
                    if (i == numberOfCards.size) break
                    numberOfCards[cardNumber + i]++
                }
            }
        }
//        println(numberOfCards.joinToString())
        return numberOfCards.sum()
    }

    val testInput = readInput("Day04_test")
    val input = readInput("Day04")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}