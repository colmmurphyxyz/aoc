fun main() {
    fun part1(input: List<String>): Int {
        var sum = 0
        for (line in input) {
            val filtered = line.filter { it.isDigit() }
            sum += (filtered.first().digitToInt() * 10) + filtered.last().digitToInt()
        }
        return sum
    }

    fun part2(input: List<String>): Int {
        val sanitisedInput = input.toMutableList()
        for (i in sanitisedInput.indices) {
            val line = sanitisedInput[i]
            sanitisedInput[i] = line.replace(Regex("(?:one|two|three|four|five|six|seven|eight|nine)")) { it: MatchResult ->
                (listOf("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
                    .indexOf(it.value) + 1).toString()
            }
//            println(sanitisedInput[i])
        }
        return part1(sanitisedInput)
    }

    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day01_test")
    val input = readInput("Day01")

//    println("Part 1 Test: ${part1(testInput)}")
//    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")

}
