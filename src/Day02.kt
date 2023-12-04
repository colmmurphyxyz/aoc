import kotlin.math.max

fun main() {
    fun part1(input: List<String>): Int {
        // 12 red 13 green 14 blue
        val bagContents = listOf(12, 13, 14)
        var result = 0
        input.forEachIndexed { index, line ->
            val game = line.substringAfter(": ").split("; ")
            for (round in game) {
                val s = round.split(" ")
                var i = 0
                while (i < s.lastIndex) {
                    val num = s[i].toInt()
                    val color = s[i + 1]
                    val colorIndex = when (color) {
                        "red" -> 0
                        "red," -> 0
                        "green" -> 1
                        "green," -> 1
                        else -> 2
                    }
                    if (num > bagContents[colorIndex]) {
                        return@forEachIndexed
                    }
                    i += 2
                }
            }
            result += index + 1
        }
        return result
    }

    fun power(vararg elements: Int): Int {
        var result = 1
        elements.forEach { result *= it }
        return result
    }

    fun part2(input: List<String>): Int {
        var result = 0
        input.forEachIndexed { index, line ->
            val minimums = intArrayOf(0, 0, 0)
            val game = line.substringAfter(": ").split("; ")
            for (round in game) {
                val s = round.split(" ")
                var i = 0
                while (i < s.lastIndex) {
                    val num = s[i].toInt()
                    val color = s[i + 1]
                    val colorIndex = when (color) {
                        "red" -> 0
                        "red," -> 0
                        "green" -> 1
                        "green," -> 1
                        else -> 2
                    }
                    minimums[colorIndex] = max(minimums[colorIndex], num)
                    i += 2
                }
            }
            result += power(*minimums)
        }
        return result
    }

    val testInput = readInput("Day02_test")
    val input = readInput("Day02")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}