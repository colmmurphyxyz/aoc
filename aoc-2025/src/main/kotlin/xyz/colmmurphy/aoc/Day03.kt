package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.readInput

private fun parseInput(input: List<String>): List<List<Long>> {
    val parse: (String) -> List<Long> = {
        it.toCharArray().map { it.digitToInt().toLong() }
    }
    return input.map(parse)
}

private fun largest2DigitNumber(nums: List<Long>): Long {
    var current = 0L
    var tens = -1L

    for (i in 0 until nums.lastIndex) {
        if (nums[i] <= tens) {
            continue
        }
        var candidate = nums[i]
        for (j in i + 1 until nums.size) {
            if (candidate * 10L + nums[j] > current) {
                current = candidate * 10 + nums[j]
                tens = candidate
            }
        }
    }
    return current
}

private fun part1(input: List<String>): Long {
    val parsed = parseInput(input)
    val nums = parsed.map { largest2DigitNumber(it) }
    return nums.sum()
}

private fun largest12DigitNumber(bank: List<Long>): Long {
    val n = bank.size
    var pos = 0
    val batteries = mutableListOf<Long>()
    for (remaining in 12 downTo 1) {
        val end = n - remaining
        val searchRange = pos..end
        val bestDigit = bank.slice(searchRange).max()
        val bestIndex = bank.slice(searchRange).indexOf(bestDigit) + pos
        pos = bestIndex + 1
        batteries.add(bestDigit)
    }
    return batteries.reduce { acc, next -> 10 * acc + next }
}

private fun part2(input: List<String>): Long {
    return parseInput(input).sumOf { largest12DigitNumber(it) }
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day03_test")
    val input = readInput("Day03")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}