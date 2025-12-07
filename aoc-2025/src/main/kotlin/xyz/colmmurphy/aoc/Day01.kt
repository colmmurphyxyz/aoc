package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.readInput

private fun toAbsolutes(input: List<String>): List<Int> {
    return input.map {
        val dir = it[0]
        val amount = it.drop(1).toInt()
        when (dir) {
            'L' -> -amount
            else -> amount
        }
    }
}

private fun part1(input: List<String>): Int {
    val nums = toAbsolutes(input).scan(50) { acc, next -> acc + next }
    return nums.filter { it % 100 == 0 }.size
}

private fun part2(input: List<String>): Int {
    val nums = toAbsolutes(input)
    var curr = 50
    var zeros = 0
    for (num in nums) {
        val next = curr + num
        val range = if (next < curr) next..<curr else (curr + 1) .. next
        for (i in range) {
            if (i % 100 == 0) {
                zeros++
            }
        }
        curr = next
    }
    return zeros
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day01_test")
    val input = readInput("Day01")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}