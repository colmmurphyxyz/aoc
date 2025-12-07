package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.readInput

private  fun parseInput(input: String): List<LongRange> = input.split(",").map {
    val (a, b) = it.split("-")
    a.toLong()..b.toLong()
}

private fun findInvalidIds(range: LongRange): List<Long> {
    return range.filter { Regex("^(\\d+)\\1$").matches(it.toString()) }
}

private fun part1(input: String): Long {
    return parseInput((input)).map { findInvalidIds(it) }.sumOf { it.sum() }
}

private fun findInvalidIds2(range: LongRange): List<Long> {
    return range.filter { Regex("^(\\d+)\\1+$").matches(it.toString()) }
}

private fun part2(input: String): Long {
    return parseInput((input)).map { findInvalidIds2(it) }.sumOf { it.sum() }
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day02_test")[0]
    val input = readInput("Day02")[0]

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}