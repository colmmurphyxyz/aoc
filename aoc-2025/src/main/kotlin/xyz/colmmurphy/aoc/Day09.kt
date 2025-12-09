package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.commaSeparatedNums
import xyz.colmmurphy.aoc.util.readInput
import kotlin.math.abs
import kotlin.system.measureTimeMillis

private fun part1(input: List<String>): Long {
    val points = input
        .map(String::commaSeparatedNums)
        .map { it.map(Int::toLong) }
        .map { it[0] to it[1] }
    var largestArea = Long.MIN_VALUE
    for (i in points.indices) {
        val pa = points[i]
        for (j in i + 1 .. points.lastIndex) {
            val pb = points[j]
            val area = (abs(pa.first - pb.first) + 1) * (abs(pa.second - pb.second) + 1)
            if (area > largestArea) {
                largestArea = area
            }
        }
    }

    return largestArea
}

private fun part2(input: List<String>): Long {

    return -1L
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day09_test")
    val input = readInput("Day09")

    measureTimeMillis {
        println("Part 1 Test: ${part1(testInput)}")
        println("Part 1: ${part1(input)}")

        println("Part 2 Test: ${part2(testInput)}")
        println("Part 2: ${part2(input)}")
    }.let {
        println("Took $it ms")
    }
}