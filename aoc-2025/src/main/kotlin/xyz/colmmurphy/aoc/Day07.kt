package xyz.colmmurphy.aoc

import arrow.core.MemoizedDeepRecursiveFunction

import xyz.colmmurphy.aoc.util.Grid
import xyz.colmmurphy.aoc.util.ImmutableGrid
import xyz.colmmurphy.aoc.util.readInput

private fun part1(input: List<String>): Int {
    val grid = Grid.fromStringList(input)
    val start = grid.firstOccurrenceOf('S').getOrNull()!!

    val seen = mutableSetOf<Pair<Int, Int>>()
    fun tachyonTraversal(curr: Pair<Int, Int>): Int {
        if (!grid.isInGrid(curr)) {
            return 0
        }
        if (seen.contains(curr)) {
            return 0
        }

        seen.add(curr)
        return if (grid.get(curr) == '^') {
            1 + tachyonTraversal(curr.first to curr.second - 1) + tachyonTraversal(curr.first to curr.second + 1)
        } else {
            tachyonTraversal(curr.first + 1 to curr.second)
        }
    }

    val splits = tachyonTraversal(start.first + 1 to start.second)

    return splits
}

private fun part2(input: List<String>): Long {
    val grid = ImmutableGrid.fromStringList(input)
    val start = grid.firstOccurrenceOf('S').getOrNull()!!

    val countTimelines = MemoizedDeepRecursiveFunction<Pair<Int, Int>, Long> { curr ->
        if (curr.first == grid.height - 1) { // if we are on the last row
            return@MemoizedDeepRecursiveFunction 1L
        }

        if (grid.get(curr) == '^') {
            return@MemoizedDeepRecursiveFunction callRecursive(curr.first + 1 to curr.second - 1) + callRecursive(curr.first + 1 to curr.second + 1)
        } else {
            return@MemoizedDeepRecursiveFunction callRecursive(curr.first + 1 to curr.second)
        }
    }

    val timelines = countTimelines(start.first + 1 to start.second)

    return timelines
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day07_test")
    val input = readInput("Day07")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}