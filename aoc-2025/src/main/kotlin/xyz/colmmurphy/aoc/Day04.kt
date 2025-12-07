package xyz.colmmurphy.aoc

import arrow.core.Option
import xyz.colmmurphy.aoc.util.Grid
import xyz.colmmurphy.aoc.util.readInput

private fun allReachablePoints(grid: Grid<Char>): List<Pair<Int, Int>> {
    return grid.allPoints()
        .filter {
            grid.get(it) == '@' && grid.getAdjacent(it.first, it.second).mapNotNull(Option<Char>::getOrNull)
                .filter { it == '@' }.size < 4
        }
}

private fun part1(input: List<String>): Int {
    return allReachablePoints(
        Grid<Char>(input.map { it.toCharArray().toList() } )
    ).size
}

private fun part2(input: List<String>): Int {
    var removed = 0
    val grid = Grid(input.map { it.toCharArray().toList() })
    while (true) {
        val toRemove = allReachablePoints(grid)
        if (toRemove.size == 0) {
            return removed
        }
        removed += toRemove.size
        for (point in toRemove) {
            grid.set(point, 'X')
        }
    }
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day04_test")
    val input = readInput("Day04")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}