package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.Grid
import xyz.colmmurphy.aoc.util.readInput


private enum class Operation(val apply: (Long, Long) -> Long) {
    ADDITION({ a, b -> a + b }),
    MULTIPLICATION({ a, b -> a * b })
}

private data class Equation(
    val operands: List<Long>,
    val operator: Operation
)

private fun evaluateEquation(equation: Equation): Long {
    return when (equation.operator) {
        Operation.MULTIPLICATION -> equation.operands.foldRight(1, Operation.MULTIPLICATION.apply)
        Operation.ADDITION -> equation.operands.foldRight(0, Operation.ADDITION.apply)
    }
}

private fun part1(input: List<String>): Long {
    val lines = input.map { it.split(Regex("\\s+")).filter(String::isNotBlank) }
    val numOperands = lines.size - 1
    val total = lines[0].size
    val equations = mutableListOf<Equation>()
    for (i in 0 until total) {
        val operands = lines.slice(0 until numOperands).map { it[i] }.map(String::toLong)
        val operation = when (lines.last()[i]) {
            "+" -> Operation.ADDITION
            else -> Operation.MULTIPLICATION
        }
        equations.add(Equation(operands, operation))
    }

    return equations.sumOf(::evaluateEquation)
}

private fun part2(input: List<String>): Long {
    // pad each line with whitespace to be the same length
    val maxLen = input.maxOf(String::length)
    val worksheet = input.toMutableList()
    for (i in worksheet.indices) {
        while (worksheet[i].length < maxLen) {
            worksheet[i] += " "
        }
    }
    // search for indices 'i' where all line[i] is a space character
    var breaks = mutableListOf<Int>(-1)
    for (i in 0 until maxLen) {
        if (worksheet.all { it[i].isWhitespace() }) {
            breaks.add(i)
        }
    }
    breaks.add(maxLen)

    var columnRanges = mutableListOf<IntRange>()
    for (i in 0 until breaks.lastIndex) {
        columnRanges.add(breaks[i] + 1 until breaks[i + 1])
    }

    val worksheetProblems = columnRanges.map { range ->
        Grid(worksheet.map { it.slice(range).toCharArray().toList() })
    }

    var ans = 0L
    for (grid in worksheetProblems) {
        val operator = if (grid.getRow(grid.height - 1).contains('+')) Operation.ADDITION else Operation.MULTIPLICATION
        var operands = mutableListOf<Long>()
        for (i in grid.width - 1 downTo 0) {
            operands.add(
                grid
                    .getCol(i)
                    .dropLast(1)
                    .filterNot(Char::isWhitespace)
                    .fold(0L) { acc, next ->
                        (10 * acc) + next.digitToInt().toLong()
                    }
            )
        }
        ans += evaluateEquation(Equation(operands, operator))
    }

    return ans
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day06_test")
    val input = readInput("Day06")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}