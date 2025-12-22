package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.Grid
import xyz.colmmurphy.aoc.util.ImmutableGrid
import xyz.colmmurphy.aoc.util.readInput
import xyz.colmmurphy.aoc.util.splitOn
import kotlin.system.measureTimeMillis

private data class Board(val width: Int, val height: Int, val quantities: IntArray) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Board

        if (width != other.width) return false
        if (height != other.height) return false
        if (!quantities.contentEquals(other.quantities)) return false

        return true
    }

    override fun hashCode(): Int {
        var result = width
        result = 31 * result + height
        result = 31 * result + quantities.contentHashCode()
        return result
    }
}

private fun parsePiece(lines: List<String>): ImmutableGrid<Boolean> {
    val chars = lines.drop(1).map { it.map { char -> char == '#' } }
    return ImmutableGrid<Boolean>(chars)
}

private fun parseBoard(line: String): Board {
    val lhs = line.substringBefore(": ")
    val (width, height) = lhs.split("x")
    val quantities = line.substringAfter(": ").split(' ').map(String::toInt)
    return Board(width.toInt(), height.toInt(), quantities.toIntArray() )
}

private fun parseInput(input: List<String>): Pair<List<ImmutableGrid<Boolean>>, List<Board>> {
    val piecesRaw = input.take(29)
    val boardsRaw = input.drop(30)

    val pieces = splitOn(piecesRaw, "").map(::parsePiece)
    val boards = boardsRaw.map(::parseBoard)

    return pieces to boards
}

private fun <T>boundingRectangleArea(piece: Grid<T>): Int {
    return piece.width * piece.height
}

private fun isTriviallySolvable(board: Board, pieces: List<ImmutableGrid<Boolean>>): Boolean {
    val boundingAreas = pieces.map(::boundingRectangleArea)
    val availableArea = board.width * board.height
    val maximalArea = board.quantities.withIndex().sumOf { q -> boundingAreas[q.index] * q.value }
    return maximalArea < availableArea
}

private fun isTriviallyUnsolvable(board: Board, pieces: List<ImmutableGrid<Boolean>>): Boolean {
    val pieceArea = pieces.map { grid -> grid.getData().flatten().count { it == true }}
    val availableArea = board.width * board.height
    val maximalArea = board.quantities.withIndex().sumOf { q -> pieceArea[q.index] * q.value }
    return maximalArea > availableArea
}

private fun part1(input: List<String>): Int {
    val (pieces, boards) = parseInput(input)
    // hack: Don't even try to solve. Check if a given board can be
    // trivially unsolvable (the sum of areas of all shaded squares of required pieces is greater than the available area)
    return boards.filterNot { isTriviallyUnsolvable(it, pieces) }.size
}

fun main() {
    val testInput = readInput("Day12_test")
    val input = readInput("Day12")

    measureTimeMillis {
        println("Part 1 Test: ${part1(testInput)}")
        println("Part 1: ${part1(input)}")
    }.let {
        println("Took $it ms")
    }
}