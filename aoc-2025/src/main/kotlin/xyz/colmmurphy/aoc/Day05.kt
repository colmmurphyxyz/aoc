package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.readInput
import java.math.BigInteger
import kotlin.math.max

private fun part1(input: List<String>): Int {
    val blank = input.indexOfFirst(String::isEmpty)
    val fresh = input.slice(0 until blank)
        .map {
            val (lhs, rhs) = it.split("-")
            lhs.toLong() to rhs.toLong()
        }

    fun isFresh(id: Long): Boolean {
        for ((lhs, rhs) in fresh) {
            if (id >= lhs && id <= rhs) {
                return true
            }
        }

        return false
    }

    val ids = input.slice(blank + 1..input.lastIndex).map(String::toLong)

    return ids.count(::isFresh)
}

private fun part2(input: List<String>): BigInteger {
    val blank = input.indexOfFirst(String::isEmpty)
    val ranges = input.slice(0 until blank)
        .map {
            val (lhs, rhs) = it.split("-")
            lhs.toLong() to rhs.toLong()
        }
        .sortedWith { a, b ->
            val x = a.first - b.first
            if (x < 0) {
                -1
            } else if (x == 0L) {
                0
            } else {
                1
            }
        }.toMutableList()

    val merged = mutableListOf<Pair<Long, Long>>(ranges[0])
    for (i in 1 until ranges.size) {
        val last = merged.last()
        val curr = ranges[i]
        if (last.second >= curr.first) {
            val newFinish = max(last.second, curr.second)
            merged[merged.lastIndex] = last.first to newFinish
        } else {
            merged.add(curr)
        }
    }

    var ans = BigInteger.ZERO
    for ((a, b) in merged) {
        ans = ans.add(BigInteger.valueOf((b - a) + 1))
    }

    return ans
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day05_test")
    val input = readInput("Day05")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")
    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}