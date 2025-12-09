package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.commaSeparatedNums
import xyz.colmmurphy.aoc.util.readInput
import java.util.*
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.system.measureTimeMillis

private fun part1(input: List<String>): Int {
    val points = input
        .map(String::commaSeparatedNums)
        .map { it[0] to it[1] }
    var largestArea = Int.MIN_VALUE
    for (i in points.indices) {
        val pa = points[i]
        for (j in i + 1..points.lastIndex) {
            val pb = points[j]
            val area = (abs(pa.first - pb.first) + 1) * (abs(pa.second - pb.second) + 1)
            if (area > largestArea) {
                largestArea = area
            }
        }
    }

    return largestArea
}

private typealias Point = Pair<Long, Long>

private data class Rectangle(val a: Point, val b: Point) {
    val area = (abs(a.first - b.first) + 1) * (abs(a.second - b.second) + 1)
}

private fun isVerticalLine(a: Point, b: Point): Boolean {
    return a.first == b.first
}

fun rangesIntersect(x: LongRange, y: LongRange): Boolean {
    return x.first <= y.last && y.first <= x.last
}

private fun part2(input: List<String>): Long {
    val points = input
        .map(String::commaSeparatedNums)
        .map{it.map(Int::toLong)}
        .map { it[0] to it[1] }
        .let { it + it[0] } // append the first element to the back, simplifies code below

    fun polygonCrossesThrough(rect: Rectangle): Boolean {
        val a = rect.a
        val b = rect.b
        val topRightCorner =    max(a.first, b.first) to max(a.second, b.second)
        val bottomLeftCorner =  min(a.first, b.first) to min(a.second, b.second)

        for (i in 0 until points.lastIndex) {
            val pStart = points[i]
            val pEnd = points[i + 1]
            if (isVerticalLine(pStart, pEnd)) {
                // check if line crosses horizontal edges of rectangle
                if (pStart.first <= bottomLeftCorner.first || pStart.first >= topRightCorner.first) {
                    // if rectangle edges do not occupy same column as the vertical line
                    continue
                }
                val yValRange = min(pStart.second, pEnd.second) + 1..<max(pStart.second, pEnd.second)
                if (rangesIntersect(yValRange, bottomLeftCorner.second + 1 ..< topRightCorner.second)) {
                    return true
                }
            } else {
                // horizontal line segment
                // does horizontal line occupy same column as rectangle?
                if (pStart.second <= bottomLeftCorner.second || pStart.second >= topRightCorner.second) {
                    continue
                }
                val xValRange = min(pStart.first, pEnd.first) + 1 ..< max(pStart.first, pEnd.first)
                if (rangesIntersect(xValRange, bottomLeftCorner.first+1 ..< topRightCorner.first)) {
                    return true
                }
            }
        }
        return false
    }

    // calculate the area of all possible rectangles and store in a max-heap
    val numRectangles = ((points.size) * (points.size + 1)) / 2
    val pq = PriorityQueue<Rectangle>(numRectangles, Collections.reverseOrder { a, b ->
        // a.area - b.area
        val x = a.area - b.area
        if (x < 0) {
            return@reverseOrder -1
        } else if (x > 0) {
            return@reverseOrder 1
        } else {
            return@reverseOrder 0
        }
    })
    for (i in 0 until points.lastIndex) { // don't include the last point we added to simplify wrap-around
        val pa = points[i]
        for (j in i + 1..points.lastIndex) {
            val pb = points[j]
            pq.add(Rectangle(pa, pb))
        }
    }
    // starting with the largest possible rectangle, find the first rectangle that is valid under the conditions
    while (pq.isNotEmpty()) {
        val rect = pq.poll()
        if (!polygonCrossesThrough(rect)) {
            return rect.area
        }
    }
    return -1
}

fun main() {
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