package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.graph.Edge
import xyz.colmmurphy.aoc.util.graph.SimpleAdjacencyListGraph
import xyz.colmmurphy.aoc.util.graph.Vertex
import xyz.colmmurphy.aoc.util.readInput
import java.util.PriorityQueue
import kotlin.system.measureTimeMillis

typealias Point3 = Triple<Long, Long, Long>

data class Line3D(val start: Point3, val end: Point3) : Comparable<Line3D> {

    val len = euclidean(start, end)
    override fun compareTo(other: Line3D): Int {
        if (len < other.len) {
            return -1
        } else if (len > other.len) {
            return 1
        }
        return 0
    }

}

fun euclidean(a: Point3, b: Point3): Long {
    return ((a.first - b.first) * (a.first - b.first)) +
            ((a.second - b.second) * (a.second - b.second)) +
            ((a.third - b.third) * (a.third - b.third))
}

private fun parsePoints(input: List<String>): List<Point3> =
    input.map { line ->
        line
            .split(",")
            .map(String::toLong)
            .let { nums ->
                Point3(nums[0], nums[1], nums[2])
            }
    }

private var edgesToCreate: Int = 10
private fun part1(input: List<String>): Long {
    // parse input
    val numPoints = input.size
    val numPairs = ((numPoints * (numPoints - 1)) / 2) + 1

    // user PQ over inserting to a (sorted) list
    val pq = PriorityQueue<Line3D>(numPairs)
    val points = parsePoints(input)
    for (i in points.indices) {
        for (j in i + 1..points.lastIndex) {
            pq.add(Line3D(points[i], points[j]))
        }
    }

    // create and populate graph
    val graph = SimpleAdjacencyListGraph<Point3, Long>()
    repeat(edgesToCreate) {
        val line = pq.poll()
        val a = Vertex(line.start)
        val b = Vertex(line.end)

        graph.addVertex(a)
        graph.addVertex(b)

        val e = Edge(a, b, line.len)
        graph.addEdge(e)
    }

    // find "circuits" e.g. connected components
    // and return the product of the size of the 3 largest
    val components = graph.getComponents()
    return components
        .map { it.size }
        .sortedDescending()
        .take(3)
        .fold(1) { a, b -> a * b}
}

private fun part2(input: List<String>): Long {
// parse input
    val numPoints = input.size
    val numPairs = ((numPoints * (numPoints - 1)) / 2) + 1

    // user PQ over inserting to a (sorted) list
    val pq = PriorityQueue<Line3D>(numPairs)
    val points = parsePoints(input)
    for (i in points.indices) {
        for (j in i + 1..points.lastIndex) {
            pq.add(Line3D(points[i], points[j]))
        }
    }

    // create and populate graphs
    val graph = SimpleAdjacencyListGraph<Point3, Long>()
    var lastCreatedEdge: Edge<Point3, Long>? = null
    // will take a hot second
    while (true) {
        val components = graph.getComponents()
        if (components.size == 1 && components[0].size == numPoints) {
            break
        }
        val line = pq.poll()
        val a = Vertex(line.start)
        val b = Vertex(line.end)

        graph.addVertex(a)
        graph.addVertex(b)

        val e = Edge(a, b, line.len)
        graph.addEdge(e)
        lastCreatedEdge = e
    }
    val la = lastCreatedEdge!!.source
    val lb = lastCreatedEdge.destination
    return la.data.first * lb.data.first
}

fun main() {
    // test if implementation meets criteria from the description, like:
    val testInput = readInput("Day08_test")
    val input = readInput("Day08")

    measureTimeMillis {
        println("Part 1 Test: ${part1(testInput)}")
        edgesToCreate = 1000
        println("Part 1: ${part1(input)}")

        println("Part 2 Test: ${part2(testInput)}")
        println("Part 2: ${part2(input)}")
    }.let {
        println("Took $it ms")
    }
}