package xyz.colmmurphy.aoc

import arrow.core.MemoizedDeepRecursiveFunction
import xyz.colmmurphy.aoc.util.graph.DirectedAdjacencyList
import xyz.colmmurphy.aoc.util.graph.Edge
import xyz.colmmurphy.aoc.util.graph.Graph
import xyz.colmmurphy.aoc.util.graph.Vertex
import xyz.colmmurphy.aoc.util.readInput
import kotlin.system.measureTimeMillis

private fun buildGraph(input: List<String>): DirectedAdjacencyList<String, Int> {
    val graph = DirectedAdjacencyList<String, Int>()
    for (line in input) {
        val vertices = line.split(Regex("[^a-z]+")).map { Vertex(it) }
        for (vertex in vertices) {
            graph.addVertex(vertex)
        }
        val source = vertices[0]
        for (i in 1 until vertices.size) {
            graph.addEdge(Edge(source, vertices[i], 1))
        }
    }
    return graph
}

private fun <T, W : Comparable<W>> countPaths(graph: Graph<T, W>, source: Vertex<T>, dest: Vertex<T>): Long {
    fun aux(curr: Vertex<T>): Long {
        if (curr == dest) {
            return 1L
        }
        return graph.adjacent(curr).sumOf(::aux)
    }

    val starts = graph.adjacent(source)
    return starts.sumOf(::aux)
}

private fun part1(input: List<String>): Long {
    val graph = buildGraph(input)
    // search from 'you' to find all paths that lead to 'out'
    // note that the graph is acyclic, no need to check for infinite search loops
    val source = graph.getVertex("you").getOrNull()!!
    val destination = graph.getVertex("out").getOrNull()!!
    return countPaths(graph, source, destination)
}

private fun <W : Comparable<W>> countProblematicPaths(
    graph: Graph<String, W>,
    source: Vertex<String>,
    dest: Vertex<String>
): Long {
    val aux =
        MemoizedDeepRecursiveFunction<Triple<Vertex<String>, Boolean, Boolean>, Long> { (curr, seenDac, seenFft) ->
            if (curr == dest) {
                if (seenDac && seenFft) {
                    return@MemoizedDeepRecursiveFunction 1L
                }
                return@MemoizedDeepRecursiveFunction 0L
            }

            return@MemoizedDeepRecursiveFunction graph.adjacent(curr).sumOf {
                callRecursive(Triple(it, seenDac || curr.data == "dac", seenFft || curr.data == "fft"))
            }
        }
    return aux(Triple(source, false, false))
}

private fun part2(input: List<String>): Long {
    val graph = buildGraph(input)

    val source = graph.getVertex("svr").getOrNull()!!
    val destination = graph.getVertex("out").getOrNull()!!
    return countProblematicPaths(graph, source, destination)
}

fun main() {
    val testInput = readInput("Day11_test")
    val testInput2 = readInput("Day11_test_2")
    val input = readInput("Day11")

    measureTimeMillis {
        println("Part 1 Test: ${part1(testInput)}")
        println("Part 1: ${part1(input)}")

        println("Part 2 Test: ${part2(testInput2)}")
        println("Part 2: ${part2(input)}")
    }.let {
        println("Took $it ms")
    }
}