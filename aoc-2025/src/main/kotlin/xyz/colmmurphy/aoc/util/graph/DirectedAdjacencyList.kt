package xyz.colmmurphy.aoc.util.graph

import arrow.core.Option
import arrow.core.firstOrNone

class DirectedAdjacencyList<T, W : Comparable<W>> : Graph<T, W> {
    private val adjacencyMap = mutableMapOf<Vertex<T>, MutableSet<Edge<T, W>>>()
    private val edges = mutableSetOf<Edge<T, W>>()

    override fun addVertex(v: Vertex<T>) {
        if (!adjacencyMap.containsKey(v)) {
            adjacencyMap[v] = mutableSetOf()
        }
    }

    override fun hasVertex(v: Vertex<T>): Boolean {
        return adjacencyMap.containsKey(v)
    }

    override fun removeVertex(v: Vertex<T>) {
        val incidentEdges = edges.filter {
            it.source == v || it.destination == v
        }
        incidentEdges.forEach { e ->
            val u = e.opposite(v)
            adjacencyMap[u]?.remove(e)
        }
        edges.removeAll(incidentEdges)

        adjacencyMap.remove(v)
    }

    override fun getVertex(data: T): Option<Vertex<T>> {
        return adjacencyMap.keys.firstOrNone { it.data == data }
    }

    override fun addEdge(e: Edge<T, W>) {
        adjacencyMap[e.source]?.add(e)
        edges.add(e)
    }

    override fun hasEdge(e: Edge<T, W>): Boolean {
        return edges.contains(e)
    }

    override fun removeEdge(e: Edge<T, W>) {
        adjacencyMap[e.source]?.remove(e)
        edges.remove(e)
    }

    override fun adjacent(v: Vertex<T>): List<Vertex<T>> {
        return adjacencyMap[v]?.map { it.opposite(v) } ?: emptyList()
    }

    fun hasCycle(): Boolean {
        fun dfs(vertex: Vertex<T>, visited: MutableSet<Vertex<T>>, recStack: MutableSet<Vertex<T>>): Boolean {
            if (recStack.contains(vertex)) {
                return true
            }
            if (visited.contains(vertex)) {
                return false
            }
            recStack.add(vertex)
            visited.add(vertex)
            for (adjacent in adjacencyMap[vertex]!!) {
                if (dfs(adjacent.opposite(vertex), visited, recStack)) {
                    return true
                }
            }

            recStack.remove(vertex)
            return false
        }

        val visited = mutableSetOf<Vertex<T>>()
        val recStack = mutableSetOf<Vertex<T>>()
        for (vertex in adjacencyMap.keys) {
            if (!visited.contains(vertex) && dfs(vertex, visited, recStack)) {
                return true
            }
        }
        return false
    }
}