package xyz.colmmurphy.aoc.util.graph

class SimpleAdjacencyListGraph<T, W : Comparable<W>> : Graph<T, W> {
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
        val incidentEdges = adjacencyMap[v] ?: emptyList()
        for (edge in incidentEdges) {
            adjacencyMap[edge.source]?.remove(edge)
            adjacencyMap[edge.destination]?.remove(edge)
            edges.remove(edge)
        }
        adjacencyMap.remove(v)
    }

    override fun addEdge(e: Edge<T, W>) {
        adjacencyMap[e.source]?.add(e)
        adjacencyMap[e.destination]?.add(e)
        edges.add(e)
    }

    override fun hasEdge(e: Edge<T, W>): Boolean {
        return edges.contains(e)
    }

    override fun removeEdge(e: Edge<T, W>) {
        adjacencyMap[e.source]?.remove(e)
        adjacencyMap[e.destination]?.remove(e)
        edges.remove(e)
    }

    fun getComponents(): List<List<Vertex<T>>> {
        val vertices = adjacencyMap.keys
        val dsu = DSU(vertices)
        for (v in vertices) {
            for (next in adjacencyMap[v]!!) {
                dsu.unite(v, next.opposite(v))
            }
        }

        val resMap = mutableMapOf<Vertex<T>, MutableList<Vertex<T>>>()
        for (v in vertices) {
            val parent = dsu.findParent(v)
            if (!resMap.containsKey(parent)) {
                resMap[parent] = mutableListOf()
            }
            resMap[parent]!!.add(v)
        }
        val res = mutableListOf<MutableList<Vertex<T>>>()
        for (par in resMap.keys) {
            res.add(resMap[par]!!)
        }
        return res
    }
}