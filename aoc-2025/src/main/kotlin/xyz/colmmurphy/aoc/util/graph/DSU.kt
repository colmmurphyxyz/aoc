package xyz.colmmurphy.aoc.util.graph

internal class DSU<T>(vertices: Iterable<Vertex<T>>) {
    private var parent = mutableMapOf<Vertex<T>, Vertex<T>>()

    init {
        // Each node is its own parent initially
        for (v in vertices) {
            parent[v] = v
        }
    }

    // Find with path compression
    fun findParent(v: Vertex<T>): Vertex<T> {
        if (v == parent[v]) {
            return v
        }
        return findParent(parent[v]!!).also { parent[v] = it }
    }

    // Union the sets of u and v
    fun unite(u: Vertex<T>, v: Vertex<T>) {
        val pu = findParent(u)
        val pv = findParent(v)
        if (pu == pv) return
        parent[pu] = pv
    }
}