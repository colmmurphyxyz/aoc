package xyz.colmmurphy.aoc.util.graph

data class Edge<T, W:Comparable<W>>(
    val source: Vertex<T>,
    val destination: Vertex<T>,
    val weight: W
) {
    fun opposite(u: Vertex<T>): Vertex<T> {
        if (u == source) {
            return destination
        } else if (u == destination) {
            return source
        }
        throw IllegalArgumentException("Vertex $u is not incident on edge $this")
    }
}
