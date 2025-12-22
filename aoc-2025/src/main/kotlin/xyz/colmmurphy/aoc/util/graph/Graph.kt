package xyz.colmmurphy.aoc.util.graph

import arrow.core.Option

interface Graph<T, W : Comparable<W>> {

    fun addVertex(v: Vertex<T>)

    fun hasVertex(v: Vertex<T>): Boolean

    fun removeVertex(v: Vertex<T>)

    fun getVertex(data: T): Option<Vertex<T>>

    fun addEdge(e: Edge<T, W>)

    fun hasEdge(e: Edge<T, W>): Boolean

    fun removeEdge(e: Edge<T, W>)

    fun adjacent(v: Vertex<T>): List<Vertex<T>>

}