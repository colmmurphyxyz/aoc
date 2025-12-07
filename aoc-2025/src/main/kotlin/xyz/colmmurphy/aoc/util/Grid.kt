package xyz.colmmurphy.aoc.util

import arrow.core.Option
import arrow.core.Some
import arrow.core.none

open class Grid<T> {
    private val data: MutableList<MutableList<T>>

    fun getData(): List<List<T>> {
        return data
    }

    val height: Int
        get() = data.size
    val width: Int
        get() = data[0].size

    constructor(data: List<List<T>>) {
        this.data = data.map { it.toMutableList() }.toMutableList()
    }

    constructor(width: Int, height: Int, initialValue: T) {
        this.data = MutableList(height) { MutableList(width) { initialValue } }
    }

    companion object {
        fun fromStringList(strings: Iterable<String>): Grid<Char> {
            return Grid(strings.map { it.toCharArray().toList() })
        }
    }

    open fun get(rowIdx: Int, colIdx: Int): T {
        return data[rowIdx][colIdx]
    }

    open fun get(pos: Pair<Int, Int>): T {
        return get(pos.first, pos.second)
    }

    open fun set(rowIdx: Int, colIdx: Int, value: T) {
        data[rowIdx][colIdx] = value
    }

    open fun set(pos: Pair<Int, Int>, value: T) {
        data[pos.first][pos.second] = value
    }

    open fun getRow(rowIdx: Int): List<T> {
        return data[rowIdx]
    }

    /**
     * @return the contents of the column at index `colIdx`. In order of increasing row indices (i.e. top-to-bottom)
     */
    open fun getCol(colIdx: Int): List<T> {
        return data.map { it[colIdx] }
    }

    /**
     * Search the grid from left to right, top to bottom for a value equal to `value`
     * @return Position (rowIdx, colIdx) or first matching item, or none if value was not found
     */
    open fun firstOccurrenceOf(value: T): Option<Pair<Int, Int>> {
        for (i in data.indices) {
            for (j in data[i].indices) {
                if (get(i, j) == value) {
                    return Some(i to j)
                }
            }
        }
        return none()
    }

    /**
     * @return List of all valid points (rowIdx, colIdx) in the grid
     */
    open fun allPoints(): List<Pair<Int, Int>> {
        val ret = mutableListOf<Pair<Int, Int>>()
        for (rowIdx in data.indices) {
            for (colIdx in data.indices) {
                ret.add(rowIdx to colIdx)
            }
        }
        return ret
    }

    open fun isInGrid(rowIdx: Int, colIdx: Int): Boolean {
        return (rowIdx in 0..<height && colIdx in 0..<width)
    }

    open fun isInGrid(point: Pair<Int, Int>): Boolean = isInGrid(point.first, point.second)

    open fun getNorth(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == 0) {
            return none()
        }
        return Some(get(rowIdx - 1, colIdx))
    }

    open fun getNorthEast(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == 0 || colIdx == data[0].lastIndex) {
            return none()
        }
        return Some(get(rowIdx - 1, colIdx + 1))
    }

    open fun getEast(rowIdx: Int, colIdx: Int): Option<T> {
        if (colIdx == data[0].lastIndex) {
            return none()
        }
        return Some(get(rowIdx, colIdx + 1))
    }

    open fun getSouthEast(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == data.lastIndex || colIdx == data[0].lastIndex) {
            return none()
        }
        return Some(get(rowIdx + 1, colIdx + 1))
    }

    open fun getSouth(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == data.lastIndex) {
            return none()
        }
        return Some(get(rowIdx + 1, colIdx))
    }

    open fun getSouthWest(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == data.lastIndex || colIdx == 0) {
            return none()
        }
        return Some(get(rowIdx + 1, colIdx - 1))
    }

    open fun getWest(rowIdx: Int, colIdx: Int): Option<T> {
        if (colIdx == 0) {
            return none()
        }
        return Some(get(rowIdx, colIdx - 1))
    }

    open fun getNorthWest(rowIdx: Int, colIdx: Int): Option<T> {
        if (rowIdx == 0 || colIdx == 0) {
            return none()
        }
        return Some(get(rowIdx - 1, colIdx - 1))
    }

    /**
     * @return The 8 directly and diagonally adjacent elements. In the order N, NE, E, SE, S, SW, W, NW
     */
    open fun getAdjacent(rowIdx: Int, colIdx: Int): List<Option<T>> {
        return listOf(
            getNorth(rowIdx, colIdx),
            getNorthEast(rowIdx, colIdx),
            getEast(rowIdx, colIdx),
            getSouthEast(rowIdx, colIdx),
            getSouth(rowIdx, colIdx),
            getSouthWest(rowIdx, colIdx),
            getWest(rowIdx, colIdx),
            getNorthWest(rowIdx, colIdx)
        )
    }

    /**
     * @return The 4 directly adjacent elements. In the order N, E, S, W
     */
    open fun getDirectlyAdjacent(rowIdx: Int, colIdx: Int): List<T> {
        return listOf(
            getNorth(rowIdx, colIdx),
            getEast(rowIdx, colIdx),
            getSouth(rowIdx, colIdx),
            getWest(rowIdx, colIdx)
        ).mapNotNull { it.getOrNull() }
    }

    override fun toString(): String {
        return data.joinToString(separator = "") { it.joinToString("") + "\n" }
    }

    override fun equals(other: Any?): Boolean {
        if (other == null) {
            return false
        }
        if (other !is Grid<*>) {
            return false
        }
        return other.hashCode() == this.hashCode()
    }

    override fun hashCode(): Int {
        return (toString().hashCode() * width) / height
    }
}