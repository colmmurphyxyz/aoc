package xyz.colmmurphy.aoc.util

class ImmutableGrid<T> : Grid<T> {

    constructor(data: List<List<T>>) : super(data.map { it.toMutableList() }.toMutableList())

    constructor(width: Int, height: Int, initialValue: T): super(width, height, initialValue)

    companion object {
        fun fromStringList(strings: Iterable<String>): ImmutableGrid<Char> {
            return ImmutableGrid(strings.map { it.toCharArray().toList() })
        }
    }

    override fun set(pos: Pair<Int, Int>, value: T) {
        throw UnsupportedOperationException()
    }

    override fun set(rowIdx: Int, colIdx: Int, value: T) {
        throw UnsupportedOperationException()
    }
}