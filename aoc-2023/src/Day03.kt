fun main() {
    fun isSymbol(c: Char): Boolean = !c.isDigit() && c != '.'

    /**
     * returns the number for which one of its digits is at the given row and column index
     */
    fun getNumber(input: List<String>, row: Int, col: Int): Int {
        if (!input[row][col].isDigit()) {
            throw IllegalArgumentException("character at row $row and column $col is not a digit")
        }
        var startIndex = col
        while (startIndex > 0 && input[row][startIndex - 1].isDigit()) startIndex--
        var endIndex = col
        while (endIndex < input[row].lastIndex && input[row][endIndex + 1].isDigit()) endIndex++
        return input[row].slice(startIndex.. endIndex).toInt()
    }

    fun getBoundingBoxPoints(input: List<String>, row: Int, startIndex: Int, endIndex: Int): List<Pair<Int, Int>> {
        val boundingBoxPoints = mutableListOf<Pair<Int, Int>>()
        val size = (endIndex - startIndex) + 1
        if (row != input.lastIndex) {
            for (i in 0 until size) {
                boundingBoxPoints.add(row + 1 to startIndex + i)
            }
            if (endIndex != input[row].lastIndex) {
                boundingBoxPoints.add(row + 1 to endIndex + 1)
            }
            if (startIndex > 0) {
                boundingBoxPoints.add(row + 1 to startIndex - 1)
            }
        }
        if (row > 0) {
            for (i in 0 until size) {
                boundingBoxPoints.add(row - 1 to startIndex + i)
            }
            if (endIndex != input[row].lastIndex) {
                boundingBoxPoints.add(row - 1 to endIndex + 1)
            }
            if (startIndex > 0) {
                boundingBoxPoints.add(row - 1 to startIndex - 1)
            }
        }
        if (startIndex > 0) {
            boundingBoxPoints.add(row to startIndex - 1)
        }
        if (endIndex != input[row].lastIndex) {
            boundingBoxPoints.add(row to endIndex + 1)
        }
        return boundingBoxPoints.toList()
    }

    fun part1(input: List<String>): Int {
        var sum = 0
        input.forEachIndexed { index, line ->
            var i = 0
            while (i < line.length) {
                val c = line[i]
                if (c.isDigit()) {
                    val num = getNumber(input, index, i)
                    val bb = getBoundingBoxPoints(input, index, i, i + num.toString().length - 1)
                    if (bb.any { input[it.first][it.second] != '.' }) {
                        sum += num
                    }
                    i += num.toString().length - 1
                }
                i += 1
            }
        }
        return sum
    }

    fun gearRatio(input: List<String>, row: Int, col: Int): Int {
        val c = input[row][col]
        if (c != '*') return 0
        val numbers = mutableListOf<Int>()
        for (i in getBoundingBoxPoints(input, row, col, col)) {
            try {
                val n = getNumber(input, i.first, i.second)
                if (n !in numbers) numbers.add(n)
            } catch (e: IllegalArgumentException) {
                continue
            }
        }
        return if (numbers.size == 2) numbers[0] * numbers[1] else 0
    }

    fun part2(input: List<String>): Int {
        var sum = 0
        input.forEachIndexed { rowIdx, line ->
            line.forEachIndexed { colIdx, c ->
                sum += gearRatio(input, rowIdx, colIdx)
            }
        }
        return sum
    }

    val testInput = readInput("Day03_test")
    val input = readInput("Day03")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

    println("Part 2 Test: ${part2(testInput)}")
    println("Part 2: ${part2(input)}")
}