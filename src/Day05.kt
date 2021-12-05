import java.io.FileInputStream
import java.util.*

fun main() {
    println("time = ${kotlin.system.measureTimeMillis {
        println("Part 1 answer: ${part1()}")
        println("Part 2 answer: ${part2()}")
    }}")
}

private fun part1(): Int {
    val input = getInput()
    // remove non-horizontal and non-vertical lines (lines where x1 != x2 and y1 != y2)
    val validLineIndices = (0 until input.size).toMutableList()
    for (i in input.indices) {
        val line = input[i]
        if (line.x1 != line.x2 && line.y1 != line.y2) {
            validLineIndices.remove(i)
        }
    }
    val lines = mutableListOf<Pair<Pair<Int, Int>, Pair<Int, Int>>>()
    for (validLineIndex in validLineIndices) {
        lines += input[validLineIndex]
    }

//    for (line in lines) {
//        System.out.printf("%d , %d -> %d , %d\n", line.x1, line.y1, line.x2, line.y2)
//    }

    val board = getBoard(lines)

    for (line in lines) {
        if (line.y1 == line.y2) { // horizontal lines
            val y = line.y1
            val range = if (line.x1 < line.x2) (line.x1..line.x2) else (line.x1 downTo line.x2)
            for (x in range) {
                board[x][y]++
            }
        } else { // vertical lines
            val x = line.x1
            val range = if (line.y1 < line.y2) (line.y1..line.y2) else (line.y1 downTo line.y2)
            for (y in range) {
                board[x][y]++
            }
        }
    }

    //printBoard(board)

    var total = 0
    for (column in board) {
        for (value in column) {
            if (value >= 2) total++
        }
    }
    return total
}

private fun part2(): Int {
    val lines = getInput()

//    for (line in lines) {
//        System.out.printf("%d , %d -> %d , %d\n", line.x1, line.y1, line.x2, line.y2)
//    }

    val board = getBoard(lines)

    for (line in lines) {
        if (line.y1 == line.y2) { // horizontal lines
            val y = line.y1
            val range = if (line.x1 < line.x2) (line.x1..line.x2) else (line.x1 downTo line.x2)
            for (x in range) {
                board[x][y]++
            }
        } else if (line.x1 == line.x2) { // vertical lines
            val x = line.x1
            val range = if (line.y1 < line.y2) (line.y1..line.y2) else (line.y1 downTo line.y2)
            for (y in range) {
                board[x][y]++
            }
        } else { // diagonal lines
            val xIncrement = if (line.x1 < line.x2) 1 else -1
            val yIncrement = if (line.y1 < line.y2) 1 else -1
            var x = line.x1
            var y = line.y1
            do {
                board[x][y]++
                x += xIncrement
                y += yIncrement
            } while (x != line.x2 && y != line.y2)
            board[x][y]++
        }
    }

    //printBoard(board)

    var total = 0
    for (column in board) {
        for (value in column) {
            if (value >= 2) total++
        }
    }
    return total
}

private fun getInput(): MutableList<Pair<Pair<Int, Int>, Pair<Int, Int>>> {
    val file = FileInputStream("src/main/resources/day05.txt")
    val scanner = Scanner(file)
    val result = mutableListOf<Pair<Pair<Int, Int>, Pair<Int, Int>>>()
    scanner.use { sc ->
        while (sc.hasNextLine()) {
            val line = sc.nextLine().split(" -> ")
            result.add(
                (line[0].substringBefore(",").toInt() to line[0].substringAfter(",").toInt())
                        to
                        (line[1].substringBefore(",").toInt() to line[1].substringAfter(",").toInt())
            )
        }
    }
    return result
}

private fun getBoard(lines: MutableList<Pair<Pair<Int, Int>, Pair<Int, Int>>>): Array<Array<Int>> {
    var maxX = 0
    var maxY = 0
    for (line in lines) {
        when {
            line.x1 > maxX -> maxX = line.x1
            line.x2 > maxX -> maxX = line.x2
            line.y1 > maxY -> maxY = line.y1
            line.y2 > maxY -> maxY = line.y2
        }
    }
    return Array<Array<Int>>(maxX + 1) { column -> Array<Int>(maxY + 1) { initialValue -> 0 } }
}

fun printBoard(board: Array<Array<Int>>) {
    for (i in 0 until board[0].size) {
        print("| ")
        for (j in 0 until board.size) {
            print(board[j][i])
        }
        print(" |\n")
    }
}

private val Pair<Pair<Int, Int>, Pair<Int, Int>>.x1: Int
    get() = this.first.first

private val Pair<Pair<Int, Int>, Pair<Int, Int>>.x2: Int
    get() = this.second.first

private val Pair<Pair<Int, Int>, Pair<Int, Int>>.y1: Int
    get() = this.first.second

private val Pair<Pair<Int, Int>, Pair<Int, Int>>.y2: Int
    get() = this.second.second