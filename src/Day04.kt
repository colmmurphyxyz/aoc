import java.io.FileInputStream
import java.util.Scanner
import kotlin.collections.toTypedArray

fun main() {
    part1()
    part2()
}

private fun part1() {
    val nums = getNumbers()
    val boards = getBoards()

    for (num in nums) {
        for (board in boards) {
            for (row in board) {
                for (i in row.indices) {
                    if (row[i] == num) row[i] = null
                }
            }
        }

        for (board in boards) {
            if (isBingo(board)) {
                println("BINGOOOO")
                var sum = 0
                for (row in board) {
                    for (x in row) {
                        if (x != null) sum += x
                    }
                }
                println(sum)
                println(num)
                println(sum * num)
                kotlin.system.exitProcess(0)
            }
        }
    }
}

fun getBoards(): List<Array<Array<Int?>>> {
    val file = FileInputStream("src/main/resources/day04.txt")
    val scanner = Scanner(file)
    scanner.use { sc ->
        val res = mutableListOf<Array<Array<Int?>>>()

        var board = Array<Array<Int?>>(0) {Array<Int?>(0) {null} }
        forEachLine@while (sc.hasNextLine()) {
            val line = sc.nextLine()
            if (line == "") continue@forEachLine

            val rowString = line.split(" ").toMutableList()
            rowString.removeAll { it -> it == "" }
            val row = rowString.map { it.toInt() }.toTypedArray<Int?>()
            board += row
            if (board.size == 5) {
                res += board
                board = Array<Array<Int?>>(0) {Array<Int?>(0) {null} }
            }
        }
        return res
    }
}

fun getNumbers(): List<Int> {
    return listOf<Int>(
        14,30,18,8,3,10,77,4,48,67,28,38,63,43,62,12,68,88,54,32,17,21,83,64,97,53,24,2,60,96,86,23,20,93,65,34,45
        ,46,42,49,71,9,61,16,31,1,29,40,59,87,95,41,39,27,6,25,19,58,80,81,50,79,73,15,70,37,92,94,7,55,85,98,5,84,
        99,26,66,57,82,75,22,89,74,36,11,76,56,33,13,72,35,78,47,91,51,44,69,0,90,52
    )
}

fun isRowNull(row: Array<Int?>): Boolean {
    for (i in row) {
        if (i != null) return false
    }
    return true
}

fun isRowNull(board: Array<Array<Int?>>, rowIndex: Int): Boolean {
    return isRowNull(board[rowIndex])
}

fun isColumnNull(board: Array<Array<Int?>>, columnIndex: Int): Boolean {
    for (row in board) {
        if (row[columnIndex] != null) return false
    }
    return true
}

fun isDiagonalNull(board: Array<Array<Int?>>): Boolean {
    for (i in 0..4) {
        if (board[i][i] != null) return false
        if (board[i][4 - i] != null) return false
    }
    return true
}

fun isBingo(board: Array<Array<Int?>>): Boolean {
    for (i in 0..4) {
        if (isRowNull(board, i)) return true
        if (isColumnNull(board, i)) return true
    }
    if (isDiagonalNull(board)) return true
    return false
}

private fun part2() {
    val nums = getNumbers()
    val boards = getBoards()
    val unansweredBoardsIndices = (boards.indices).toMutableList()
    println(unansweredBoardsIndices.joinToString(", "))

    var count = -1
    for (num in nums) {
        count++
        for (board in boards) {
            for (row in board) {
                for (i in row.indices) {
                    if (row[i] == num) row[i] = null
                }
            }
        }

        forAllBoards@for (i in boards.indices) {
            if (isBingo(boards[i])) {
                unansweredBoardsIndices.remove(i)
                continue@forAllBoards
            }

            if (unansweredBoardsIndices.size == 1) {
                println("final board is at index ${unansweredBoardsIndices[0]}")
                finalBoardCalcs(boards[unansweredBoardsIndices[0]], count, nums)
            }
        }
    }
}

private fun finalBoardCalcs(finalBoard: Array<Array<Int?>>, count: Int, nums: List<Int>) {
    for (n in count until nums.size) {
        val num = nums[n]

        for (row in finalBoard) {
            for (i in row.indices) {
                if (row[i] == num) row[i] = null
                if (isBingo(finalBoard)) {
                    var sum = 0
                    for (finalRow in finalBoard) {
                        for (j in finalRow.indices) {
                            if (finalRow[j] != null) sum += finalRow[j]!!
                        }
                    }
                    println(sum)
                    println(num)
                    println(sum * num)
                    return
                }
            }
        }
    }

    return
}