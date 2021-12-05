import java.io.File

fun main() {
    part1()
    part2()
}

private fun part1() {
    val input = readInput()
    println(input.joinToString("\n"))

    var increases = 0
    for (i in 1 until input.size) {
        if (input[i] > input[i - 1]) {
            increases++
        }
    }

    println(increases)
}

private fun readInput(): List<Int> {
    val file = File("src/main/resources/day01.txt")
    val input = mutableListOf<Int>()
    file.forEachLine {
        input += it.toInt()
    }
    return input
}

private fun part2() {
    val input = readInput()
    println(input.joinToString("\n"))

    var increases = 0
    for (i in 1 until input.size) {
        if (input[i] > input[i - 1]) {
            increases++
        }
    }

    println(increases)
}

private fun readInput2(): List<Int> {
    val file = File("src/main/resources/day01.txt")
    val result = mutableListOf<Int>()
    val lines = file.readLines()
    for (i in lines.indices) {
        val reading1 = lines[i].toInt()
        val reading2 = try { lines[i + 1].toInt() } catch (e: IndexOutOfBoundsException) { 0 }
        val reading3 = try { lines[i + 2].toInt() } catch (e: IndexOutOfBoundsException) { 0 }
        result += reading1 + reading2 + reading3
    }
    return result
}