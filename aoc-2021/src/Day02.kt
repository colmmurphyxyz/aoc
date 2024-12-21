import java.io.FileInputStream
import java.util.*

fun main() {
    part1()
    part2()
}

private fun part1() {
    var horizontal = 0
    var vertical = 0
    for ((command, num) in getInput()) {
        when (command) {
            "forward" -> horizontal += num
            "up" -> vertical -= num
            "down" -> vertical += num
            else -> println("ERROR")
        }
    }
    println(horizontal * vertical)
}

private fun getInput(): List<Pair<String, Int>> {
    val inputStream = FileInputStream("src/main/resources/day02.txt")
    val scanner = Scanner(inputStream)
    val result = mutableListOf<Pair<String, Int>>()
    try {
        while (scanner.hasNextLine()) {
            val line: String = scanner.nextLine()
            result += line.substringBefore(" ") to line.substringAfter(" ").toInt()
        }
        // note that Scanner suppresses exceptions
        if (scanner.ioException() != null) {
            throw scanner.ioException()
        }
    } finally {
        scanner.close()
    }
    return result
}

private fun part2() {
    var horizontal = 0
    var vertical = 0
    var aim = 0
    for ((command, num) in getInput2()) {
        when (command) {
            "forward" -> {
                horizontal += num
                vertical += aim * num
            }
            "up" -> {
                aim -= num
            }
            "down" -> {
                aim += num
            }
            else -> println("ERROR")
        }
    }
    println(horizontal * vertical)
}

private fun getInput2(): List<Pair<String, Int>> {
    val inputStream = FileInputStream("src/main/resources/day02.txt")
    val scanner = Scanner(inputStream)
    val result = mutableListOf<Pair<String, Int>>()
    try {
        while (scanner.hasNextLine()) {
            val line: String = scanner.nextLine()
            result += line.substringBefore(" ") to line.substringAfter(" ").toInt()
        }
        // note that Scanner suppresses exceptions
        if (scanner.ioException() != null) {
            throw scanner.ioException()
        }
    } finally {
        scanner.close()
    }
    return result
}