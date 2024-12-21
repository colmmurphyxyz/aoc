import java.io.FileInputStream
import java.util.*
import kotlin.math.abs

fun main() {

    val input = getInput()

    val totalTime = kotlin.system.measureTimeMillis {
        val part1Time = kotlin.system.measureTimeMillis {
            val part1 = part1(input)
            println("part 1 answer: $part1")
        }
        println("part 1 time: $part1Time ms")

        val part2Time = kotlin.system.measureTimeMillis {
            val part2 = part2(input)
            println("part 2 answer: $part2")
        }
        println("part 2 time: $part2Time ms")
    }
    println("total time: $totalTime ms")
}

private fun part1(input: List<Int>): Long {
    var minPos = Int.MAX_VALUE
    var maxPos = 0
    for (x in input) {
        if (x < minPos) minPos = x
        if (x > maxPos) maxPos = x
    }

    var minFuel = Long.MAX_VALUE
    for (x in minPos..maxPos) {
        var fuelCost = 0L
        for (crab in input) {
            fuelCost += abs(crab - x)
        }
        if (fuelCost < minFuel) minFuel = fuelCost
    }
    return minFuel
}

private fun part2(input: List<Int>): Long {
    var minPos = Int.MAX_VALUE
    var maxPos = 0
    for (x in input) {
        if (x < minPos) minPos = x
        if (x > maxPos) maxPos = x
    }

    var minFuel = Long.MAX_VALUE
    for (x in minPos..maxPos) {
        var fuelCost = 0L
        for (crab in input) {
            val n = abs(crab - x)
            fuelCost += (0.5 * ((n * n) + n)).toLong()
        }
        if (fuelCost < minFuel) minFuel = fuelCost
    }
    return minFuel
}

private fun getInput(): List<Int> {
    val file = FileInputStream("src/main/resources/day07.txt")
    val scanner = Scanner(file)
    val result = mutableListOf<Int>()
    scanner.use { sc ->
        while (sc.hasNextLine()) {
            val line = sc.nextLine().split(",")
            for (i in line) result += i.toInt()
        }
    }
    return result
}