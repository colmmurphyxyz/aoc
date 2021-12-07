import java.io.File
import java.io.FileInputStream
import java.util.*

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


private fun part1(input: List<Int>): Int {
    val fish = input.toMutableList()
    repeat (80) {
        for (i in fish.indices) {
            val age = fish[i]
            if (age == 0) {
                fish[i] = 6
                fish.add(8)
            } else {
                fish[i]--
            }
        }
    }
    return fish.size
}

private fun part2(input: List<Int>): Long {
    val fish = input.toMutableList()
    val fishAges = hashMapOf<Int, Long>(
        0 to 0,
        1 to 0,
        2 to 0,
        3 to 0,
        4 to 0,
        5 to 0,
        6 to 0,
        7 to 0,
        8 to 0,
    )

    for (num in input) {
        fishAges[num] = fishAges[num]!! + 1
    }

    for (day in 1..256) {
        println("day = $day")
        val mapCopy = fishAges.toMap()
        val newFish = fishAges[0]!!
        for (i in 0..7) {
            fishAges[i] = mapCopy[i + 1]!!
        }
        fishAges[8] = newFish
        fishAges[6] = fishAges[6]!! + newFish
    }
    var sum = 0L
    for (i in 0..8) {
        sum += fishAges[i]!!
    }
    return sum
}

private fun getInput(): MutableList<Int> {
    val file = FileInputStream("src/main/resources/day06.txt")
    val scanner = Scanner(file)
    val result = mutableListOf<Int>()
    scanner.use { sc ->
        while (sc.hasNextLine()) {
            val line = sc.nextLine().split(",").map { it.toInt() }
            for (num in line) result += num
        }
    }

//    val file = File("src/main/resources/day06_sample.txt").readText()
//    val result = file.split(",").map { it.toInt() }.toMutableList()
    return result
}