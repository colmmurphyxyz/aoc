import java.io.FileInputStream
import java.util.*

fun main() {
    part1()
    part2()
}

private fun part1() {
    val input = inputStringDay03()

    var gammaRate = 0
    var epsilonRate = 0
    for (i in 0..11) {
        var one = 0
        var zero = 0
        for (num in input) {
            if (num[i] == '1') one++ else zero++
        }
        if (one > zero) {
            gammaRate += 1 shl (11 - i)
        } else {
            epsilonRate += 1 shl (11 - i)
        }
    }
    println(gammaRate * epsilonRate)
}

private fun part2() {
    val input = inputStringDay03()

    val oxygenGeneratorList = input.toMutableList()
    val co2ScrubberList = input.toMutableList()
    for (i in 0..11) {
        var x = 0
        for (num in oxygenGeneratorList) {
            if (num[i] == '1') x++ else x--
        }
        val mcb = if (x >= 0) '1' else '0'
        oxygenGeneratorList.removeIf { it -> it[i] != mcb }
        if (oxygenGeneratorList.size <= 1) break
    }
    val oxygenGeneratorRating = Integer.parseInt(oxygenGeneratorList[0], 2)
    println("Oxygen Generator Rating: $oxygenGeneratorRating")

    for (i in 0..11) {
        var x = 0
        for (num in co2ScrubberList) {
            if (num[i] == '1') x++ else x--
        }
        val lcb = if (x >= 0) '0' else '1'
        co2ScrubberList.removeIf { it -> it[i] != lcb }
        if (co2ScrubberList.size <= 1) break
    }
    val co2ScrubberRating = Integer.parseInt(co2ScrubberList[0], 2)
    println("Co2 Scrubber Rating: $co2ScrubberRating")

    println(oxygenGeneratorRating * co2ScrubberRating)
}

private fun inputStringDay03(): List<String> {
    val inputStream = FileInputStream("src/main/resources/day03.txt")
    val scanner = Scanner(inputStream)
    val res = mutableListOf<String>()
    scanner.use { sc ->
        while (sc.hasNextLine()) {
            val num = sc.nextLine()
            res += num
        }
        if (sc.ioException() != null) {
            throw sc.ioException()
        }
    }

    return res
}