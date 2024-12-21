fun main() {
    fun part1(input: List<String>): Int {
        val signal = input.joinToString("")
        var i = 0
        whileLoop@while (i < signal.lastIndex - 3) {
            for (j in i..(i + 3)) {
                if (j == i + 3) {
                    return j + 1
                }
                for (k in (j+1)..(i + 3)) {
                    if (signal[j] == signal[k]) {
                        i += 1
                        continue@whileLoop
                    }
                }
            }
        }
        throw Exception("Could not find start-of-packet-market")
    }

    fun part2(input: List<String>): Int {
        val signal = input.joinToString("")
        var i = 0
        whileLoop@while (i < signal.lastIndex - 13) {
            for (j in i..(i + 13)) {
                if (j == i + 13) {
                    return j + 1
                }
                for (k in (j+1)..(i + 13)) {
                    if (signal[j] == signal[k]) {
                        i += 1
                        continue@whileLoop
                    }
                }
            }
        }
        throw Exception("Could not find start-of-message-market")
    }

    val input = readInput("Day06")
    println("part 1 answer: ${part1(input)}")
    println("part 2 answer: ${part2(input)}")
}