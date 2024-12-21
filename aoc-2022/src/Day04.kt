fun main() {
    fun part1(input: List<String>): Int {
        var answer = 0
        for (line in input) {
            val pair = line.split(",")
            val r1 = pair[0].split("-").map { it -> it.toInt() }
            val r2 = pair[1].split("-").map { it -> it.toInt() }
            val set1 = (r1[0]..r1[1]).toSet()
            val set2 = (r2[0]..r2[1]).toSet()
            if (set1.containsAll(set2) || set2.containsAll(set1)) {
                answer += 1
            }
        }
        return answer
    }

    fun part2(input: List<String>): Int {
        var answer = 0
        for (line in input) {
            val pair = line.split(",")
            val r1 = pair[0].split("-").map { it -> it.toInt() }
            val r2 = pair[1].split("-").map { it -> it.toInt() }
            val set1 = (r1[0]..r1[1]).toSet()
            val set2 = (r2[0]..r2[1]).toSet()
            if ((set1 intersect set2).isNotEmpty()) {
                answer += 1
            }
        }
        return answer
    }

    val input = readInput("Day04")
    println("Part 1 answer: ${part1(input)}")
    println("Part 2 answer: ${part2(input)}")
}