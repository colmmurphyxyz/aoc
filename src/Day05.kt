fun main() {
    fun part1(input: List<String>): Long {
        val nums = input[0]
                .substringAfter("seeds: ")
                .split(" ")
                .map { it.toLong() }
                .toMutableList()
        var newNums = mutableListOf<Long>()
        var inputLineIndex = 3
        while (inputLineIndex < input.size) {
            while (inputLineIndex < input.size && input[inputLineIndex].isNotBlank()) {
                println("Line $inputLineIndex")
                if (nums.isEmpty()) {
                    inputLineIndex++
                    continue
                }
                val line = input[inputLineIndex]
                        .split(" ")
                        .map { it.toLong() }
                val destStart = line[0]
                val sourceStart = line[1]
                val rangeLen = line[2]
                val iter = nums.iterator()
                while (iter.hasNext()) {
                    val num = iter.next()
                    if (num in sourceStart..(sourceStart + rangeLen)) {
                        newNums.add(destStart + (num - sourceStart))
                        iter.remove()
                    }
                }
                inputLineIndex ++
            }
            nums.addAll(newNums)
            println(nums.joinToString())
            newNums = mutableListOf()
            inputLineIndex += 2
        }
//        println(newNums.joinToString())
        nums.addAll(newNums)
//        println(nums.joinToString())
        return nums.min()
    }

    val testInput = readInput("Day05_test")
    val input = readInput("Day05")

    println("Part 1 Test: ${part1(testInput)}")
    println("Part 1: ${part1(input)}")

//    println("Part 2 Test: ${part2(testInput)}")
//    println("Part 2: ${part2(input)}")
}