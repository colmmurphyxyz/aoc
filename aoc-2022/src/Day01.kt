import java.util.*

fun main() {
    fun getInput(): List<Int> {
        val input = readInput("Day01")
        val elfCaloriesString = mutableListOf<List<String>>()
        var sliceStart = 0
        for (i in input.indices) {
            if (i == input.lastIndex) {
                elfCaloriesString.add(input.slice(sliceStart..i))
                break
            }
            if (input[i] == "") {
                elfCaloriesString.add(input.slice(sliceStart until i))
                sliceStart = i + 1
            }
        }
//    convert the strings to ints
        val elfCalories: List<List<Int>> = elfCaloriesString.map { it: List<String> ->
            it.map { str: String -> str.toInt() }
        }
//    get and return the sum of each inner list
        return elfCalories.map { it -> it.sum() }
    }

    fun part1(): Int {
        return getInput().max()
    }

    fun part2(): Int {
        val totalElfCalories = getInput()
        val top3Calories = intArrayOf(-1, -2, -3)
        for (cals in totalElfCalories) {
            if (cals > top3Calories[2]) {
                top3Calories[2] = cals
                if (top3Calories[2] > top3Calories[1]) {
                    top3Calories.swap(2, 1)
                }
                if (top3Calories[1] > top3Calories[0]) {
                    top3Calories.swap(1, 0)
                }
            }
        }
        println(top3Calories.joinToString(", "))

        return top3Calories.sum()
    }
    println("Part 1 answer: ${part1()}")
    println("Part 2 answer: ${part2()}")
//        if (cals > top3Calories[0]) {
//            top3Calories[2] = top3Calories[1]
//            top3Calories[1] = top3Calories[0]
//            top3Calories[0] = cals
//        } else if (cals > top3Calories[1]) {
//            top3Calories[2] = top3Calories[1]
//            top3Calories[1] = cals
//        } else if (cals > top3Calories[2]) {
//            top3Calories[2] = cals
//        }
//    }

}
