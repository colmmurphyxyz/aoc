fun main() {

    val priorityOf: (Char) -> Int = { c ->
       val code = c.code
       if (code in 97..122) {   // lower case letters
            code - 96
        } else if (code in 65..90) { // upper case letters
            code - 38
        } else throw IllegalArgumentException() // non-letter characters
    }
    fun part1_old(): Int {
        var priorities = 0
        val input = readInput("Day03")
        for (rucksack in input) {
            val halfway = rucksack.length / 2
            val compartment1 = rucksack.slice(0 until halfway)
            val compartment2 = rucksack.slice(halfway until rucksack.length)
            val comp1Chars = HashMap<Char, Int>(52)
            val comp2Chars = HashMap<Char, Int>(52)
            for (i in compartment1.indices) {
                comp1Chars[compartment1[i]] = comp1Chars.getOrDefault(compartment1[i], 0) + 1
                comp2Chars[compartment2[i]] = comp2Chars.getOrDefault(compartment2[i], 0) + 1
            }
            for (key in comp1Chars.keys) {
                if (comp1Chars.getOrDefault(key, 0) > 0 && comp2Chars.getOrDefault(key, 0) > 0) {
                    priorities += priorityOf(key)
                }
            }
        }
        return priorities
    }

    fun part1(): Int {
        var priorities = 0
        val input = readInput("Day03")
        for (rucksack in input) {
            val halfway = rucksack.length / 2
            val compartment1 = rucksack.slice(0 until halfway).toSet()
            val compartment2 = rucksack.slice(halfway until rucksack.length).toSet()
            val commonCharacters = compartment1 intersect compartment2
            for (c in commonCharacters) {
                priorities += priorityOf(c)
            }
        }
        return priorities
    }

    fun part2(): Int {
        var priorities = 0
        val input = readInput("Day03")
        for (i in 0..input.size-2 step 3) {
            // find the one letter in common among all 3 lines
            val commonCharacter = (input[i].toSet() intersect input[i + 1].toSet() intersect input[i + 2].toSet())
            if (commonCharacter.size != 1) {
                throw IllegalArgumentException("commonCharacter.size = ${commonCharacter.size}, should be 1")
            }
            priorities += priorityOf(commonCharacter.first())
        }
        return priorities
    }

//    println("Part 1 answer: ${part1()}")
    println("Part 2 answer: ${part2()}")
}