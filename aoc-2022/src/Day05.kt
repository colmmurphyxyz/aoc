import java.util.Stack
import kotlin.properties.Delegates

fun main() {

    fun inputToStacks(input: List<String>): List<Stack<Char>> {
        val stacks = List<Stack<Char>>(9) { Stack<Char>()}
//        val stacks = listOf(Stack<Char>(), Stack<Char>(), Stack<Char>())
        for (line in input) {
            for (charIndex in 1..33 step 4) {
                if (charIndex >= line.length) continue
                if (!line[charIndex].isWhitespace()) {
                    stacks[charIndex / 4].push(line[charIndex])
                }
            }
        }
        return stacks.map { it -> it.invert() }
    }

    fun part1(input: List<String>, firstBlankLineIndex: Int): String {
        val stacks = inputToStacks(input.slice(0..(firstBlankLineIndex - 2)))
        val operations = input.slice((firstBlankLineIndex + 1)..input.lastIndex)
        for (operation in operations) {
            val components = operation.split(" ")

            repeat (components[1].toInt()) {
                val from = components[3].toInt() - 1
                val to = components[5].toInt() - 1
                stacks[to].push(stacks[from].pop())
            }
        }
        return stacks.joinToString(separator = "") { it -> it.peek().toString() }
    }

    fun part2(input: List<String>, firstBlankLineIndex: Int): String {
        val stacks = inputToStacks(input.slice(0..(firstBlankLineIndex - 2))).toMutableList()
        val operations = input.slice((firstBlankLineIndex + 1)..input.lastIndex)
        for (operation in operations) {
            val components = operation.split(" ")

            val from = stacks[components[3].toInt() - 1]
            val to = stacks[components[5].toInt() - 1]
            stacks[components[5].toInt() - 1] = moveNItems(from, to, components[1].toInt())
        }
        return stacks.joinToString(separator = "") { it -> it.peek().toString() }
    }

    val input = readInput("Day05")
    var firstBlankLineIndex by Delegates.notNull<Int>()
    for (i in input.indices) {
        if (input[i].isBlank()) {
            println("first blank line = $i")
            firstBlankLineIndex = i
            break
        }
    }
    println("Part 1 answer: ${part1(input, firstBlankLineIndex)}")
    println("Part 2 answer: ${part2(input, firstBlankLineIndex)}")
}