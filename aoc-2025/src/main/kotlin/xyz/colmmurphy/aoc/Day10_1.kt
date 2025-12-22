package xyz.colmmurphy.aoc

import xyz.colmmurphy.aoc.util.commaSeparatedNums
import xyz.colmmurphy.aoc.util.readInput
import java.util.*
import kotlin.system.measureTimeMillis

private data class Machine(val goalLights: Int, val switches: List<Int>, val joltages: IntArray)

private fun BooleanArray.toBitmaskedInt(): Int {
    var ans = 0
    for (i in this.indices) {
        if (this[i]) {
            ans = ans or (1 shl i)
        }
    }
    return ans
}

private fun IntArray.toBitmaskedInt(): Int {
    var ans = 0
    for (idx in this) {
        ans = ans or (1 shl idx)
    }
    return ans
}

private fun flipSwitches(lights: Int, moves: Int): Int {
    return lights xor moves
}

private fun parseInput(input: Iterable<String>): List<Machine> {
    val machines = mutableListOf<Machine>()
    for (line in input) {
        val lights = line.substringAfter('[').substringBefore(']')
            .map { it == '#'}
            .toBooleanArray()
            .toBitmaskedInt()
        val switchesRaw = line.substringAfter(']').substringBefore('{').trim()
        val buckets = mutableListOf<String>()
        var bucket = ""
        var i = 1
        while (i < switchesRaw.length) {
            val char = switchesRaw[i]
            if (char == ')') {
                buckets.add(bucket)
                bucket = ""
                i += 3
            } else {
                bucket += char
                i += 1
            }
        }
        if (bucket.isNotEmpty()) {
            buckets.add(bucket)
        }
        val switches = buckets.map(String::commaSeparatedNums).toList().map { it.toIntArray().toBitmaskedInt() }

        val joltages = line
            .substringAfter('{')
            .substringBefore('}')
            .commaSeparatedNums()
            .toIntArray()

        machines.add(Machine(lights, switches, joltages))

    }

    return machines
}

private data class QueueEntry(val moves: Int, val curr: Int, val nextMove: Int)

private fun part1(input: List<String>): Int {
    val machines = parseInput(input)
    var minimumMoves = 0
    for (i in machines.indices) {
        val machine = machines[i]
        val goal = machine.goalLights
        val queue: Queue<QueueEntry> = LinkedList()
        // populate queue
        val initialState = 0
        for (switch in machine.switches) {
            queue.add(QueueEntry(0, initialState, switch))
        }
        // maintain a set of states we have seen before to limit the search space
        val seenStates = mutableSetOf<Int>()
        // technically an infinite loop, hope we find a solution
        while (queue.isNotEmpty()) {
            val entry = queue.poll()
            val nextState = flipSwitches(entry.curr, entry.nextMove)
            if (nextState == goal) {
                minimumMoves += entry.moves + 1
                break
            }
            if (seenStates.contains(nextState)) {
                continue
            }
            seenStates.add(nextState)
            // add next paths to queue
            for (switch in machine.switches) {
                queue.add(QueueEntry(entry.moves + 1, nextState, switch))
            }
        }
    }
    return minimumMoves
}

private fun part2(input: List<String>): Long {

    return -1L
}

fun main() {
    val testInput = readInput("Day10_test")
    val input = readInput("Day10")

    measureTimeMillis {
        println("Part 1 Test: ${part1(testInput)}")
        println("Part 1: ${part1(input)}")
//
//        println("Part 2 Test: ${part2(testInput)}")
//        println("Part 2: ${part2(input)}")
    }.let {
        println("Took $it ms")
    }
}