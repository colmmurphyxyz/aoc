package xyz.colmmurphy.aoc

import arrow.atomic.AtomicInt
import org.chocosolver.solver.Model
import org.chocosolver.solver.search.strategy.Search.intVarSearch
import org.chocosolver.solver.search.strategy.selectors.values.IntDomainMin
import org.chocosolver.solver.search.strategy.selectors.variables.FirstFail
import org.chocosolver.solver.variables.IntVar
import xyz.colmmurphy.aoc.util.commaSeparatedNums
import xyz.colmmurphy.aoc.util.readInput
import java.util.stream.Collectors
import kotlin.system.measureTimeMillis

private data class JMachine(val numLights: Int, val switches: List<IntArray>, val joltages: IntArray)

class ConstraintSolver(
    private val switchesMatrix: Array<IntArray>,
    private val joltagesVector: IntArray
) {
    private val n = switchesMatrix.size // number of columns in A (size of x)
    private val m = switchesMatrix[0].size // number of rows in A (size of B)

    init {
        require(joltagesVector.size == m) {
            "Joltages size must match number of rows in switches"
        }
        require(switchesMatrix.all { row -> row.all { it in 0..1 } }) {
            "Switches must contain only 0s and 1s"
        }
        require(joltagesVector.all { it >= 0 }) {
            "Joltages must contain only non-negative integers"
        }
    }

    fun solve(maxValue: Int = 1000): Solution? {
        val model = Model("My Constraint Solver")

        // calculate upper bounds for each variable
        // for x[i], the maximum it can be is the max of B[j] where A[i][j] = 1
        val upperBounds = IntArray(n) { i ->
            val maxB = joltagesVector.indices
                .filter { j -> switchesMatrix[i][j] == 1 }
                .maxOfOrNull { j -> joltagesVector[j] } ?: 0
            maxValue.coerceAtMost(maxB)
        }

        // create variables for vector x
        val x = Array<IntVar>(n) { i ->
            model.intVar("x_$i", 0, upperBounds[i].coerceAtLeast(1))
        }

        // add constraints: x * A = B
        // for each column j in A, sum(x[i] * A[i][j]) = B[j]
        for (j in 0 until m) {
            val column = IntArray(n) { i -> switchesMatrix[i][j] }

            // create scalar product constraint: sum(x[i] * column[i]) = B[j]
            model.scalar(x, column, "=", joltagesVector[j]).post()
        }

        // minimize sum of all elements in x
        val sumUpperBound = upperBounds.sum()
        val sum = model.intVar("sum", 0, sumUpperBound)
        model.sum(x, "=", sum).post()
        model.setObjective(Model.MINIMIZE, sum)

        val solver = model.solver
        solver.setSearch(
            intVarSearch(
                FirstFail(model),
                IntDomainMin(),
                *x
            )
        )

        var bestSolution: Solution? = null

        while (solver.solve()) {
            bestSolution = Solution(
                x = x.map { it.value }.toIntArray(),
                sum = sum.value,
                isOptimal = true
            )
        }

        return bestSolution
    }

    data class Solution(
        val x: IntArray,
        val sum: Int,
        val isOptimal: Boolean
    ) {
        override fun toString(): String {
            return "Solution(x=${x.contentToString()}, sum=$sum, optimal=$isOptimal)"
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (other !is Solution) return false
            return x.contentEquals(other.x) && sum == other.sum
        }

        override fun hashCode(): Int {
            return 31 * x.contentHashCode() + sum
        }
    }
}

private fun parseInput(input: Iterable<String>): List<JMachine> {
    val machines = mutableListOf<JMachine>()
    for (line in input) {
        val numLights = line.substringAfter('[').substringBefore(']').length
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
        val switches = buckets.map(String::commaSeparatedNums).toList().map { it.toIntArray() }

        val joltages = line
            .substringAfter('{')
            .substringBefore('}')
            .commaSeparatedNums()
            .toIntArray()

        machines.add(JMachine(numLights, switches, joltages))

    }

    return machines
}
private var completed = AtomicInt(0)
private fun minimalPresses(machine: JMachine): Long {
    val matrix = Array<IntArray>(machine.switches.size) { i ->
        val row = IntArray(machine.numLights)
        for (num in machine.switches[i]) {
            row[num] = 1
        }
        row
    }

    val solver = ConstraintSolver(matrix, machine.joltages)
    val optimalSolution = solver.solve()
    val count = completed.incrementAndGet()
    println("Finished $count problems")
    if (optimalSolution != null) {
        return optimalSolution.sum.toLong()
    } else {
        println("No solution found!")
        return 0L
    }
}


private fun part2(input: List<String>): Long {
    val machines = parseInput(input)
    return machines
        .parallelStream()
        .map(::minimalPresses)
        .collect(Collectors.summingLong { it })
}

// WARNING: takes ~10 mins to run
fun main() {
    val testInput = readInput("Day10_test")
    val input = readInput("Day10")

    measureTimeMillis {
        println("Part 2 Test: ${part2(testInput)}")
        println("Part 2: ${part2(input)}")
    }.let {
        println("Took $it ms")
    }
}