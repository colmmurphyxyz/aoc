import javax.swing.tree.TreeNode

fun main() {
    class FileTreeNode(val name: String, val type: String, private val size: Int?, val parent: FileTreeNode?,
                       val children: MutableList<FileTreeNode> = mutableListOf<FileTreeNode>()) {

        override fun toString(): String {
            return "$type $name, size: ${getSize()}\t parent: ${parent?.name}"
        }

        fun depth(): Int {
            if (parent == null) return 0
            return 1 + parent.depth()
        }

        fun printTree() {
            println("${"\t" * depth()} ${toString()}")
            children.forEach { child ->
                child.printTree()
            }
        }
        fun getSize(): Int {
            if (type == "file") {
                return size!!
            }
            var s = 0
            for (child in children) {
                s += child.getSize()
            }
            return s
        }

        fun hasChild(childName: String): Boolean {
            for (child in children) {
                if (child.name == childName) {
                    return true
                }
            }
            return false
        }

        fun addChild(node: FileTreeNode) {
            children.add(node)
        }

        fun getChildByNameOrNull(childName: String): FileTreeNode? {
            for (child in children) {
                if (child.name == childName) {
                    return child
                }
            }
            return null
        }

    }

    fun part1(input: List<String>): Int {
        val fileTreeRoot = FileTreeNode("/", "dir", null, null)
        var currentDirectory = fileTreeRoot
        val directories = mutableListOf<FileTreeNode>(fileTreeRoot)
        var i = 1
        while (i < input.size) {
            val line = input[i]
            val components = line.split(" ")
            val command = components.slice(1 until components.size)
            if (command[0] == "cd") {
                if (command[1] == "..") {
                    // go back 1 directory
                    currentDirectory = currentDirectory.parent!!
                } else {
                    val dirExists = currentDirectory.hasChild(command[1])
                    if (dirExists) {
                        currentDirectory = currentDirectory.getChildByNameOrNull(command[1])!!
                    } else {
                        println("dir does not exist, creating now")
                        val newNode = FileTreeNode(command[1], "dir", null, currentDirectory)
                        currentDirectory.addChild(newNode)
                        currentDirectory = newNode
                        directories.add(newNode)
                    }
                }
            } else { // command[0] == "ls"
                var j = i + 1
                while (j < input.size && input[j][0] != '$') j += 1
                val fileList = input.slice((i+1) until j).map { it -> it.split(" ") }
                for (file in fileList) {
                    if (!currentDirectory.hasChild(file[1])) {
                        val newNode = FileTreeNode(
                            file[1],
                            if (file[0] == "dir") "dir" else "file",
                            if (file[0] == "dir") null else file[0].toInt(),
                            currentDirectory
                        )
                        currentDirectory.addChild(newNode)
                        if (newNode.type == "dir") {
                            directories.add(newNode)
                        }
                    }
                }
                i = j - 1
            }
            i += 1
        }
        var smallDirsSize = 0
        for (dir in directories) {
            if (dir.getSize() < 100_000) {
                smallDirsSize += dir.getSize()
            }
        }
        return smallDirsSize
    }

    fun part2(input: List<String>): Int {
        val fileTreeRoot = FileTreeNode("/", "dir", null, null)
        var currentDirectory = fileTreeRoot
        val directories = mutableListOf<FileTreeNode>(fileTreeRoot)
        var i = 1
        while (i < input.size) {
            val line = input[i]
            val components = line.split(" ")
            val command = components.slice(1 until components.size)
            if (command[0] == "cd") {
                if (command[1] == "..") {
                    // go back 1 directory
                    currentDirectory = currentDirectory.parent!!
                } else {
                    val dirExists = currentDirectory.hasChild(command[1])
                    if (dirExists) {
                        currentDirectory = currentDirectory.getChildByNameOrNull(command[1])!!
                    } else {
                        println("dir does not exist, creating now")
                        val newNode = FileTreeNode(command[1], "dir", null, currentDirectory)
                        currentDirectory.addChild(newNode)
                        currentDirectory = newNode
                        directories.add(newNode)
                    }
                }
            } else { // command[0] == "ls"
                var j = i + 1
                while (j < input.size && input[j][0] != '$') j += 1
                val fileList = input.slice((i+1) until j).map { it -> it.split(" ") }
                for (file in fileList) {
                    if (!currentDirectory.hasChild(file[1])) {
                        val newNode = FileTreeNode(
                            file[1],
                            if (file[0] == "dir") "dir" else "file",
                            if (file[0] == "dir") null else file[0].toInt(),
                            currentDirectory
                        )
                        currentDirectory.addChild(newNode)
                        if (newNode.type == "dir") {
                            directories.add(newNode)
                        }
                    }
                }
                i = j - 1
            }
            i += 1
        }

        val freeSpace = 70_000_000 - fileTreeRoot.getSize()
        val spaceToDelete = 30_000_000 - freeSpace
        println("need to delete a directory with size > $spaceToDelete")
        var smallestSuitableDirSize = Int.MAX_VALUE
        for (dir in directories) {
            val size = dir.getSize()
            if (size in (spaceToDelete + 1) until smallestSuitableDirSize) {
                smallestSuitableDirSize = size
            }
        }
        return smallestSuitableDirSize
    }

    val input = readInput("Day07")
    println("Part 1 answer: ${part1(input)}")
    println("Part 2 answer: ${part2(input)}")
}