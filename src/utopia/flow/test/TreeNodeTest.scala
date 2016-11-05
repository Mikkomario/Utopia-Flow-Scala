package utopia.flow.test

import utopia.flow.datastructure.mutable.Tree
import utopia.flow.datastructure.immutable

object TreeNodeTest
{
    def main(args: Array[String]): Unit = 
    {
        val root = new Tree(1)
        val bottomNode = new Tree(4)
        root += new Tree(2, new Tree(3, bottomNode), new Tree(5))
        val secondChild = new Tree(6)
        root += secondChild
        
        basicCheck(root)
        
        // Creates an immutable copy of the tree
        val copy = immutable.Tree.copy[Int, Tree[Int]](root)
        assert(copy != root)
        basicCheck(copy)
        
        assert(root.nodesBelow.contains(bottomNode))
        assert(root.contains(bottomNode))
        assert(root.contains(secondChild))
        
        root.removeChild(secondChild)
        assert(root.children.size == 1)
        
        root -= bottomNode
        assert(root.depth == 2)
        
        root.clear()
        assert(root.isEmpty)
        
        // Tests immutable treenode equality
        val copy2 = immutable.Tree.copy[Int, immutable.Tree[Int]](copy)
        assert(copy == copy2)
        assert(copy2 == copy)
        
        println("Success")
    }
    
    private def basicCheck(tree: utopia.flow.datastructure.template.Tree[_, _]) = 
    {
        assert(tree.children.size == 2)
        assert(!tree.isEmpty)
        assert(tree.size == 5)
        assert(tree.depth == 3)
    }
}