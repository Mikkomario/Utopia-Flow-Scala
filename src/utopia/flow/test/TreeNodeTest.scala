package utopia.flow.test

import utopia.flow.datastructure.TreeNode

object TreeNodeTest
{
    def main(args: Array[String]): Unit = 
    {
        val root = new TreeNode(1)
        val bottomNode = new TreeNode(4)
        root += new TreeNode(2, new TreeNode(3, bottomNode), new TreeNode(5))
        val secondChild = new TreeNode(6)
        root += secondChild
        
        assert(root.children.size == 2)
        assert(root.nodesBelow.contains(bottomNode))
        assert(root.contains(bottomNode))
        assert(root.contains(secondChild))
        assert(!root.isEmpty)
        assert(root.size == 5)
        assert(root.depth == 3)
        
        root -= secondChild
        assert(root.children.size == 1)
        
        root.clear()
        assert(root.isEmpty)
        
        println("Success")
    }
}