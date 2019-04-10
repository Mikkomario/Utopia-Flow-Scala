package utopia.flow.test

import utopia.flow.datastructure.mutable.Tree

object TreeNodeTest extends App
{
    private def basicCheck(tree: utopia.flow.datastructure.template.Tree[_, _]) =
    {
        assert(tree.children.size == 2)
        assert(!tree.isEmpty)
        assert(tree.size == 5)
        assert(tree.depth == 3)
    }
    
    val root = Tree(1)
    val bottomNode = Tree(4)
    root += Tree(2, Tree(3, bottomNode), Tree(5))
    val secondChild = Tree(6)
    root += secondChild
    
    basicCheck(root)
    println(root)
    
    assert(root(2)(3).children.size == 1)
    assert(root(5).children.isEmpty)
    
    // Creates an immutable copy of the tree
    val copy = root.immutableCopy
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
    
    println("Success")
}