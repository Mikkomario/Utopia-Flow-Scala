package utopia.flow.datastructure.mutable

import utopia.flow.datastructure.template
import utopia.flow.datastructure.immutable

object Tree
{
    def apply[T](content: T, children: Vector[Tree[T]] = Vector()) = new Tree(content, children)
    
    def apply[T](content: T, child: Tree[T]) = new Tree(content, Vector(child))
    
    def apply[T](content: T, firstC: Tree[T], secondC: Tree[T], more: Tree[T]*) = new Tree(content,
        Vector(firstC, secondC) ++ more)
}

/**
 * Tree nodes form individual trees. They can also be used as subtrees in other tree nodes. Like 
 * other nodes, treeNodes contain / wrap certain type of content. A tree node can never contain 
 * itself below itself.
 * @param content The contents of this node
 * @author Mikko Hilpinen
 * @since 1.11.2016
 */
class Tree[T](var content: T, initialChildren: Vector[Tree[T]] = Vector()) extends template.Tree[T, Tree[T]]
{
    // ATTRIBUTES    -----------------
    
    private var _children = initialChildren
    
    
    // COMP. PROPERTIES    -----------
    
    /**
     * Creates an immutable copy of this tree
     * @return An immutable copy of this tree
     */
    def immutableCopy: immutable.Tree[T] = immutable.Tree(content, children.map { _.immutableCopy })
    
    
    // IMPLEMENTED PROPERTIES    -----
    
    def children = _children
    
    /**
      * @param content Content for the child node
      * @return A new node
      */
    override protected def makeNode(content: T) = new Tree(content)
    
    
    // OPERATORS    -----------------
    
    /**
     * Adds a treeNode directly under this node. The node won't be added if a) it already exists as 
     * a direct child of this node or b) This node exists under the provided node
     * @param child The node that is added under this node
     * @return Whether the node was successfully added under this node
     */
    def +=(child: Tree[T]) =
    {   
        // Makes sure the child doesn't already exist in the direct children
        // And that this node won't end up under a child node
        if (!children.contains(child) && !child.contains(this))
        {
            _children :+= child
            true
        }
        else
            false
    }
    
    /**
     * Removes a node from this tree. If it appears in multiple locations, all occurrences will be 
     * removed
     * @param node The node that is removed from under this node
     */
    def -=(node: Tree[T]): Unit = 
    {
        removeChild(node)
        children.foreach { child => child -= node }
    }
    
    
    // OTHER METHODS    ------------
    
    /**
     * Clears the node, removing any nodes below it
     */
    def clear() = _children = Vector()
    
    /**
     * Removes a node from the direct children under this node
     * @param child The node that is removed from under this node
     */
    def removeChild(child: Tree[T]) = _children = _children.filterNot { 
            existingChild => existingChild == child }
}