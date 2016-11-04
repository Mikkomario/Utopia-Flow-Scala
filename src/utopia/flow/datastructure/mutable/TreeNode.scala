package utopia.flow.datastructure.mutable

import scala.Vector
import utopia.flow.datastructure.template.Node

/**
 * Tree nodes form individual trees. They can also be used as subtrees in other tree nodes. Like 
 * other nodes, treeNodes contain / wrap certain type of content. A tree node can never contain 
 * itself below itself.
 * @param content The contents of this node
 * @author Mikko Hilpinen
 * @since 1.11.2016
 */
class TreeNode[T](var content: T) extends utopia.flow.datastructure.template.TreeNode[T, TreeNode[T]]
{
    // ATTRIBUTES    -----------------
    
    private var _children = Vector[TreeNode[T]]()
    
    
    // IMPLEMENTED PROPERTIES    -----
    
    def children = _children
    
    
    // CONSTRUCTOR OVERLOAD    -------
    
    /**
     * Creates a new node with existing child nodes
     * @param content The content assigned to this node
     * @param children The children directly under this node
     */
    def this(content: T, children: TreeNode[T]*) = {this(content); _children ++= children}
    
    
    // OPERATORS    -----------------
    
    /**
     * Adds a treeNode directly under this node. The node won't be added if a) it already exists as 
     * a direct child of this node or b) This node exists under the provided node
     * @param child The node that is added under this node
     * @return Whether the node was successfully added under this node
     */
    def +=(child: TreeNode[T]) =
    {   
        // Makes sure the child doesn't already exist in the direct children
        // And that this node won't end up under a child node
        if (!children.contains(child) && !child.contains(this))
        {
            _children :+= child
            true
        }
        else
        {
            false
        }
    }
    
    /**
     * Removes a node from this tree. If it appears in multiple locations, all occurrences will be 
     * removed
     * @param node The node that is removed from under this node
     */
    def -=(node: TreeNode[T]): Unit = 
    {
        removeChild(node)
        children.foreach { child => child -= node }
    }
    
    
    // OTHER METHODS    ------------
    
    /**
     * Clears the node, removing any nodes below it
     */
    def clear() = _children = Vector[TreeNode[T]]()
    
    /**
     * Removes a node from the direct children under this node
     * @param child The node that is removed from under this node
     */
    def removeChild(child: TreeNode[T]) = _children = _children.filterNot { 
            existingChild => existingChild == child }
}