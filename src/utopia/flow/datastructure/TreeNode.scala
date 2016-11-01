package utopia.flow.datastructure

/**
 * Tree nodes form individual trees. They can also be used as subtrees in other tree nodes. Like 
 * other nodes, treeNodes contain / wrap certain type of content. A tree node can never contain 
 * itself below itself.
 * @param content The contents of this node
 * @author Mikko Hilpinen
 * @since 1.11.2016
 */
class TreeNode[T](var content: T) extends Node[T]
{
    // ATTRIBUTES    -----------------
    
    private var _children = Vector[TreeNode[T]]()
    
    /**
     * The child nodes directly under this node
     */
    def children = _children
    
    
    // COMPUTED PROPERTIES    --------
    
    /**
     * All nodes below this node, in no specific order
     */
    def nodesBelow: Vector[TreeNode[T]] = children ++ children.flatMap { child => child.nodesBelow }
    
    /**
     * The size of this tree. In other words, the number of nodes below this node
     */
    def size: Int = children.foldLeft(children.size)((size, child) => size + child.size)
    
    /**
     * Whether this tree is empty and doesn't contain a single node below it
     */
    def isEmpty = children.isEmpty
    
    /**
     * The depth of this tree. A tree with no children has depth of 0, a tree with only direct 
     * children has depth of 1, a tree with grand children has depth of 2 and so on.
     */
    def depth: Int = children.foldLeft(0)((maxDepth, child) => math.max(maxDepth, 1 + child.depth))
    
    
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
     * Removes a node from the direct children under this node
     * @param child The node that is removed from under this node
     */
    def -=(child: TreeNode[T]) = _children = _children.filterNot { existingChild => existingChild == child }
    
    
    // OTHER METHODS    ------------
    
    /**
     * Checks whether a node exists below this node
     * @param node A node that may exist below this node
     * @return Whether the provided node exists below this node
     */
    def contains(node: TreeNode[_]): Boolean = { children.contains(node) || 
            children.find({ child => child.contains(node) }).isDefined }
    
    /**
     * Clears the node, removing any nodes below it
     */
    def clear() = _children = Vector[TreeNode[T]]()
}