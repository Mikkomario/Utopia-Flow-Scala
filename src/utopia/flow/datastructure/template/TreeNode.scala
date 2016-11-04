package utopia.flow.datastructure.template

/**
 * Tree nodes form individual trees. They can also be used as subtrees in other tree nodes. Like 
 * other nodes, treeNodes contain / wrap certain type of content. A tree node can never contain 
 * itself below itself.
 * @param content The contents of this node
 * @author Mikko Hilpinen
 * @since 1.11.2016
 */
trait TreeNode[T, ChildType <: TreeNode[T, ChildType]] extends Node[T]
{
    // ATTRIBUTES    -----------------
    
    /**
     * The child nodes directly under this node
     */
    def children: Vector[ChildType]
    
    
    // COMPUTED PROPERTIES    --------
    
    /**
     * All nodes below this node, in no specific order
     */
    def nodesBelow: Vector[ChildType] = children ++ children.flatMap { child => child.nodesBelow }
    
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
    
    
    // OTHER METHODS    ------------
    
    def find(filter: ChildType => Boolean): Option[ChildType] =
    {
        children.foldLeft(children.find(filter))({(found, child) => 
            if (found.isDefined) {found}
            else {child.find(filter)}
        })
    }
    
    /**
     * Checks whether a node exists below this node
     * @param node A node that may exist below this node
     * @return Whether the provided node exists below this node
     */
    def contains(node: TreeNode[_, _]): Boolean = { children.contains(node) || 
            children.find({ child => child.contains(node) }).isDefined }
}