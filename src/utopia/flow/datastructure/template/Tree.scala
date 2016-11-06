package utopia.flow.datastructure.template

/**
 * Tree nodes form individual trees. They can also be used as subtrees in other tree nodes. Like 
 * other nodes, treeNodes contain / wrap certain type of content. A tree node can never contain 
 * itself below itself.
 * @param content The contents of this node
 * @author Mikko Hilpinen
 * @since 1.11.2016
 */
trait Tree[T, ChildType <: Tree[T, ChildType]] extends Node[T]
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
    
    
    // OPERATORS    ----------------
    
    /**
     * Finds a child directly under this node that has the provided content
     * @param content The searched content of the child
     * @returns The first child with the provided content or None if there is no direct child with
     * such content
     */
    def apply(content: T) = children.find { _.content == content }
    
    
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
    def contains(node: Tree[_, _]): Boolean = { children.contains(node) || 
            children.find({ child => child.contains(node) }).isDefined }
}