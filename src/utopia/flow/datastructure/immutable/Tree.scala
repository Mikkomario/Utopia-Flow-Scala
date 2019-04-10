package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template

object Tree
{
    /*
    def copy[T, TreeType <: template.Tree[T, TreeType]](tree: TreeType): Tree[T] = 
    {
        new Tree(tree.content, tree.children.map { child => copy[T, TreeType](child) })
    }*/
}

/**
 * This TreeNode implementation is immutable and safe to reference from multiple places
 * @author Mikko Hilpinen
 * @since 4.11.2016
 */
case class Tree[T](override val content: T, override val children: Vector[Tree[T]] = Vector()) extends template.Tree[T, Tree[T]]
{
    // IMPLEMENTED    ---------------
    
    override protected def makeNode(content: T) = Tree(content)
    
    
    // OPERATORS    ----------------
    
    /**
     * Creates a new tree that contains a new child node
     * @param tree The child node in the new tree
     * @return a copy of this tree with the provided child tree
     */
    def +(tree: Tree[T]) = new Tree(content, children :+ tree)
    
    /**
      * Creates a new tree that contains a child node with specified content
      * @param nodeContent Child node content
      * @return A copy of this tree with child node added
      */
    def +(nodeContent: T): Tree[T] = this + makeNode(nodeContent)
    
    /**
     * Creates a new copy of this tree where the provided tree doesn't occur. Removes the element from even child
      * trees
     * @param tree The tree that is not included in the copy
     * @return A copy of this tree without the provided tree
     */
    def -(tree: Tree[T]): Tree[T] = Tree(content, children.filterNot { _ == tree } map { _ - tree })
    
    /**
      * Creates a new copy of this tree where specified content never occurs. Removes the content from every child,
      * including grandchildren etc.
      * @param removedContent The content to be removed
      * @return A tree without the specified content
      */
    def -(removedContent: T): Tree[T] = filterContents { _ != removedContent }
    
    
    // OTHER METHODS    -------------
    
    /**
     * Creates a new copy of this tree without the provided direct child node
     * @param child The child node that is removed from the direct children under this tree
     */
    def withoutChild(child: Tree[T]) = new Tree(content, children.filterNot { _ == child })
    
    /**
      * Filters all content in this tree
      * @param f A filter function
      * @return A new tree with all nodes filtered
      */
    def filterContents(f: T => Boolean): Tree[T] = Tree(content, children.filter { c => f(c.content) } map { _.filterContents(f) })
    
    /**
      * Filters the direct children of this tree
      * @param f A filter function
      * @return A filtered version of this tree
      */
    def filterChildren(f: Tree[T] => Boolean): Tree[T] = Tree(content, children.filter(f))
    
    /**
      * Maps the content of all nodes in this tree
      * @param f A mapping function
      * @tparam B Target content type
      * @return A mapped version of this tree
      */
    def map[B](f: T => B): Tree[B] = Tree(f(content), children.map { _.map(f) })
    
    /**
      * Maps the content of all nodes in this tree. May map to None where the node is removed.
      * @param f A mapping function
      * @tparam B Target content type
      * @return A mapped version of this tree. None if the content of this node mapped to None.
      */
    def flatMap[B](f: T => Option[B]): Option[Tree[B]] = f(content).map { c => Tree(c, children.flatMap { _.flatMap(f) }) }
}