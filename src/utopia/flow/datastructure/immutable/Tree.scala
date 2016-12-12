package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template
import utopia.flow.util.Equatable

object Tree
{   
    def copy[T, TreeType <: template.Tree[T, TreeType]](tree: TreeType): Tree[T] = 
    {
        new Tree(tree.content, tree.children.map { child => copy[T, TreeType](child) })
    }
}

/**
 * This TreeNode implementation is immutable and safe to reference from multiple places
 * @author Mikko Hilpinen
 * @since 4.11.2016
 */
class Tree[T](val content: T, val children: Vector[Tree[T]]) extends 
        template.Tree[T, Tree[T]] with Equatable
{
    // COMP. PROPERTIES    ---------
    
    override def properties = children :+ content
    
    
    // OPERATORS    ----------------
    
    /**
     * Creates a new tree that contains a new child node
     * @param tree The child node in the new tree
     * @return a copy of this tree without the provided tree
     */
    def +(tree: Tree[T]) = new Tree(content, children :+ tree)
    
    /**
     * Creates a new copy of this tree where the provided tree doesn't occur once
     * @param tree The tree that is not included in the copy
     * @return A copy of this tree without the provided tree
     */
    def -(tree: Tree[T]): Tree[T] = new Tree(content, children.filterNot { _ == tree } map { _ - tree })
    
    
    // OTHER METHODS    -------------
    
    /**
     * Creates a new copy of this tree without the provided direct child node
     * @param child The child node that is removed from the direct children under this tree
     */
    def withoutChild(child: Tree[T]) = new Tree(content, children.filterNot { _ == child })
}