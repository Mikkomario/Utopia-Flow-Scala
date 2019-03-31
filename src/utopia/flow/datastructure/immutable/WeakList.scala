package utopia.flow.datastructure.immutable

import scala.ref.WeakReference
import scala.collection.immutable.Traversable

/**
* This list only weakly references items, which means that they may disappear once no other object 
* is referencing them. The class is immutable from the outside, but the list contents may vary 
* based on time so this class doens't have value semantics.
* @author Mikko Hilpinen
* @since 31.3.2019
**/
class WeakList[T <: AnyRef](private val refs: Vector[WeakReference[T]]) extends Traversable[T]
{
    // COMPUTED    -----------------
    
    def strong = refs.flatMap { _.get }
    
    private def filteredRefs = refs.filter { _.get.isDefined }
    
    
    // IMPLEMENTED    --------------
    
    def foreach[U](f: T => U) = refs.foreach { _.get.foreach(f) }
    
    
    // OPERATORS    ----------------
    
    def +(item: T) = new WeakList(filteredRefs :+ WeakReference(item))
    
    def ++(items: TraversableOnce[T]) = new WeakList(filteredRefs ++ items.map { WeakReference(_) })
}