package utopia.flow.datastructure.immutable

import scala.ref.WeakReference
import scala.collection.immutable.Traversable
import scala.collection.Iterable
import scala.collection.IterableLike
import scala.collection.mutable.Builder
import scala.collection.immutable.VectorBuilder

object WeakList
{
    def apply[A <: AnyRef]() = new WeakList[A](Vector())    
}

/**
* This list only weakly references items, which means that they may disappear once no other object 
* is referencing them. The class is immutable from the outside, but the list contents may vary 
* based on time so this class doens't have value semantics.
* @author Mikko Hilpinen
* @since 31.3.2019
**/
class WeakList[A <: AnyRef](private val refs: Vector[WeakReference[A]]) extends IterableLike[A, WeakList[A]]
{
    // COMPUTED    -----------------
    
    def strong = refs.flatMap { _.get }
    
    
    // IMPLEMENTED    --------------
    
    def iterator = refs.view.flatMap { _.get }.iterator
    
    def seq = this
    
    override def foreach[U](f: A => U) = refs.foreach { _.get.foreach(f) }
    
    protected[this] def newBuilder = new WeakListBuilder()
}

class WeakListBuilder[A <: AnyRef] extends Builder[A, WeakList[A]]
{
    // ATTRIBUTES    ---------------------
    
    private val builder = new VectorBuilder[WeakReference[A]]()
    
    
    // IMPLEMENTED    --------------------
    
    def +=(elem: A): WeakListBuilder.this.type = 
    {
        builder += WeakReference(elem)
        this
    }
    
    def clear() = builder.clear()
    
    def result() = new WeakList(builder.result())
}