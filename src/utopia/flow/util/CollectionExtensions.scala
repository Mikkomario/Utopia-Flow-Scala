package utopia.flow.util

import collection.mutable

import scala.collection.generic.CanBuildFrom
import scala.collection.GenSeqLike
import scala.util.Try

/**
* This object contains some extensions for the more traditional collections / data structures
* @author Mikko Hilpinen
* @since 10.10.2018
**/
object CollectionExtensions
{
    implicit class RichGenSeqLike[T, Repr](val seq: GenSeqLike[T, Repr]) extends AnyVal
    {
        /**
         * Finds the index of the first item that matches the predicate
         * @param find a function for finding the correct item
         * @return The index of the item in this seq or None if no such item was found
         */
        def indexWhereOption(find: T => Boolean) = 
        {
            val result = seq.indexWhere(find)
            if (result < 0)
                None
            else
                Some(result)
        }
    }
    
    implicit class RichTry[T](val t: Try[T]) extends AnyVal
    {
        /**
         * The success value of this try. None if this try was a failure
         */
        def success = t.toOption
        /**
         * The failure (throwable) value of this try. None if this try was a success.
         */
        def failure = t.failed.toOption
    }
    
    implicit class RichTraversable[A](val t: Traversable[A]) extends AnyVal
    {
        /**
          * Maps items until a concrete result is found, then returns that result
          * @param map A mapping function that maps to either Some or None
          * @tparam B The map target type
          * @return The first item that was mapped to Some. None if all items were mapped to None.
          */
        def findMap[B](map: A => Option[B]) = t.view.map(map).find { _.isDefined }.flatten
    }
    
    /**
     * This implicit class is used for extending collection map conversion
     */
    implicit class MultiMapConvertible[T](val list: TraversableOnce[T]) extends AnyVal
    {
        // Referenced from: https://stackoverflow.com/questions/22090371/scala-grouping-list-of-tuples [10.10.2018]
        def toMultiMap[Key, Value, Values](key: T => Key, value: T => Value)
                (implicit cbfv: CanBuildFrom[Nothing, Value, Values]): Map[Key, Values] = 
        {
            val b = mutable.Map.empty[Key, mutable.Builder[Value, Values]]
	        list.foreach { elem => b.getOrElseUpdate(key(elem), cbfv()) += value(elem) }
	        b.map { case (k, vb) => (k, vb.result()) } (collection.breakOut)
        }
    }
    
    /**
     * This extension allows tuple lists to be transformed into multi maps directly
     */
    implicit class RichTupleList[K, V](val list: TraversableOnce[Tuple2[K, V]]) extends AnyVal
    {
        def toMultiMap[Values]()(implicit cbfv: CanBuildFrom[Nothing, V, Values]): Map[K, Values] = 
        {
            new MultiMapConvertible(list).toMultiMap(_._1, _._2)
        }
    }
}