package utopia.flow.util

import collection.{SeqLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.util.Try

/**
* This object contains some extensions for the more traditional collections / data structures
* @author Mikko Hilpinen
* @since 10.10.2018
**/
object CollectionExtensions
{
    implicit class RichSeq[A](val s: Seq[A]) extends AnyVal
    {
        /**
          * Same as apply except returns None on non-existing indices
          * @param index Target index
          * @return Value from index or None if no such index exists
          */
        def getOption(index: Int) = if (s.isDefinedAt(index)) Some(s(index)) else None
        
        /**
          * Same as apply except returns a default value on non-existing indices
          * @param index Target index
          * @param default Default value
          * @return Value from index or default value if no such index exists
          */
        def getOrElse(index: Int, default: => A) = if (s.isDefinedAt(index)) s(index) else default
    }
    
    implicit class RichSeqLike[A, Repr](val seq: SeqLike[A, Repr]) extends AnyVal
    {
        /**
         * Finds the index of the first item that matches the predicate
         * @param find a function for finding the correct item
         * @return The index of the item in this seq or None if no such item was found
         */
        def indexWhereOption(find: A => Boolean) =
        {
            val result = seq.indexWhere(find)
            if (result < 0)
                None
            else
                Some(result)
        }
    
        /**
          * Filters a seq so that only distinct values remain. Uses a special function to determine equality
          * @param equals A function that determines whether two values are equal
          * @param cbf A canbuildfrom (implicit) to build the final collection
          * @tparam To The type of final collection
          * @return A collection with only distinct values (when considering the provided 'equals' function)
          */
        def distinctWith[To](equals: (A, A) => Boolean)(implicit cbf: CanBuildFrom[_, A, To]) =
        {
            val builder = cbf.apply()
            val collected = mutable.HashSet[A]()
            
            seq.foreach
            {
                item =>
                    if (!collected.exists { equals(_, item) })
                    {
                        collected += item
                        builder += item
                    }
            }
            
            builder.result()
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
    
        /**
          * Finds the maximum value in this traversable
          * @param cmp Ordering (implicit)
          * @tparam B Ordering type
          * @return Maximum item or None if this traversable was empty
          */
        def maxOption[B >: A](implicit cmp: Ordering[B]): Option[A] =
        {
            if (t.isEmpty)
                None
            else
                Some(t.max(cmp))
        }
        
        /**
          * Finds the minimum value in this traversable
          * @param cmp Ordering (implicit)
          * @tparam B Ordering type
          * @return Minimum item or None if this traversable was empty
          */
        def minOption[B >: A](implicit cmp: Ordering[B]): Option[A] =
        {
            if (t.isEmpty)
                None
            else
                Some(t.min(cmp))
        }
    }
    
    implicit class RichMap[K, V](val m: Map[K, V]) extends AnyVal
    {
        /**
          * Merges this map with another map. If value is present only in one map, it is preserved as is.
          * @param another Another map
          * @param merge A merge function used when both maps contain a value
          * @tparam V2 The resulting value type
          * @return A map with merged values
          */
        def mergedWith[V2 >: V](another: Map[K, V2], merge: (V, V2) => V2) =
        {
            val myKeys = m.keySet
            val theirKeys = another.keySet
            val onlyInMe = myKeys.diff(theirKeys)
            val onlyInThem = theirKeys.diff(myKeys)
            val inBoth = myKeys.intersect(theirKeys)
            
            val myPart = onlyInMe.map { k => k -> m(k) }.toMap
            val theirPart = onlyInThem.map { k => k -> another(k) }.toMap
            val ourPart = inBoth.map { k => k -> merge(m(k), another(k)) }.toMap
            
            myPart ++ theirPart ++ ourPart
        }
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