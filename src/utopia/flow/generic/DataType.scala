package utopia.flow.generic

import scala.collection.immutable.HashSet
import utopia.flow.datastructure.mutable.Tree


/**
 * Any type is the superType for all other types
 * @see Any
 */
object AnyType extends DataType("Any", None)
/**
 * String type stands for strings which are below Any
 * @see String
 */
object StringType extends DataType("String", Some(AnyType))


object DataType
{
    private var _values = HashSet[DataType]()
    def values = _values
}

/**
 * A data types specifies the internal type of a value
 * @author Mikko Hilpinen
 * @since 4.11.2016
 */
class DataType(val name: String, val superType: Option[DataType])
{
    private val tree = new Tree(this)
    
    // INITIAL CODE    -----------
    
    // Introduces the data type
    DataType._values += this
    superType.foreach { superType => superType.tree += this.tree }
    
    
    // IMPLEMENTED METHODS    ----
    
    override def toString: String = name
    
    
    // OTHER METHODS    ----------
    
    /**
     * Finds out whether this data type is a subType of another data type
     */
    def isOfType(other: DataType): Boolean = {superType.contains(other) || 
        superType.forall { t => t.isOfType(other) }}
}