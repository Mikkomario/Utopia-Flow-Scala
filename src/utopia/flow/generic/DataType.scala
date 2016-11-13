package utopia.flow.generic

import scala.collection.immutable.HashSet
import utopia.flow.datastructure.mutable.Tree


/**
 * Any type is the superType for all other types
 * @see Any
 */
object AnyType extends DataType("Any", classOf[Any], None)
/**
 * String type stands for strings which are below Any
 * @see String
 */
object StringType extends DataType("String", classOf[String])


object DataType
{
    // ATTRIBUTES    ----------
    
    private var _values = HashSet[DataType]()
    /**
     * Each data type ever created
     */
    def values = _values
    
    
    // INITIAL CODE    --------
    
    introduceTypes(AnyType, StringType)
    
    
    // OTHER METHODS    -------
    
    /**
     * Introduces a number of new data types to the data type interface. Each data type created
     * outside the Flow project should be introduced this way.
     */
    def introduceTypes(types: DataType*) = 
    {
        _values ++= types
        // Also introduces super type handling
        ConversionHandler.addCaster(new SuperTypeCaster(types.toSet))
    }
}

/**
 * A data types specifies the internal type of a value
 * @author Mikko Hilpinen
 * @since 4.11.2016
 * @param name The name of the data type. Used when printing the type information. Should start 
 * with a upper case letter. Eg. "String"
 * @param superType The type above this data type. All DataTypes should have a super type, except 
 * Any, which is the highest data type. The value defaults to Any but in some cases you want to 
 * specify a different type like Number, for example.
 */
class DataType(val name: String, val supportedClass: Class[_], val superType: Option[DataType] = Some(AnyType))
{
    private val tree = new Tree(this)
    
    // INITIAL CODE    -----------
    
    superType.foreach { _.tree += this.tree }
    
    
    // COMPUTED PROPERTIES    ----
    
    /**
     * The type hierarchy of this data type where the data type is the root node and the subtypes
     * are below that
     */
    def typeHierarchy = tree.immutableCopy
    
    /**
     * All types deriving from this data type
     */
    def subTypes = typeHierarchy.nodesBelow.map { _.content }
    
    
    // IMPLEMENTED METHODS    ----
    
    override def toString: String = name
    
    
    // OTHER METHODS    ----------
    
    /**
     * Finds out whether this data type is a subType of another data type
     */
    def isOfType(other: DataType): Boolean = {superType.contains(other) || superType.forall { _.isOfType(other) }}
    
    /**
     * Checks whether this data type supports an instance
     * @param instance An instance that may or may not be of the supported type
     * @return Whether the provided value is an instance of this data type
     */
    def isInstance(instance: Any) = supportedClass.isInstance(instance)
}