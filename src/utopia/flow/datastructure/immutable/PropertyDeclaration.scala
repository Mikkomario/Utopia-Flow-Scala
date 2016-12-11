package utopia.flow.datastructure.immutable

import utopia.flow.generic.DataType
import utopia.flow.util.Equatable

/**
 * Property declarations are used for defining and instantiating model properties
 * @author Mikko Hilpinen
 * @since 1.12.2016
 */
class PropertyDeclaration(val name: String, val dataType: DataType, 
        val defaultValue: Option[Value] = None) extends Equatable
{
    // COMP. PROPERTIES    ------------
    
    def properties = Vector(name, dataType, defaultValue)
    
    
    // CONSTRUCTOR OVERLOAD    --------
    
    def this(name: String, defaultValue: Value) = this(name, defaultValue.dataType, Some(defaultValue))
}