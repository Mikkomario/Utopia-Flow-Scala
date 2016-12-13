package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template.Property
import utopia.flow.util.Equatable

/**
 * Constants are named properties whose value can't be changed
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Constant(val name: String, val content: Value) extends Property with Equatable
{
    // COMP. PROPERTIES    ---------
    
    override def dataType = content.dataType
    
    override def properties = Vector(name, content)
    
    
    // OTHER METHODS    ------------
    
    /**
     * Creates a new constant that has the provided value but the same name
     * @param value the value the new constant will have
     */
    def withValue(value: Value) = new Constant(name, value)
}