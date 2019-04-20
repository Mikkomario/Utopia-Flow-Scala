package utopia.flow.datastructure.immutable

import utopia.flow.datastructure.template.Property
import utopia.flow.util.Equatable
import utopia.flow.datastructure.mutable.Variable

/**
 * Constants are named properties whose value can't be changed
 * @author Mikko Hilpinen
 * @since 29.11.2016
 */
class Constant(val name: String, val value: Value) extends Property with Equatable
{
    // COMP. PROPERTIES    ---------
    
    override def dataType = value.dataType
    
    override def properties = Vector(name, value)
    
    
    // COMPUTED    -----------------
    
    /**
     * Converts this constant to a variable
     */
    def toVariable = new Variable(name, value)
    
    
    // OTHER METHODS    ------------
    
    /**
     * Creates a new constant that has the provided value but the same name
     * @param value the value the new constant will have
     */
    def withValue(value: Value) = new Constant(name, value)
}