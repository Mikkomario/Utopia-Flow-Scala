package utopia.flow.generic

import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.Value
import utopia.flow.util.Equatable

/**
 * This constant generator uses a single default value for all new constants
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
class SimpleConstantGenerator[T <: Constant](val defaultValue: Value, 
        val createConstant: (String, Value) => T = new Constant(_, _)) extends ConstantGenerator[T] 
        with Equatable
{
    def properties = Vector(defaultValue, createConstant)
    
    /**
     * Creates a new constant with the provided name. Will always succeed.
     * @param constantName The name of the constant
     * @return A constant with the provided name and the generator's default value
     */
    def apply(constantName: String) = Some(createConstant(constantName, defaultValue))
}