package utopia.flow.generic

import utopia.flow.datastructure.immutable.Value
import utopia.flow.datastructure.mutable.Variable
import utopia.flow.util.Equatable

/**
 * This simple variable generator implementation is able to create variables using a possible 
 * default value
 * @author Mikko Hilpinen
 * @since 1.12.2016
 */
case class SimpleVariableGenerator[T <: Variable](val defaultValue: Option[Value] = None, 
        createVariable: (String, Value) => T = new Variable(_, _)) extends VariableGenerator[T] 
           with Equatable
{
    def properties = Vector(defaultValue, createVariable)
    
    /**
     * Generates a new variable. Variable creation will succeed if either a) 'value' is not None or 
     * b) generator's default value has been defined
     * @param varName The name of the new variable
     * @param value the value assigned to the variable (optional)
     * @return A generated variable or None if no value could be determined
     */
    def apply(varName: String, value: Option[Value]) = value.orElse(defaultValue).flatMap { 
        value => Some(createVariable(varName, value)) }
}