package utopia.flow.generic

import utopia.flow.datastructure.immutable.PropertyDeclaration
import utopia.flow.datastructure.immutable.Value
import utopia.flow.datastructure.immutable.ModelDeclaration
import utopia.flow.datastructure.mutable.Variable
import utopia.flow.util.Equatable

/**
 * This variable generator uses property declarations when generating new variables
 * @author Mikko Hilpinen
 * @since 11.12.2016
 */
class DeclarationVariableGenerator[T <: Variable](val declarations: ModelDeclaration, 
        val defaultValue: Option[Value] = None, 
        val createVariable: (String, Value) => T = new Variable(_, _)) extends VariableGenerator[T] 
        with Equatable
{
    override def properties = Vector(declarations, defaultValue, createVariable)
    
    override def apply(variableName: String, value: Option[Value] = None) = 
    {
        val declaration = declarations.find(variableName)
        if (declaration.isDefined)
        {
            try
            {
                val newValue = value.orElse(declaration.get.defaultValue).orElse(defaultValue).map { 
                    _ withType declaration.get.dataType }
                newValue.map { createVariable(variableName, _) }
            }
            catch
            {
                case _: DataTypeException => None
            }
        }
        else
        {
            value.orElse(defaultValue).map { createVariable(variableName, _) }
        }
    }
}