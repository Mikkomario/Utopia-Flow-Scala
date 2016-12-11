package utopia.flow.generic

import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.ModelDeclaration
import utopia.flow.datastructure.immutable.Value
import utopia.flow.util.Equatable

class DeclarationConstantGenerator[T <: Constant](val declarations: ModelDeclaration, 
        val generateOnlyDeclared: Boolean = false, val defaultValue: Option[Value] = None, 
        val createConstant: (String, Value) => T = new Constant(_, _)) extends ConstantGenerator[T] 
        with Equatable
{
    override def properties = Vector(declarations, generateOnlyDeclared, defaultValue, createConstant)
    
    override def apply(constantName: String) = 
    {
        val declaration = declarations.find(constantName)
        if (declaration.isDefined)
        {
            // Uses the declaration's or generator's default value casted to a correct data type
            declaration.get.defaultValue.orElse(defaultValue).flatMap { 
                _.safeCast(declaration.get.dataType).map { createConstant(constantName, _) } }
        }
        else if (!generateOnlyDeclared && defaultValue.isDefined)
        {
            Some(createConstant(constantName, defaultValue.get))
        }
        else
        {
            None
        }
    }
}