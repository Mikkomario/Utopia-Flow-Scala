package utopia.flow.generic

import utopia.flow.datastructure.immutable.Constant
import utopia.flow.datastructure.immutable.ModelDeclaration
import utopia.flow.datastructure.immutable.Value

class DeclarationConstantGenerator(declaration: ModelDeclaration, 
        defaultValue: Value = Value.empty()) extends 
        DeclarationPropertyGenerator(new Constant(_, _), declaration, defaultValue)
{
    override def properties = Vector(declaration, defaultValue)    
}