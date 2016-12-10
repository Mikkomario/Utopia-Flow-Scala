package utopia.flow.datastructure.mutable

import utopia.flow.datastructure.template.Property
import utopia.flow.datastructure.immutable.Value
import utopia.flow.datastructure.immutable.Constant

/**
 * A variable is a property whose value can be changed
 * @author Mikko Hilpinen
 * @since 27.11.2016
 * @param name The name of the variable. Immutable
 * @param initialContent The initial content of the variable. This determines the data type of the 
 * variable
 */
class Variable(val name: String, initialContent: Value) extends Property
{
    // ATTRIBUTES    -----------------
    
    private var _content = initialContent
    def content = _content
    def content_=(value: Value) = _content = value.withType(dataType)
    
    
    // COMP. PROPERTIES    -----------
    
    override def dataType = content.dataType
    
    /**
     * This variable as a constant copy
     */
    def toConstant = new Constant(name, content)
}