package utopia.flow.generic

import utopia.flow.datastructure.template.Node

/**
 * Values can wrap an object value and associate it with a certain data type. Values can be cast 
 * to different data types. They are immutable.
 */
class Value(val content: Any, val dataType: DataType) extends Node[Any]
{
    // INITIAL CODE    ---------
    
    // The content must be of correct type
    require(dataType.isInstance(content), s"$content is not of type $dataType")
    
    
}