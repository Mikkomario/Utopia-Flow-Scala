package utopia.flow.parse

object ReadMode extends Enumeration
{
    type ReadMode = Value
    val NONE, MODEL, ARRAY, PROPERTY, PROPERTY_NAME, VALUE = Value
}
