package utopia.flow.datastructure.mutable

/**
* This is a simple structure for holding a single mutable value
* @author Mikko Hilpinen
* @since 23.3.2019
**/
case class Pointer[T](var value: T)
{
	override def toString() = value.toString()
}