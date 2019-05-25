package utopia.flow.datastructure.mutable

import utopia.flow.event.{ChangeEvent, ChangeListener}

/**
  * Classes with this trait generate change events when they mutate
  * @author Mikko Hilpinen
  * @since 25.5.2019, v1.4.1+
  */
class PointerWithEvents[A](initialValue: A) extends PointerLike[A]
{
	// ATTRIBUTES	----------------
	
	private var _value = initialValue
	
	/**
	  * The listeners interested in this mutable item's changes
	  */
	var listeners = Vector[ChangeListener[A]]()
	
	
	// COMPUTED	--------------------
	
	/**
	  * @return The current value in this mutable
	  */
	def value = _value
	
	/**
	  * @param newValue The new value in this mutable
	  */
	def value_=(newValue: A) =
	{
		if (_value != newValue)
		{
			val oldValue = _value
			_value = newValue
			val event = ChangeEvent(oldValue, newValue)
			listeners.foreach { _.onChangeEvent(event) }
		}
	}
	
	
	// IMPLEMENTED	----------------
	
	override def get = value
	
	override def set(newVal: A) = value = newVal
	
	
	// OTHER	--------------------
	
	/**
	  * Adds a new listener to this mutable
	  * @param changeListener A change listener that will be informed when the value of this mutable changes
	  */
	def addListener(changeListener: ChangeListener[A]) = listeners :+= changeListener
	
	/**
	  * Removes a listener from the informed change listeners
	  * @param listener A listener that will be removed
	  */
	def removeListener(listener: Any) = listeners = listeners.filterNot { _ == listener }
}