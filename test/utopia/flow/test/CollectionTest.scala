package utopia.flow.test

import utopia.flow.util.CollectionExtensions._

/**
  * A test for Flow collections / collection extensions
  * @author Mikko Hilpinen
  * @since 5.4.2019
  */
object CollectionTest extends App
{
	val words = Vector("Apina", "Banaani", "Car", "David")
	
	val lengthUnder4 = words.findMap
	{
		w =>
			val len = w.length
			if (len <= 3) Some(len) else None
 	}
	
	assert(lengthUnder4.contains(3))
}
