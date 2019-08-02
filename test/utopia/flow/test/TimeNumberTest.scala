package utopia.flow.test

import java.time.{Month, Year}
import java.util.concurrent.TimeUnit

import utopia.flow.util.TimeExtensions._

import scala.concurrent.duration.FiniteDuration

/**
  * This test checks time number eguality
  * @author Mikko Hilpinen
  * @since 5.6.2019, v1+
  */
object TimeNumberTest extends App
{
	assert(13.nanos == FiniteDuration(13, TimeUnit.NANOSECONDS))
	assert(13.millis == FiniteDuration(13, TimeUnit.MILLISECONDS))
	assert(13.seconds == FiniteDuration(13, TimeUnit.SECONDS))
	assert(13.512.seconds == FiniteDuration(13512, TimeUnit.MILLISECONDS))
	assert(13.minutes == FiniteDuration(13, TimeUnit.MINUTES))
	assert(1.5.minutes == FiniteDuration(90, TimeUnit.SECONDS))
	assert(13.hours == FiniteDuration(13, TimeUnit.HOURS))
	
	println(13.5231.nanos.description)
	println(13.5231.millis.description)
	println(324.5231.millis.description)
	println(13.5231.seconds.description)
	println(13.5231.minutes.description)
	println(13.5231.hours.description)
	
	// Also tests some other time extensions
	val weeksAtAug2019 = (Year.of(2019) + Month.AUGUST).weeks()
	assert(weeksAtAug2019.head.size == 4)
	assert(weeksAtAug2019.last.size == 6)
	// println(LocalDate.now.yearMonth.weeks().map { _.map { d => s"${d.getDayOfMonth} ${d.weekDay}" }.mkString(", ") }.mkString("\n"))
	
	println("Success!")
}
