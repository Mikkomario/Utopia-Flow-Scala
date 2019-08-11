package utopia.flow.test

import java.time.{Duration, Instant}

import utopia.flow.async.AsyncExtensions._
import utopia.flow.util.TimeExtensions._
import utopia.flow.async.{Loop, ThreadPool}
import utopia.flow.util.WaitTarget.Until
import utopia.flow.util.WaitUtils

import scala.concurrent.ExecutionContext

/**
  * Tests loops
  */
object LoopTest extends App
{
	// Makes sure WaitTarget / Until is working
	val started = Instant.now()
	val targetTime = started + Duration.ofSeconds(1)
	Until(targetTime).waitWith(this)
	val firstWaitDuration = Instant.now() - started
	
	assert(firstWaitDuration < Duration.ofMillis(1100))
	assert(firstWaitDuration > Duration.ofMillis(999))
	
	// Creates execution context
	implicit val context: ExecutionContext = new ThreadPool("Test").executionContext
	
	// Creates the loop
	var loopCount = 0
	val loop = Loop(Duration.ofMillis(100), () => loopCount += 1)
	
	assert(loopCount == 0)
	
	// Starts the loop, then waits
	loop.startAsync()
	WaitUtils.wait(Duration.ofSeconds(3), this)
	val loopCountAfterWait = loopCount
	
	println(loopCountAfterWait)
	assert(loopCountAfterWait > 25)
	assert(loopCountAfterWait < 35)
	
	val completion = loop.stop()
	val loopCountAfterStop = loopCount
	
	WaitUtils.wait(Duration.ofSeconds(1), this)
	
	println(loopCount - loopCountAfterStop)
	assert(loopCountAfterStop == loopCount)
	
	completion.waitFor()
	
	println("Success")
}
