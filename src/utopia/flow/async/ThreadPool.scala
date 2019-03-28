package utopia.flow.async

import java.util.concurrent.Executor
import java.time.Duration
import utopia.flow.util.Counter
import utopia.flow.util.WaitUtils
import scala.concurrent.Promise
import scala.concurrent.Await

/**
* This class handles thread reuse and distribution
* @author Mikko Hilpinen
* @since 28.3.2019
**/
class ThreadPool(val name: String, coreSize: Int, val maxSize: Int, val maxIdleDuration: Duration, 
        val errorHandler: Exception => Unit) extends Executor
{
    // ATTRIBUTES    ---------------------
    
    private val indexCounter = new Counter(1)
    
    
    // IMPLEMENTED    --------------------
    
	def execute(action: Runnable) = ???
}

private class WorkerThread(name: String, val maxIdleDuration: Option[Duration], 
        val getWaitingTask: () => Option[Runnable]) extends Thread
{
    // ATTRIBUTES    ---------------------
    
    private val ended = new VolatileFlag()
    private val nextTask = new Volatile(Promise[Runnable]())
    
    
    // INITIAL CODE    -------------------
    
    setName(name)
    setDaemon(true)
    
    
    // IMPLEMENTED    --------------------
    
    override def run() = 
    {
        var waitingTask: Option[Runnable] = None
        
        while (!ended.isSet)
        {
            // Finds the next task to perform, may fail if maximum idle duration is reached
            val next = waitingTask orElse 
            {
                // Await.ready(nextTask.get.future, maxIdleDuration getOrElse(scala.concurrent.duration.Duration.Inf))
                ???
            }
            
            /*
            // If no task was received and none is available, ends this thread
            if (next.isEmpty)
            {
                endingLock.synchronized
                {
                    if (nextTask.isEmpty)
                        ended.set()
                }
            }
            else
            {
                // Otherwise performs the task
                next.get.run()
                
                // Looks for the next task
                nextTask.setIfEmpty(getNext)
            }*/
        }
    }
    
    
    // OTHER    -------------------------
    
    
    
    // Waits until a) new task is received or b) max idle duration is reached, whichever comes first
    // Returns the next task, which is now removed from the next task slot
    /*
    private def waitForNextTask(): Option[Runnable] = nextTask.get.orElse
    {
        if (maxIdleDuration.isDefined)
        {
            WaitUtils.wait(maxIdleDuration.get, this)
            nextTask.pop()
        }
        else
        {
            WaitUtils.waitUntilNotified(this)
            nextTask.pop() orElse waitForNextTask()
        }
    }*/
}