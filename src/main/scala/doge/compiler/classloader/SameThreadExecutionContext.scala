package doge.compiler.classloader

import scala.concurrent.ExecutionContext

/**
 * An execution context which just runs futures immediately on the same thread.
 */
object SameThreadExecutionContext extends ExecutionContext {
  override def execute(runnable: Runnable): Unit = runnable.run()
  override def reportFailure(cause: Throwable): Unit = throw cause
}
