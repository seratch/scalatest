import org.scalatest._

class ExampleSpec extends WordSpec with ParallelTestExecution {

  "Thing 1" should {
    "do thing 1a" in {
      println("Starting 1a")
      Thread.sleep(1000)
      println("Ending 1a")
    }
    "do thing 1b" in {
      println("Starting 1b")
      Thread.sleep(900)
      println("Ending 1b")
    }
    "do thing 1c" in {
      println("Starting 1c")
      Thread.sleep(800)
      println("Ending 1c")
    }
  }
  "Thing 2" should {
    "do thing 2a" in {
      println("Starting 2a")
      Thread.sleep(700)
      println("Ending 2a")
    }
    "do thing 2b" in {
      println("Starting 2b")
      Thread.sleep(600)
      println("Ending 2b")
    }
    "do thing 2c" in {
      println("Starting 2c")
      Thread.sleep(500)
      println("Ending 2c")
    }
  }
}
