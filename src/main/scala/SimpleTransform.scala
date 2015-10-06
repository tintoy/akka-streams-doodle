import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._


/**
 * The main application entry-point.
 * @note From what I've read, Akka Streams seems to solve similar problems to TPL Dataflow (although
 *       I'm interested to see how it interacts with Akka's remoting capabilities).
 */
object SimpleTransform extends App {
  // Adapted from Typesafe's Akka Streams sample.

  // AF: Don't have good intuitions for how this stuff works yet, so for now we'll just cargo-cult it.
  implicit val actorSystem = ActorSystem.create("SimpleSystem")
  import actorSystem.dispatcher
  implicit val materializer = ActorMaterializer()

  val text =
    """
      |Ok, Mr Burns, what's your first name?
      |I don't know.
    """.stripMargin

  println("Done!")
  println("Starting pipeline...")
  Source(() => text.split("\\s").iterator)
    .map(value => value.toUpperCase)
    .runForeach(
      value => println(value)
    )
    .onComplete(_ => actorSystem.shutdown())
  println("Pipeline started.")

  println("Waiting for actor system to shut down..")
  actorSystem.awaitTermination()
	println("Done.")
}

