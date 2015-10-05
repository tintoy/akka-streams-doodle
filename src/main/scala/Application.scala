import akka.actor.ActorSystem

/**
 * The main application entry-point.
 */
object Application extends App {
	println("Starting...")
	val actorSystem = ActorSystem.create("SimpleSystem")
	println("Started.")
	
	println("Stopping...")
	actorSystem.shutdown()
	println("Stopped.")
}
