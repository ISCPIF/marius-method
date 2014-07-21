logger.level("INFO")

// This workflow calibrate the marius model using a multi-objective
// genetic algorithm

// Import namespaces of openmole plugins
import org.openmole.plugin.domain.distribution._
import org.openmole.plugin.domain.modifier._
import org.openmole.plugin.task.groovy._
import org.openmole.plugin.hook.file._
import org.openmole.plugin.hook.display._
import org.openmole.plugin.grouping.batch._
import org.openmole.plugin.environment.glite._

// Path where to store the results
val path = "./simple/3g/"

// Define the variables
val economicMultiplier = Prototype[Double]("economicMultiplier")
val populationToWealthExponent = Prototype[Double]("populationToWealthExponent")
val sizeEffectOnSupply = Prototype[Double]("sizeEffectOnSupply")
val sizeEffectOnDemand = Prototype[Double]("sizeEffectOnDemand")
val wealthToPopulationExponent = Prototype[Double]("wealthToPopulationExponent")
val distanceDecay = Prototype[Double]("distanceDecay")

val distribution = Prototype[Double]("distribution")
val dead = Prototype[Double]("dead")
val overflow = Prototype[Double]("overflow")

 
// Define the task which runs the model
val modelTask = 
  GroovyTask(
    "modelTask", 
    "model = new SimpleModel(economicMultiplier, sizeEffectOnSupply, sizeEffectOnDemand, distanceDecay, wealthToPopulationExponent, populationToWealthExponent)\n" +
    "error = Target2.error(model)\n" +
    "dead = error[0]\n" +
    "distribution = error[1]\n" +
    "overflow = error[2]\n")

modelTask.addImport("fr.geocites.marius.calibration.*")

modelTask addInput economicMultiplier
modelTask addInput sizeEffectOnSupply
modelTask addInput sizeEffectOnDemand
modelTask addInput wealthToPopulationExponent
modelTask addInput distanceDecay
modelTask addInput populationToWealthExponent

modelTask addOutput distribution
modelTask addOutput dead
modelTask addOutput overflow

val model = Capsule(modelTask)


// Define an island
import org.openmole.plugin.method.evolution._
import ga._


val scales = 
  Seq(
    economicMultiplier -> (0.0, 100.0),
    sizeEffectOnSupply -> (1.0, 10.0),
    sizeEffectOnDemand -> (1.0, 10.0),
    wealthToPopulationExponent -> (0.0,10.0),
    distanceDecay -> (0.0, 10.0),
    populationToWealthExponent -> (1.0, 10.0)
  )

val evolution = 
  Optimisation (
    mu = 200,
    termination = Timed(2 hour),
    ranking = Pareto,
    inputs = scales,
    objectives = Seq(dead, distribution, overflow)
  )

val nsga2  = 
  steadyGA(evolution)(
    "calibrateModel",
    model
  )

// Define the island model
val islandModel = islandGA(nsga2)("island", 2500, Counter(200000), 100)

// Define the execution environment
//val env = GliteEnvironment("biomed", openMOLEMemory = 1400, wallTime = 4 hours)
val env = DIRACGliteEnvironment("biomed", "https://ccdirac06.in2p3.fr:9178", cpuTime = 4 hours)

val savePopulation = SavePopulationHook(islandModel, path)

// Define the hook to display the generation in the console
val display = DisplayHook("Generation ${" + islandModel.generation.name + "}")

// Define the execution
val ex = 
  (islandModel + 
   (islandModel.island on env) + 
   (islandModel.output hook savePopulation hook display)) toExecution

// Lauch the execution
ex.start

