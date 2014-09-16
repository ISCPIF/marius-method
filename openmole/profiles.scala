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
import org.openmole.plugin.hook.modifier._


// Define the variables
val economicMultiplier = Prototype[Double]("economicMultiplier")
val populationToWealthExponent = Prototype[Double]("populationToWealthExponent")
val sizeEffectOnSupply = Prototype[Double]("sizeEffectOnSupply")
val sizeEffectOnDemand = Prototype[Double]("sizeEffectOnDemand")
val wealthToPopulationExponent = Prototype[Double]("wealthToPopulationExponent")
val distanceDecay = Prototype[Double]("distanceDecay")
val bonusMultiplier = Prototype[Double]("bonusMultiplier")
val fixedCost = Prototype[Double]("fixedCost")

val distribution = Prototype[Double]("distribution")
val dead = Prototype[Double]("dead")
val overflow = Prototype[Double]("overflow")

  
// Define the task which runs the model
val modelTask = 
  GroovyTask(
    "modelTask", 
    "model = new BonusFixedCostModel(economicMultiplier, sizeEffectOnSupply, sizeEffectOnDemand, distanceDecay, wealthToPopulationExponent, populationToWealthExponent, bonusMultiplier, fixedCost)\n" +
    "error = Target2.error(model)\n" +
    "dead = error[0] * 1000\n" +
    "distribution = error[1]\n" +
    "overflow = (error[2] == 0.0d?0.0d:(error[2] + 1) * 1000)\n")

modelTask.addImport("fr.geocites.marius.calibration.*")

modelTask addInput economicMultiplier
modelTask addInput sizeEffectOnSupply
modelTask addInput sizeEffectOnDemand
modelTask addInput wealthToPopulationExponent
modelTask addInput distanceDecay
modelTask addInput populationToWealthExponent
modelTask addInput bonusMultiplier
modelTask addInput fixedCost

modelTask addOutput distribution
modelTask addOutput dead
modelTask addOutput overflow

val model = Capsule(modelTask)


// Define an island
import org.openmole.plugin.method.evolution._
import ga._


val scales = 
  Seq(
    economicMultiplier -> (0.0, 1.0),
    sizeEffectOnSupply -> (1.0, 10.0),
    sizeEffectOnDemand -> (1.0, 10.0),
    wealthToPopulationExponent -> (0.0,10.0),
    distanceDecay -> (0.0, 10.0),
    populationToWealthExponent -> (1.0, 10.0),
    bonusMultiplier -> (0.0, 10000.0),
    fixedCost -> (0.0, 10.0)
  )

// Define the execution environment
//val env = GliteEnvironment("biomed", openMOLEMemory = 1400, wallTime = 4 hours)
//val env = DIRACGliteEnvironment("biomed", "https://ccdirac06.in2p3.fr:9178", cpuTime = 4 hours, openMOLEMemory = 1500)

val env = DIRACGliteEnvironment("vo.france-grilles.fr", "https://ccdirac06.in2p3.fr:9178", group = "frangrilles_user", cpuTime = 4 hours, openMOLEMemory = 1500)


def build(number: Int) = {

  val (prototype, _) = scales(number)

  val evolution = 
    GenomeProfile (
      x = number, 
      nX = 10000, 
      termination = Timed(2 hours),
      inputs = scales,
      objectives = Seq(dead, distribution, overflow)
    )

  // Define the distributed genetic algorithm
  val islandGA = islandSteadyGA(evolution, model)("island", 2500, Counter(200000), 5000)

  val hookCondition = s"${islandGA.generation.name} % 100 == 0"
  val saveProfile = SaveProfileHook(islandGA, "./profiles/" + prototype.name) condition hookCondition
  val savePopulation = SavePopulationHook(islandGA, "./populations/" + prototype.name) condition hookCondition

  // Define the hook to display the generation in the console
  val display = DisplayHook("Generation ${" + islandGA.generation.name + "} for " + prototype.name)

  // Define the execution
  (islandGA + 
   (islandGA.island on env) + 
   (islandGA.output hook savePopulation hook saveProfile hook display)) toExecution
}

val moles = Seq(0, 1, 2).map(build)


