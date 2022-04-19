Pokemon <- R6Class(
  classname = "Pokemon",
  list(
    #The species of the Pokemon is kept track of so as to allow for the       evolution mechanic. You can still nickname your Pokemon through the           variable names that the "Pokemon" objects are stored in
    species = NULL,
    #Pokemon types allow for damage to increase or decrease based off of the  relationship between the attack's type and the opposing Pokemon's type. Think of the famous "Super-Effective!" and "Not very effective..." markers from the Pokemon games. Every Pokemon has a primary type (type1), but not every Pokemon has a secondary type (type2.
    type1 = NULL,
    type2 = NA,
    #HP serves as a health bar, which is lowered when the Pokemon is on the receving end of an attack. Upon HP reaching zero, the Pokemon is declared fainted.
    hp = NULL,
    #The receiving Pokemon's defense is used to calculate the damage it receives. the higher defense a Pokemon has, the less damage it receives.
    defense = NULL,
    #the higher offense a Pokemon has, the more damage its attacks deal to the receiving Pokemon.

    #If an attack has a base power of 60, and the dealing Pokemon has an offense of 60, but the receiving Pokemon has a high defense of 100, then the receiving Pokemon only lose 20 HP. if the attack is super-effective against the receiving Pokemon's type, it instead loses 40 HP.
    offense = NULL,

    #It is suggested that all Pokemon start at level 1, and that the level be raised using the later-defined levelup() function. For many species of Pokemon, when their level reaches a certain number, they're granted the ability to evolve. Evolution and leveling up both serve to power-up the Pokemon.
    level = 1,
    #Turn serves as a boolean switch to prevent players from acting twice consecutively. It is flipped off after the Pokemon deals an attack, and flipped back on when the Pokemon receives an attack. This switch is defined inside of the attack() function, rather than inside of this class
    turn = TRUE,
    #Happiness serves as an alternate evolution requirement. This is exclusive to a few Pokemon, as it is a rarer evolution method in the Pokemon franchise. To prevent crowded output, this stat isn't displayed when calling the object.
    happiness = 0,

    #Evolution stage is part of ruleset pocketMonstR uses to restrict certain Pokemon from using certain moves. More powerful moves are locked to Pokemon in their 2nd or 3rd evolution stage. Note that certain Pokemon can go from evoluton stage 1 to evolution stage 3 after one evolution, as some two-stage Pokemon families share a similar strength level to that of three-stage families.

    #To explain in simple terms, a Charmander can use ember, the weakest common Fire type move, but gains access to Flamethrower when evolving into Charmeleon. Finally, when fully evolved into the powerful Charizard species, the same Pokemon object gains access to the mighty Fire Blast attack.
    evo.stage = 1,

    #The initial species for this class is set to Squirtle, as it is one of the starting trio in the original Pokemon games.
    initialize = function(species = "Squirtle", type1 = "Water", type2 = NA,
                          hp = 100, defense = 0, offense = 0, level = 1, turn = TRUE, happiness = 0, evo.stage = 1) {
      self$species <- species
      self$type1 <- type1
      self$type2 <- type2
      self$hp <- hp
      self$defense <- defense
      self$offense <- offense
      self$level <- level
      self$turn <- turn
      self$evo.stage <- evo.stage
    },

    #Displaying the Pokemon's attributes
    print = function(...) {
      cat("Pokemon: \n")
      cat("  Species: ", self$species, "\n", sep = "")
      cat("  Primary Type:  ", self$type1, "\n", sep = "")
      cat("  Secondary Type:  ", self$type2, "\n", sep = "")
      cat("  Level: ", self$level, "\n", sep = "")
      cat("  Evolution Stage: ", self$evo.stage, "\n", sep = "")
      cat("  HP: ", self$hp, "\n", sep = "")
      cat("  Defense:  ", self$defense, "\n", sep = "")
      cat("  Offense:  ", self$offense, "\n", sep = "")
      invisible(self)
    },

    #A "level up" function that raises the Pokemon's level as well as boosts its stats. This works by generating a random number for each individual level that's being added to the Pokemon, rounding it to the nearest whole number (making 2 the most common stat increase) and summing all of the generated numbers into one final stat increase. IN the original Pokemon games, stat growth is distributed differently for each species. This was impractically for the pocketMonstR package, so stat boosts Pokemon gain when evolving are more exaggerated to match the nature of the Pokemon species they're evolving into.
    levelup  = function(x = 1) {
      #setting a level cap of 100, as in the original Pokemon games
      if (self$level+x > 100) {
        stop(
          "The Pokemon's level cannot exceed 100",
          call. = TRUE
        )}
      self$level <- self$level + x
      self$defense <- self$defense +
        sum(round(runif(x,1,3),0))
      self$offense <- self$offense +
        sum(round(runif(x,1,3),0))
    },
    #the damage() function removes health from the receiving Pokemon based off of the damage calculated in the attack() function.
    damage = function(x){
      self$hp <- self$hp - x
      #if (self$hp <= 0){
      # stop("The opposing Pokemon has fainted.", call. = TRUE)
      #}
    },

    #Certain attacks from the Pokemon franchsie possess the ability of restoring the dealing Pokemon's health with a fraction of the damage dealt. This is included in the package via absorb() , which is called based off of a boolean switch in the attack( function).
    absorb = function(x){
      self$hp <- self$hp + 0.5*x
    },
    #Here is a sample validator for the class.
    validate_factor = function(x) {
      values <- unclass(x)
      levels <- attr(x, "levels")
      if (!all(!is.na(values))) {
        stop(
          "All `x` values must be non-missing!",
          call. = FALSE
        )}}
  ))
