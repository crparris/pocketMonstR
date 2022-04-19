#Dealing damage to a Pokemon and lowering its health
#' attack
#'
#' @param pokemon1
#' @param move
#' @param pokemon2
#'
#' @return
#' @export
#'
#' @examples
attack <-function(pokemon1, move, pokemon2){
  #Checking the turn-based constraint
  if (pokemon1$turn == FALSE){
    stop("It is not this Pokemon's turn",
         call. = TRUE)
  }

  compat = NULL

  #Checking type compatibility with the user's Pokemon.
  if (move["type"] == pokemon1$type1){
    compat = TRUE
  } else if (move["type"] == pokemon1$type2) {
    compat = TRUE
  } else if (move["type"] == "Normal") {
    compat = TRUE
  } else {
    compat = FALSE
  }


  if (compat == FALSE){
    stop(paste0(pokemon1$species, "cannot use attacks of this type"), call. = TRUE)
  }

  #Checking if the Pokemon is powerful enough to use the attack
  if (move["evo.stage"] > pokemon1$evo.stage){
    stop(paste0(pokemon1$species, " is not powerful enough to use this attack"), call. = TRUE)
  }

  #super-effective!!
  if (move["type"] == "Fire" & !is.na(pokemon2$type1 == "Grass" | pokemon2$type2 == "Grass" | pokemon2$type1 == "Bug"| pokemon2$type2 == "Bug" | pokemon2$type1 == "Ice" | pokemon2$type2 == "Ice"| pokemon2$type1 == "Steel" | pokemon2$type2 == "Steel")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    #deals damage to pokemon2
    pokemon2$damage(x)
  } else if
  #not very effective...
  (move["type"] == "Fire" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Water" | pokemon2$type2== "Water" | pokemon2$type1== "Rock" | pokemon2$type2== "Rock" | pokemon2$type1== "Dragon" | pokemon2$type2== "Dragon")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Water" & !is.na(pokemon2$type1 == "Ground" | pokemon2$type2 == "Ground" | pokemon2$type1 == "Rock"| pokemon2$type2 == "Rock" | pokemon2$type1 == "Fire" | pokemon2$type2 == "Fire")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Water" & !is.na(pokemon2$type1== "Water" | pokemon2$type2== "Water" | pokemon2$type1== "Grass" | pokemon2$type2== "Grass" | pokemon2$type1== "Dragon" | pokemon2$type2== "Dragon")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Grass" & !is.na(pokemon2$type1 == "Ground" | pokemon2$type2 == "Ground" | pokemon2$type1 == "Rock"| pokemon2$type2 == "Rock" | pokemon2$type1 == "Water" | pokemon2$type2 == "Water")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
    #granting moves permission to absorb the opposing Pokemon's health
    if (move["absorb"] == TRUE){
      pokemon1$absorb(x)
    }
  } else if (move["type"] == "Grass" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Grass" | pokemon2$type2== "Grass" | pokemon2$type1== "Poison" | pokemon2$type2== "Poison" | pokemon2$type1== "Dragon" | pokemon2$type2== "Dragon"| pokemon2$type1== "Flying" | pokemon2$type2== "Flying"| pokemon2$type1== "Steel" | pokemon2$type2== "Steel" | pokemon2$type1== "Bug" | pokemon2$type2== "Bug")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
    if (move["absorb"] == TRUE){
      pokemon1$absorb(x)
    }
  } else if (move["type"] == "Normal" & !is.na(pokemon2$type1 == "Ghost" | pokemon2$type2 == "Ghost")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Normal" & !is.na(pokemon2$type1== "Rock" | pokemon2$type2== "Rock" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Electric" & !is.na(pokemon2$type1 == "Water" | pokemon2$type2 == "Water" | pokemon2$type1 == "Flying"| pokemon2$type2 == "Flying")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Electric" & !is.na(pokemon2$type1== "Electric" | pokemon2$type2== "Electric" | pokemon2$type1== "Grass" | pokemon2$type2== "Grass" | pokemon2$type1== "Dragon" | pokemon2$type2== "Dragon")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Electric" & !is.na(pokemon2$type1 == "Ground" | pokemon2$type2 == "Ground")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ice" & !is.na(pokemon2$type1 == "Ground" | pokemon2$type2 == "Ground" | pokemon2$type1 == "Grass"| pokemon2$type2 == "Grass" | pokemon2$type1 == "Flying" | pokemon2$type2 == "Flying" | pokemon2$type1 == "Dragon" | pokemon2$type2 == "Dragon")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ice" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Water" | pokemon2$type2== "Water" | pokemon2$type1== "Ice" | pokemon2$type2== "Ice" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Fighting" & !is.na(pokemon2$type1 == "Rock" | pokemon2$type2 == "Rock" | pokemon2$type1 == "Dark"| pokemon2$type2 == "Dark" | pokemon2$type1 == "Steel" | pokemon2$type2 == "Steel" | pokemon2$type1 == "Ice" | pokemon2$type2 == "Ice" | pokemon2$type1 == "Normal" | pokemon2$type2 == "Normal")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Fighting" & !is.na(pokemon2$type1== "Poison" | pokemon2$type2== "Poison" | pokemon2$type1== "Flying" | pokemon2$type2== "Flying" | pokemon2$type1== "Psychic" | pokemon2$type2== "Psychic" | pokemon2$type1== "Bug" | pokemon2$type2== "Bug" | pokemon2$type1 == "Fairy" | pokemon2$type2 == "Fairy")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Fighting" & !is.na(pokemon2$type1 == "Ghost" | pokemon2$type2 == "Ghost")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Poison" & !is.na(pokemon2$type1 == "Fairy" | pokemon2$type2 == "Fairy" | pokemon2$type1 == "Grass"| pokemon2$type2 == "Grass")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Poison" & !is.na(pokemon2$type1== "Poison" | pokemon2$type2== "Poison" | pokemon2$type1== "Ground" | pokemon2$type2== "Ground" | pokemon2$type1== "Rock" | pokemon2$type2== "Rock" | pokemon2$type1== "Ghost" | pokemon2$type2== "Ghost")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Poison" & !is.na(pokemon2$type1 == "Steel" | pokemon2$type2 == "Steel")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ground" & !is.na(pokemon2$type1 == "Fire" | pokemon2$type2 == "Fire" | pokemon2$type1 == "Rock"| pokemon2$type2 == "Rock" | pokemon2$type1 == "Steel" | pokemon2$type2 == "Steel" | pokemon2$type1 == "Poison" | pokemon2$type2 == "Poison" | pokemon2$type1 == "Electric" | pokemon2$type2 == "Electric")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ground" & !is.na(pokemon2$type1== "Grass" | pokemon2$type2== "Grass" | pokemon2$type1== "Bug" | pokemon2$type2== "Bug")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ground" & !is.na(pokemon2$type1 == "Flying" | pokemon2$type2 == "Flying")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Flying" & !is.na(pokemon2$type1 == "Fighting" | pokemon2$type2 == "Fighting" | pokemon2$type1 == "Grass"| pokemon2$type2 == "Grass" | pokemon2$type1 == "Bug" | pokemon2$type2 == "Bug")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Flying" & !is.na(pokemon2$type1== "Electric" | pokemon2$type2== "Electric" | pokemon2$type1== "Rock" | pokemon2$type2== "Rock" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Psychic" & !is.na(pokemon2$type1 == "Fighting" | pokemon2$type2 == "Fighting" | pokemon2$type1 == "Poison"| pokemon2$type2 == "Poison")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Psychic" & !is.na(pokemon2$type1== "Steel" | pokemon2$type2== "Steel" | pokemon2$type1== "Psychic" | pokemon2$type2== "Psychic")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Psychic" & !is.na(pokemon2$type1 == "Dark" | pokemon2$type2 == "Dark")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Bug" & !is.na(pokemon2$type1 == "Dark" | pokemon2$type2 == "Dark" | pokemon2$type1 == "Grass"| pokemon2$type2 == "Grass" | pokemon2$type1 == "Psychic" | pokemon2$type2 == "Psychic")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Bug" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Fighting" | pokemon2$type2== "Fighting" | pokemon2$type1== "Poison" | pokemon2$type2== "Poison" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel" | pokemon2$type1 == "Flying" | pokemon2$type2 == "Flying" | pokemon2$type1 == "Fairy" | pokemon2$type2 == "Fairy" | pokemon2$type1 == "Ghost" | pokemon2$type2 == "Ghost")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Rock" & !is.na(pokemon2$type1 == "Fire" | pokemon2$type2 == "Fire" | pokemon2$type1 == "Ice"| pokemon2$type2 == "Ice" | pokemon2$type1 == "Flying" | pokemon2$type2 == "Flying" | pokemon2$type1 == "Bug" | pokemon2$type2 == "Bug")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Rock" & !is.na(pokemon2$type1== "Fighting" | pokemon2$type2== "Fighting" | pokemon2$type1== "Ground" | pokemon2$type2== "Ground" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Ghost" & !is.na(pokemon2$type1 == "Ghost" | pokemon2$type2 == "Ghost" | pokemon2$type1 == "Psychic"| pokemon2$type2 == "Pyschic")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ghost" & !is.na(pokemon2$type1== "Dark" | pokemon2$type2== "Dark")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Ghost" & !is.na(pokemon2$type1 == "Normal" | pokemon2$type2 == "Normal")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Dragon" & !is.na(pokemon2$type1 == "Dragon" | pokemon2$type2 == "Dragon")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Dragon" & !is.na(pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Dragon" & !is.na(pokemon2$type1 == "Fairy" | pokemon2$type2 == "Fairy")){
    x <- 0*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Dark" & !is.na(pokemon2$type1 == "Ghost" | pokemon2$type2 == "Ghost" | pokemon2$type1 == "Psychic"| pokemon2$type2 == "Psychic")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Dark" & !is.na(pokemon2$type1== "Dark" | pokemon2$type2== "Dark" | pokemon2$type1== "Fighting" | pokemon2$type2== "Fighting" | pokemon2$type1== "Fairy" | pokemon2$type2== "Fairy")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Steel" & !is.na(pokemon2$type1 == "Rock" | pokemon2$type2 == "Rock" | pokemon2$type1 == "Ice"| pokemon2$type2 == "Ice" | pokemon2$type1 == "Fairy" | pokemon2$type2 == "Fairy")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Steel" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Water" | pokemon2$type2== "Water" | pokemon2$type1== "Electric" | pokemon2$type2== "Electric" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)

  } else if (move["type"] == "Fairy" & !is.na(pokemon2$type1 == "Fighting" | pokemon2$type2 == "Fighting" | pokemon2$type1 == "Dark"| pokemon2$type2 == "Dark" | pokemon2$type1 == "Dragon" | pokemon2$type2 == "Dragon")){
    x <- 2*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else if (move["type"] == "Fairy" & !is.na(pokemon2$type1== "Fire" | pokemon2$type2== "Fire" | pokemon2$type1== "Poison" | pokemon2$type2== "Poison" | pokemon2$type1== "Steel" | pokemon2$type2== "Steel")){
    x <- 0.5*(as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  } else {
    #no type advantages or disadvantages
    x <- (as.numeric(move["power"])+pokemon1$offense-pokemon2$defense)
    pokemon2$damage(x)
  }
  #If the Pokemon's HP reaches zero, it faints, declaring the opposing Pokemon victorious.
  if (pokemon2$hp <= 0){
    pokemon1$happiness <- pokemon1$happiness + 50
    stop("The opposing Pokemon has fainted.", call. = TRUE)
  }
  pokemon1$turn <- FALSE
  pokemon2$turn <- TRUE
}
