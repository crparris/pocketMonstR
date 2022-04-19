#' status
#'
#' @param pokemon1
#' @param move
#' @param pokemon2
#'
#' @return
#' @export
#'
#' @examples
status <- function(pokemon1, move, pokemon2){
  #Checking the turn-based constraint
  if (pokemon1$turn == FALSE){
    stop("It is not this Pokemon's turn",
         call. = TRUE)
  }
  #Recover allows the Pokemon to recover some of it's own health. This is useful for Pokemon who don't have access to HP- absorbing attacks.
  if (move == "Recover"){
    pokemon1$hp <- pokemon1$hp + 0.333*pokemon1$hp

    #Hone Claws allows the Pokemon to raise it's offense stat, sacrificing a turn in order to raise its baseline damage output.
  } else if (move == "Hone.Claws"){
    pokemon1$offense <- pokemon1$offense + 0.35*pokemon1$offense

    #Defense Curl allows the user to raise its defense stat, sacrificing a turn to ensure that later it receives less damage.
  } else if (move =="Defense.Curl"){
    pokemon1$defense <- pokemon1$defense + 0.35*pokemon1$defense

    #Belly Drum functions as a riskier version of Hone Claws by raising the user's attack by a huge amount at the user losing half of its HP.
  } else if (move == "Belly.Drum"){
    pokemon1$offense <- 2*pokemon1$offense
    pokemon1$hp <- 0.5*pokemon1$hp

    #Bulk Up serves as a middle-ground between Hone CLaws and Defense Curl, raising both the offense and defense stats by a smaller percentage.
  } else if (move == "Bulk.Up"){
    pokemon1$offense <- pokemon1$offense + 0.20*pokemon1$offense
    pokemon1$defense <- pokemon1$defense + 0.20*pokemon1$defense
  } else {
    stop("This is not a status move.", call. = TRUE)
  }

  #flipping the turn-based boolean switch.
  pokemon1$turn <- FALSE
  pokemon2$turn <- TRUE
}
