#' evolve
#'
#' @param pokemon1
#'
#' @return
#' @export
#'
#' @examples
evolve <- function(pokemon1){
  if
  (pokemon1$species == "Bulbasaur" & pokemon1$level >= 16){
    pokemon1$species <- "Ivysaur"
    pokemon1$offense <- pokemon1$offense + 18
    pokemon1$defense <- pokemon1$defense + 17
    pokemon1$evo.stage <- 2
  } else if
  (pokemon1$species == "Ivysaur" & pokemon1$level >= 32){
    pokemon1$species <- "Venusaur"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if
  (pokemon1$species == "Charmander" & pokemon1$level >= 16){
    pokemon1$species <- "Charmeleon"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 15
    pokemon1$evo.stage <- 2
  } else if
  (pokemon1$species == "Charmeleon" & pokemon1$level >= 36){
    pokemon1$species <- "Charizard"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$type2 <- "Flying"
    pokemon1$evo.stage <- 3
  } else if
  (pokemon1$species == "Squirtle" & pokemon1$level >= 16){
    pokemon1$species <- "Wartortle"
    pokemon1$offense <- pokemon1$offense + 15
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 2
  } else if
  (pokemon1$species == "Wartortle" & pokemon1$level >= 16){
    pokemon1$species <- "Blastoise"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 35
    pokemon1$evo.stage <- 3
  } else if
  (pokemon1$species == "Eevee" & pokemon1$happiness >= 100){
    split.evo <- round(runif(1, 0.5, 3.49), 0)
    if (split.evo == 1){
      pokemon1$species <- "Sylveon"
      pokemon1$offense <- pokemon1$offense + 30
      pokemon1$defense <- pokemon1$defense + 25
      pokemon1$type1 <- "Fairy"
      pokemon1$evo.stage <- 2
    } else if (split.evo == 2){
      pokemon1$species <- "Espeon"
      pokemon1$offense <- pokemon1$offense + 35
      pokemon1$defense <- pokemon1$defense + 20
      pokemon1$type1 <- "Psychic"
      pokemon1$evo.stage <- 2
    } else if (split.evo == 3){
      pokemon1$species <- "Umbreon"
      pokemon1$offense <- pokemon1$offense + 20
      pokemon1$defense <- pokemon1$defense + 35
      pokemon1$type1 <- "Dark"
      pokemon1$evo.stage <- 2
    }
  } else if (pokemon1$species == "Pichu" & pokemon1$happiness >= 100){
    pokemon1$species <- "Pikachu"
    pokemon1$offense <- pokemon1$offense + 26
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Pikachu" & pokemon1$happiness >= 200){
    pokemon1$species <- "Raichu"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 24
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Ralts" & pokemon1$level >= 20){
    pokemon1$species <- "Kirlia"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 10
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Kirlia" & pokemon1$level >= 30){
    pokemon1$species <- "Gardevoir"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Gible" & pokemon1$level >= 24){
    pokemon1$species <- "Gabite"
    pokemon1$offense <- pokemon1$offense + 30
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Gabite" & pokemon1$level >= 48){
    pokemon1$species <- "Garchomp"
    pokemon1$offense <- pokemon1$offense + 45
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Ponyta" & pokemon1$level >= 42){
    pokemon1$species <- "Rapidash"
    pokemon1$offense <- pokemon1$offense + 55
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Munchlax" & pokemon1$happiness >= 200){
    pokemon1$species <- "Snorlax"
    pokemon1$offense <- pokemon1$offense + 30
    pokemon1$defense <- pokemon1$defense + 80
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Riolu" & pokemon1$happiness >= 200){
    pokemon1$species <- "Lucario"
    pokemon1$offense <- pokemon1$offense + 80
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$type2 <- "Steel"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Magikarp" & pokemon1$level >= 20){
    pokemon1$species <- "Gyarados"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Sandile" & pokemon1$level >= 29){
    pokemon1$species <- "Krokorok"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 15
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Krokorok" & pokemon1$level >= 40){
    pokemon1$species <- "Krookodile"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Aron" & pokemon1$level >= 32){
    pokemon1$species <- "Lairon"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Lairon" & pokemon1$level >= 42){
    pokemon1$species <- "Aggron"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Starly" & pokemon1$level >= 14){
    pokemon1$species <- "Staravia"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Staravia" & pokemon1$level >= 34){
    pokemon1$species <- "Staraptor"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Yamask" & pokemon1$level >= 34){
    pokemon1$species <- "Cofagrigus"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 50
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Venipede" & pokemon1$level >= 22){
    pokemon1$species <- "Whirlipede"
    pokemon1$offense <- pokemon1$offense + 15
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Whirlipede" & pokemon1$level >= 30){
    pokemon1$species <- "Scolipede"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Cubchoo" & pokemon1$level >= 37){
    pokemon1$species <- "Beartic"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Meowth" & pokemon1$level >= 20){
    pokemon1$species <- "Persian"
    pokemon1$offense <- pokemon1$offense + 45
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Ekans" & pokemon1$level >= 22){
    pokemon1$species <- "Arbok"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 35
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Dratini" & pokemon1$level >= 30){
    pokemon1$species <- "Dragonair"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Dragonair" & pokemon1$level >= 55){
    pokemon1$species <- "Dragonite"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$type2 <- "Flying"
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Dreepy" & pokemon1$level >= 50){
    pokemon1$species <- "Drakloak"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Drakloak" & pokemon1$level >= 60){
    pokemon1$species <- "Dragapult"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 35
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Mudkip" & pokemon1$level >= 16){
    pokemon1$species <- "Marshtomp"
    pokemon1$offense <- pokemon1$offense + 27
    pokemon1$defense <- pokemon1$defense + 28
    pokemon1$evo.stage <- 2
    pokemon1$type2 <- "Ground"
  } else if (pokemon1$species == "Marshtomp" & pokemon1$level >= 36){
    pokemon1$species <- "Swampert"
    pokemon1$offense <- pokemon1$offense + 38
    pokemon1$defense <- pokemon1$defense + 27
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Torchic" & pokemon1$level >= 16){
    pokemon1$species <- "Combusken"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$type2 <- "Fighting"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Combusken" & pokemon1$level >= 36){
    pokemon1$species <- "Blaziken"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Numel" & pokemon1$level >= 33){
    pokemon1$species <- "Camerupt"
    pokemon1$offense <- pokemon1$offense + 55
    pokemon1$defense <- pokemon1$defense + 50
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Impidimp" & pokemon1$level >= 32){
    pokemon1$species <- "Morgrem"
    pokemon1$offense <- pokemon1$offense + 30
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Morgrem" & pokemon1$level >= 42){
    pokemon1$species <- "Grimmsnarl"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Grubbin" & pokemon1$level >= 20){
    pokemon1$species <- "Charjabug"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$type2 <- "Electric"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Charjabug" & pokemon1$level >= 32){
    pokemon1$species <- "Vikavolt"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Ferroseed" & pokemon1$level >= 40){
    pokemon1$species <- "Ferrothorn"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 80
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Rolycoly" & pokemon1$level >= 18){
    pokemon1$species <- "Carkol"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$type2 <- "Fire"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Carkol" & pokemon1$level >= 34){
    pokemon1$species <- "Coalossoal"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Rookidee" & pokemon1$level >= 18){
    pokemon1$species <- "Corvisquire"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Corvisquire" & pokemon1$level >= 38){
    pokemon1$species <- "Corviknight"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 50
    pokemon1$type2 <- "Steel"
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Larvitar" & pokemon1$level >= 30){
    pokemon1$species <- "Pupitar"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Pupitar" & pokemon1$level >= 55){
    pokemon1$species <- "Tyranitar"
    pokemon1$offense <- pokemon1$offense + 45
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$type2 <- "Dark"
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Croagunk" & pokemon1$level >= 37){
    pokemon1$species <- "Toxicroak"
    pokemon1$offense <- pokemon1$offense + 60
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Pancham" & pokemon1$level >= 32){
    pokemon1$species <- "Pangoro"
    pokemon1$offense <- pokemon1$offense + 50
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 2
    pokemon1$type2 <- "Dark"
    #fake pokemon
  } else if (pokemon1$species == "Brainon" & pokemon1$level >= 34){
    pokemon1$species <- "Psydon"
    pokemon1$offense <- pokemon1$offense + 30
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$type <- "Dragon"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Psydon" & pokemon1$level >= 52){
    pokemon1$species <- "Psyclops"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Soupin" & pokemon1$level >= 30){
    pokemon1$species <- "Souptaki"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$type2 <- "Fire"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Dolphlegm" & pokemon1$level >= 25){
    pokemon1$species <- "Toxorca"
    pokemon1$offense <- pokemon1$offense + 25
    pokemon1$defense <- pokemon1$defense + 35
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Toxorca" & pokemon1$level >= 42){
    pokemon1$species <- "Whalution"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 40
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Aurrorror" & pokemon1$level >= 22){
    pokemon1$species <- "Crystellation"
    pokemon1$offense <- pokemon1$offense + 35
    pokemon1$defense <- pokemon1$defense + 25
    pokemon1$type2 <- "Ice"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Crystellation" & pokemon1$level >= 44){
    pokemon1$species <- "Blizziverse"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Magiprent" & pokemon1$level >= 20){
    pokemon1$species <- "Equestrasus"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$type2 <- "Flying"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Equestrasus" & pokemon1$level >= 55){
    pokemon1$species <- "Uniwarn"
    pokemon1$offense <- pokemon1$offense + 30
    pokemon1$defense <- pokemon1$defense + 50
    pokemon1$type2 <- "Steel"
    pokemon1$evo.stage <- 3
  } else if (pokemon1$species == "Lampish" & pokemon1$level >= 16){
    pokemon1$species <- "Ophswich"
    pokemon1$offense <- pokemon1$offense + 20
    pokemon1$defense <- pokemon1$defense + 20
    pokemon1$type2 <- "Dark"
    pokemon1$evo.stage <- 2
  } else if (pokemon1$species == "Ophswich" & pokemon1$level >= 40){
    pokemon1$species <- "Shinister"
    pokemon1$offense <- pokemon1$offense + 40
    pokemon1$defense <- pokemon1$defense + 30
    pokemon1$evo.stage <- 3
  } else {
    stop(paste0("This Pokemon cannot currently evolve"), call. = FALSE)
  }
}
