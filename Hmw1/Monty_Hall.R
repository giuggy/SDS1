switch_win <- 0
no_switch_win <- 0
simulation <- 50
doors <- c(1,2,3)

for(i in 1:simulation){
  
  door_prize <- sample(doors,1) 
  door_chosen <- sample(doors,1)
  
  #Monty opens the door with a goat
  options = doors[which(doors != door_prize & doors != door_chosen)]
  if(length(options) == 1){
    door_opened = options
  }else{
    door_opened <- sample(options, 1)
  }

  doors_available <- doors[-c(door_opened)]

  #The contestant doesn't change his decision
  if(door_prize == door_chosen){
    #number of times that if the contestant doesn't change, and his door hides the prize
    no_switch_win <- no_switch_win + 1 
  } 
  #The contestant changes his door
  new_choice <- doors_available[doors_available != door_chosen][1]
  if(new_choice == door_prize){
    #number of times that the contestant win after the changement of the door
    switch_win = switch_win + 1
  }
  
}

cat("The average that the contestant doesn't change and he wins ", no_switch_win/simulation, fill = TRUE)
cat("The average that the contestant changes and he wins ", switch_win/simulation, fill = TRUE)

