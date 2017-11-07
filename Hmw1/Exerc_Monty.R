switch_win <- 0
no_switch_win <- 0
simulation <- 50000

for(i in 1:simulation){
  
  door_chosen <- 1
  door_opened <- 3
  #The prize is under the door 1 or 2
  door_prize <- sample(c(1,2),1)
  
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

