triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

#Empty game board
display <- function(state){
  cat("\n", state[1], " |", state[2], "|", state[3], "\n",
      "---+---+---", "\n",
      state[4], " |", state[5], "|", state[6], "\n",
      "---+---+---", "\n",
      state[7], " |", state[8], "|", state[9],"\n")
}


update = function(state, who, pos){
  state[pos] = who
  return(state)
}


check_winner = function(state){
  win = F
  for (i in 1:8) {
    if( sum(state[triples[[i]]] == "x") == 3){
      win = T
    }
    if(sum(state[triples[[i]]] == "o") == 3){
      win = T
    }
  }
  return(win)
}


computer_turn = function(state){
  #Check computer play x or o
  if(sum(state[1:9] == "x") == sum(state[1:9] == "o")){
    com = "x"
    hum = "o"
  } else{
    com = "o"
    hum = "x"
  }
  
  p = ceiling(9* runif(1))
  for (j in 1:8) {
    if ( sum(state[triples[[j]]] == com) == 2 && sum(state[triples[[j]]] == hum) == 0){    #check if com ready to win
      for (k in 1:3) {
        if(state[triples[[j]][k]] != com){
          p = triples[[j]][k]
        }
      }
      break
      
    }else if ( sum(state[triples[[j]]] == hum) == 2 && sum(state[triples[[j]]] == com) == 0){    #check if need to block hum
      for (l in 1:3) {
        if(state[triples[[j]][l]] != hum){
          p = triples[[j]][l]
        }
      }
      
    }else {   #play in a random grid
      while(state[p] == "x" || state[p] =="o"){
        p = ceiling(9* runif(1))
      }
    }
  }
  
  state = update(state, com, p)
  cat(com, "plays position",p,"\n")
  return(state)
}



play = function(){
  #Initialize display
  in.state = 1:9
  in.state = as.character(in.state)
  nplayer = readline(prompt = "How many human players? 1 or 2: ")
  
  #play with computer
  if(nplayer == 1){
    
    f_or_s = readline(prompt = "Should the computer play first or second? 1 or 2: ")
    
    if(f_or_s == "1"){
      
      while (check_winner(in.state) == F) {
        #com
        in.state = computer_turn(in.state)
        check_winner(in.state)
        if(sum(in.state == "x") + sum(in.state == "o") == 9 && check_winner(in.state) == F){
          cat("Game ends in a draw.")
          display(in.state)
          break
        }
        if(check_winner(in.state) == T){
          cat("x wins")
          display(in.state)
          break
        }
        
        #hum
        display(in.state)
        pos = as.numeric(readline("Where should o play: "))
        
        while (in.state[pos] == "x" || in.state[pos] == "o"){
          pos = as.numeric(readline(prompt = "This position is occupied, please choose another one: "))
        }
        
        who = "o"
        in.state = update(in.state, who, pos)
        check_winner(in.state)
        if(check_winner(in.state) == T){
          cat("o wins")
          display(in.state)
          break
        }
      }
      
    } else {
      
      while (check_winner(in.state) == F) {
        
        #hum
        display(in.state)
        pos = as.numeric(readline("Where should x play: "))
        
        while (in.state[pos] == "x" || in.state[pos] == "o"){
          pos = as.numeric(readline(prompt = "This position is occupied, please choose another one: "))
        }
        
        who = "x"
        in.state = update(in.state, who, pos)
        check_winner(in.state)
        if(sum(in.state == "x") + sum(in.state == "o") == 9 && check_winner(in.state) == F){
          cat("Game ends in a draw.")
          display(in.state)
          break
        }
        if(check_winner(in.state) == T){
          cat("x wins")
          display(in.state)
          break
        }
        
        #com
        in.state = computer_turn(in.state)
        check_winner(in.state)
        if(check_winner(in.state) == T){
          cat("o wins")
          display(in.state)
          break
        }
        
      }
      
    }
    
  }
  
  #play with another human
  if(nplayer == 2){
    while (check_winner(in.state) == F) {
      
      #player x
      display(in.state)
      pos = as.numeric(readline("Where should x play: ")) 
      
      while (in.state[pos] == "x" || in.state[pos] == "o"){
        pos = as.numeric(readline(prompt = "This position is occupied, please choose another one: "))
      } 
      
      who = "x"
      in.state = update(in.state, who, pos)
      check_winner(in.state)
      if(sum(in.state == "x") + sum(in.state == "o") == 9 && check_winner(in.state) == F){
        cat("Game ends in a draw.")
        display(in.state)
        break
      }
      if(check_winner(in.state) == T){
        cat("x wins")
        display(in.state)
        break
      }

      
      #player o
      display(in.state)
      pos = as.numeric(readline("Where should o play: "))
      
      while (in.state[pos] == "x" || in.state[pos] == "o"){
        pos = as.numeric(readline(prompt = "This position is occupied, please choose another one: "))
      }
      
      who = "o"
      in.state = update(in.state, who, pos)
      check_winner(in.state)
      if(check_winner(in.state) == T){
        cat("o wins")
        display(in.state)
        break
      }
    }
    
  }
}

play()
