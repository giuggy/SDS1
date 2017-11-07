generator <- function(tables, sym){
  res = data.frame(tables[1])
  tables[1] = NULL
  for(t in tables){
    pos_av = which(is.na(t))
    for(p in pos_av){
      tmp = t
      tmp[p] = sym
      res = data.frame(res,tmp)
    }
  }
  return(res)
}

winner <- function(g){
  game = matrix(g, nrow = 3, ncol = 3)
  
  for (i in 1:3){
    s_row = sum(game[i,])
    s_col = sum(game[,i])
    
    if(!is.na(s_row)){
      if(s_row == 3){
        return('x')
      }
      else if(s_row == 0){
        return('o')
      } 
    }
    if(!is.na(s_col)){
      if(s_col == 3) return('x')
      else if(s_col == 0) return('o')
    }
  }
  s_diag = sum(diag(game))
  s_diag_opp = sum(diag(game[1:3, rev(1:3)]))
  
  
  if(!is.na(s_diag)){
    if(s_diag == 3) return('x')
    else if(s_diag == 0) return('o')
  }
  
  if(!is.na(s_diag_opp)){
    if( s_diag_opp == 3) return('x')
    else if( s_diag_opp == 0) return('o')
  }
  
  return('t')
}

verify <- function(tables){
  print('sono qui')
  c = 0
  res = data.frame(tables[1])
  tables[1] = NULL
  for(t in tables){
    c = c + 1
    s = winner(t)
    if(s == 'x'){
      res[1, 1] = res[1, 1] + 1
      res[4,1] =  res[4,1] + 1
    } else if(s == 'o'){
      res[2,1] = res[2,1] + 1
      res[4,1] =  res[4,1] + 1
    } else {
      res = data.frame(res, t)
    }
  }
  return(res)
}


tables = data.frame(res = c(0,0,0,0,NA,NA,NA,NA,NA),tmp = c(NA,NA,NA,NA,NA,NA,NA,NA,NA))
i = 1

while(i < 10){
  print(i)
  k = 10 - i
  if(i %% 2 != 0){
    tables = generator(tables, 1)
  } else {
    tables = generator(tables, 0)
  }
  if(i >= 5){
    tables = verify(tables)
    print(tables[1])
  }
  if( i == 9){
    tables[3,1] = length(tables - 1)
    tables[4,1] = tables[4,1] + length(tables - 1)
  }
  i = i + 1
}
result = data.frame(x_win = tables[1,1], o_win = tables[2,1], tie = tables[3,1], tot =tables[4,1])
print(result)