tent <- function(x){
  if(x <= 1/2){
    return(2*x)
  }else{
    return(2*(1-x))
  }
}

x_0 = runif(1, 0, 1)
print(x_0)
seq = c()
for(x in 1:100){
  rs = tent(x_0)
  seq = c(seq, rs)
  print(x_0)
  x_0 = rs

}


plot(seq, col = 'red', type = 'l')

#plot(runif(100, 0, 1), type = 'l')


k = 100
x_0 = 1/(2^k)
print(x_0)
seq = c()
counter = 0
for (x in 1:150){
  rs = tent(x_0)
  seq = c(seq, rs)
  print(x_0)
  x_0 = rs
  if(x_0 == 0){
    counter = counter + 1
  }
}

print(150 - counter)

plot(seq, col = 'blue', type = 'l')
