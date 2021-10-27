library(tidyverse)

bernstein = function(k,n){
  return(function(x){
    choose(n,k)*x^k*(1-x)^(n-k)}
  )
}

N = 5
a = sapply(0:N,function(x) bernstein(x,N))

z = sapply(a, function(f){
  t = seq(0,1,0.02)
  return(f(t))
})

zd = data.frame(t = seq(0,1,0.02),z)
gzd = gather(zd,"key","value",-1)
p = ggplot(data =gzd) +geom_line(aes(x = t,y = value, color = key))
print(p)

#---just use the dbinom to get it
t = seq(0,1,0.02)
s = sapply(t, function(p){dbinom(0:N,N,p)})
ss = data.frame(t = t, t(s))
gss = gather(ss,"key","value",-1)
p = ggplot(data =gss) +geom_line(aes(x = t,y = value, color = key))
print(p)


#pick N+1 random points
N = 3
t = seq(0,1,0.02)
s = sapply(t, function(p){dbinom(0:N,N,p)})

x = runif(N+1,max = 10)
y = runif(N+1,max = 10)
bx = matrix(x,ncol = N+1) %*% s
by = matrix(y,ncol = N+1) %*% s
plot(x,y,col = "red")
lines(x,y,col = "grey")
lines(bx,by)

#include the lower order
N = 5
t = seq(0,1,0.02)
s.list = sapply(1:N,
               function(w){
                 return(sapply(t,function(p){dbinom(0:w,w,p)}))
                 })

#x = runif(N+1,max = 10) 
x = c(2.6122230, 8.9649724, 2.3694770, 8.1857733, 0.1079161, 6.3198825)
#y = runif(N+1,max = 10) 
y = c(9.127837, 8.970236, 2.930961, 3.065698, 6.277990, 8.775528)

bx = matrix(x,ncol = N+1) %*% s.list[[N]]
by = matrix(y,ncol = N+1) %*% s.list[[N]]

plot(x,y,col = "red")
lines(x,y,col = "grey")
lines(bx,by)
colors = c('lightblue','pink','lightgreen')

for(k in seq(3,5)){
  bxq = matrix(x[1:k],ncol = k) %*% s.list[[k-1]]
  byq = matrix(y[1:k],ncol = k) %*% s.list[[k-1]]
  lines(bxq,byq,col = colors[k-2])
}

plot(x,y,col = "red")
colors = c('grey','lightblue','pink','lightgreen','black')

for(k in seq(2,6)){
  bxq = matrix(x[1:k],ncol = k) %*% s.list[[k-1]]
  byq = matrix(y[1:k],ncol = k) %*% s.list[[k-1]]
  lines(bxq,byq,col = colors[k-1])
}


plot(x,y,col = "red")
lines(x,y,col = "grey")
lines(bx,by)
colors = c('lightblue','pink','lightgreen')
for(k in seq(2,4)){
  for(j in 1:(N+1-k)){
    bxq = matrix(x[j:(j+k)],ncol = k+1) %*% s.list[[k]]
    byq = matrix(y[j:(j+k)],ncol = k+1) %*% s.list[[k]]
    lines(bxq,byq,col = colors[k-1] )
  }
}
