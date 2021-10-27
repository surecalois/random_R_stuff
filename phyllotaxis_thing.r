phi = (1+sqrt(5))/2
N = 501
a = (3-sqrt(5))*pi
n = 0:(N-1)
th = n*a
r = sqrt(n)

x = r*cos(th)
y = r*sin(th)
plot(x,y,asp = 1)

thc = seq(0,10*2*pi,length.out = 1000)
rc = sqrt(1/a)*sqrt(thc)
plot(rc*cos(thc),rc*sin(thc),col = "red",asp = 1,type = 'l')
points(x,y)
w = 3
points(x[seq(1,N,w)],y[seq(1,N,w)],col = "green")

fibo = function(n){
  a = c(1,1,rep(0,n-2))
  for(i in 3:n){
    a[i] = a[i-1]+a[i-2]
  }
  return(a)
}

s = fibo(50)

fb = 10#s[6]
#colors = brewer.pal(fb,"Paired")
colors = hsv(seq(0,1,length.out = fb+1),1,1)
plot(x,y,asp = 1)
z = sapply(1:fb,function(s){
  points(x[seq(s,N,fb)],y[seq(s,N,fb)],col = colors[s])
  lines(x[seq(s,N,fb)],y[seq(s,N,fb)],col = colors[s])})

q = sapply(5:25,function(fb){
#fb = 10#s[6]
#colors = brewer.pal(fb,"Paired")
colors = hsv(seq(0,1,length.out = fb+1),1,1)
plot(x,y,asp = 1)
z = sapply(1:fb,function(s){
  points(x[seq(s,N,fb)],y[seq(s,N,fb)],col = colors[s])
  lines(x[seq(s,N,fb)],y[seq(s,N,fb)],col = colors[s])})
})

ss = s[7:10]
plot(x,y,asp = 1)
colors = hsv(seq(0,1,length.out = length(ss)+1),1,1)
q = sapply(ss,function(fb){
  z = sapply(1:fb,function(s){
    lines(x[seq(s,N,fb)],y[seq(s,N,fb)],col = colors[ss == fb])})
})

library(tidyverse)
ds = data.frame(x = x, y= y)
ds$n = seq(1,length(x))

k = s[9]
ds$fb = factor(ds$n %% k)
colors = hsv(seq(0,1,length.out = k + 1),1,1)
ds$colors = factor(colors[ds$fb])

p = ggplot(ds) + geom_path(aes(x = x, y = y,color = colors))+ theme(legend.position = "none")+
 # geom_text(data = ds %>% filter(n <= k ),aes(x = x, y = y, label = n))+
  coord_fixed()+scale_color_manual(values = levels(ds$colors))#+theme_void()
print(p)


k = s[5]
ds$fb = factor(ds$n %% k)
colors = hsv(seq(0,1,length.out = k + 1),1,1)
ds$colors = factor(colors[ds$fb])

p = ggplot(ds) + geom_path(aes(x = x, y = y,color = fb),size = 0.2)+ theme(legend.position = "none")+
  # geom_text(data = ds %>% filter(n <= k ),aes(x = x, y = y, label = n))+
  coord_fixed()+scale_color_manual(values = levels(ds$colors))#+theme_void()
print(p)


fseq= 1:30
z = sapply(ds$n,function(x) x %% fseq) %>% t()  #%>% add_column(x = x, y = y)
colnames(z) = paste0("fn_",fseq)
z = as.data.frame(z)
dm = data.frame(x = x, y = y, n = 1:length(x),z)
tdm = gather(dm,key = "fn",value = "track",starts_with("fn"))
tdm$ff = as.numeric(sub("fn_","",tdm$fn))
tdm$colors = hsv(h = tdm$track/(tdm$ff+1))

tdm$fn = factor(tdm$fn,levels = paste0("fn_",fseq))
#tdm$track = factor(tdm$track )
p = ggplot(data = tdm) + geom_path(aes(x = x,y = y, color = colors),size = 0.2)+facet_wrap(~fn) +
      coord_fixed()+theme_void()+theme(legend.position = "none")
print(p)

p = ggplot(data = tdm) + geom_point(aes(x = x,y = y, color = colors),size = 0.2)+facet_wrap(~fn) +
  coord_fixed()+theme_void()+theme(legend.position = "none")
print(p)


k = s[9]
ds$fb = factor(ds$n %% k)
colors = hsv(seq(0,1,length.out = k + 1),1,1)
ds$colors = factor(colors[ds$fb])

d0 = data.frame(x = rep(0,k),y = rep(0,k),n = rep(0,k),fb = seq(0,k-1),colors = colors[1:k])

p = ggplot() +geom_path(data = rbind(d0,ds),aes(x = x, y = y,color = colors))+ theme(legend.position = "none")+
        coord_fixed()+scale_color_manual(values =levels(ds$colors))#+theme_void()
print(p)


N = 1501
a = (3-sqrt(5))*pi #137.5/180*pi
n = 0:(N-1)
th = n*a
r = sqrt(n)

x = r*cos(th)
y = r*sin(th)

thc = seq(0,500*2*pi,length.out = 6000)
rc = sqrt(1/a)*sqrt(thc)

w = 55    #34   #21     #13    #8   #5    # 3 #2
l = -1/2584 #1/987#-1/377 #-1/144#1/55#1/21 #1/8#1/3
plot(x,y,asp = 1,col = "gray")
#lines(rc*cos(thc),rc*sin(thc),col = "lightblue")
points(x[seq(1,N,w)],y[seq(1,N,w)],col = "red")
lines(rc*cos(thc*l),-rc*sin(thc*l),col = "black")
print(s)
#text(x[w+1],y[w+1],toString(w))


w = 55    #34   #21     #13    #8   #5    # 3 #2
l = -1/2584 #1/987#-1/377 #-1/144#1/55#1/21 #1/8#1/3
plot(x,y,asp = 1,col = "gray")
#lines(rc*cos(thc),rc*sin(thc),col = "lightblue")
points(x[seq(2,N,w)],y[seq(2,N,w)],col = "red")
lines(rc*cos(thc*l-a),-rc*sin(thc*l-a),col = "black")
print(s)
#text(x[w+1],y[w+1],toString(w))

w = 13    #34   #21     #13    #8   #5    # 3 #2
l = 1/144 #1/987#-1/377 #-1/144#1/55#1/21 #1/8#1/3
plot(x,y,asp = 1,col = "gray")
#lines(rc*cos(thc),rc*sin(thc),col = "lightblue")
points(x[seq(3,N,w)],y[seq(3,N,w)],col = "red")
lines(rc*cos(thc*l-2*a),-rc*sin(thc*l-2*a),col = "black")
print(s)
#text(x[w+1],y[w+1],toString(w))

s = fibo(50)
k = seq(0,1000)
ss = 5
theta = pi*(3-sqrt(5))*(ss + s[9]*k)
rho = sqrt(ss+s[3]*k)
xt = rho*cos(theta)
yt = rho*sin(theta)
plot(x,y,asp = 1, col = "gray")
points(xt,yt,col = "red")


N = 1501
a = (3-sqrt(5))*pi #137.5/180*pi
n = 0:(N-1)
th = n*a

ii = seq(2,20)
k = length(ii)
colors = hsv(h = seq(0,1,length.out = k +1))
for(j in seq(1,k)){
  ind = seq(1,N,ii[j])
  plot(th %% (2*pi),col = "gray",main = toString(ii[j]))
  points(ind, th[ind] %%(2*pi),col = colors[j],pch = 3)
  lines(ind,th[ind] %% (2*pi),col= colors[j])
  #points(ind, th[ind+1] %%(2*pi),col = colors[j],pch = 3)
}

j = 19
ind = seq(1,N,ii[j])
plot(th %% (2*pi),col = "gray",main = toString(ii[j]))
points(ind, th[ind] %%(2*pi),col = colors[j],pch = 3)
lines(ind, th[ind] %%(2*pi),col = colors[j])

w = 21
ind = seq(1,N,w)
plot(x,y,asp = 1, col = "gray",main = toString(w))
lines(x[ind],y[ind],pch = 3)
plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")


w = s[7:10]
k = length(w)
colors = hsv(h = seq(0,1,length.out = k +1))
plot(th %% (2*pi),col = "gray")
for(j in 1:k ){
  ind = seq(1,N,w[j])
  lines(ind, th[ind] %%(2*pi),col = colors[j])
  cat(w[j])
  cat(" => ")
  print(diff(th[ind[1:3]] %% (2*pi)) %% (2*pi))
}

w = 55
ind = seq(1,N,w)
h = diff(th[ind[1:2]] %% (2*pi)) %% (2*pi)
if(h > 1) h = h - 2*pi
h = h/w

t = seq(0,N,length.out = 1e4)
thc = h*t
rc = sqrt(t)

plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t,thc,col = "red")

plot(x,y,asp = 1,col = "gray")
points(x[ind],y[ind],col = "red")
lines(rc*cos(thc),rc*sin(thc),col = "black")



w = 55
st = 44
ind = seq(st,N,w)
h = diff(th[ind[1:2]] %% (2*pi)) %% (2*pi)
if(h > 1) h = h - 2*pi
h = h/w
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)

plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t,thc,col = "red")

plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
points(x[ind],y[ind],col = "red")
lines(rc*cos(thc),rc*sin(thc),col = "black")


w = 55
plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
t = seq(0,N,length.out = 1e4)
colors = hsv(h = seq(0,1,length.out = w+1))
for(st in seq(1,w)){
  ind = seq(st,N,w)
  h = diff(th[ind[1:2]] %% (2*pi)) %% (2*pi)
  if(h > 1) h = h - 2*pi
  h = h/w
  print(paste0(st," => ", h))
  thc = h*(t-ind[1]) + th[st] %% (2*pi) #t = ind[1] => thc = b with slop h
  rc = sqrt(t)
  
  points(x[ind],y[ind],col = colors[st])
  lines(rc*cos(thc),rc*sin(thc),col = colors[st])
  #print(paste0(c("done with ",toString(st))))
}

u = 3-sqrt(5)
uu = u*seq(1,1000)
plot(uu %% 2,col = "gray")#,pch =20)
ind = seq(1,1000,11)
points(ind,uu[ind] %% 2,pch = 3)
lines(ind,uu[ind] %% 2,col = "blue")


phi = (1-sqrt(5))/2
phiz = phi %% 1
u = phi*seq(0,1000)
w = 17
ind = seq(1,1000,w)
plot(u %% 1,col = "gray",main = toString(w))
points(ind,u[ind] %% 1,pch = 3)
lines(ind,u[ind] %% 1,col = "blue")
h = diff(u[ind[1:5]]) %% 1
print(1 %/% h + 1)
if(h[1] > phiz) h = h - 1

phi = (1-sqrt(5))/2
phiz = phi %% 1
u = phi*seq(0,1000)
for(w in seq(2:60)){
  ind = seq(1,1000,w)
  plot(u %% 1,col = "gray",main = toString(w))
  points(ind,u[ind] %% 1,pch = 3)
  lines(ind,u[ind] %% 1,col = "blue")
  h = diff(u[ind[1:5]]) %% 1
  if(h[1] > phiz) h = h - 1
  hw = 1+ 1 %/% h
  if(h[1] <0 ) hw = (1 - 1 %/% h )
  if(hw[1] > 0){
    cat(paste0(w," => "))
    print(hw)
  }
  
}

N = 1000
u = seq(1,N)
plot(u,(phi*u) %% 1,col = "gray")
uu = seq(3,N,s[9])
points(uu,(phi*uu) %% 1,pch = 3)
v = (phi*u) %% 1
vs = sort(v,index.return=TRUE)



w = 3*55
st = 44
ind = seq(st,N,w)
h = diff(th[ind[1:2]] %% (2*pi)) %% (2*pi)
if(h > 1) h = h - 2*pi
h = h/w
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)

plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t,thc,col = "red")

plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
points(x[ind],y[ind],col = "red")
lines(rc*cos(thc),rc*sin(thc),col = "black")

w = 11
st = 1
l = 10
ind = seq(st,N,w)
h = diff(th[ind],l) %% (2*pi)
h = h[1]
if(h > pi) h = h - 2*pi
h = h/w/l
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)

plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t+ind[2],thc+th[ind[2]] %% (2*pi),col = "red")

plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
points(x[ind],y[ind],col = "red")
lines(rc*cos(thc),rc*sin(thc),col = "black")



w = 33
st = 3
ind = seq(st,N,w)
zl = min((w-1),length(ind)-1)
zm = sapply(1:zl, function(l){
  h = diff(th[ind],l) %% (2*pi)
  h = h[1]
  if(h > pi) h = h - 2*pi
  return(h)
})

hi = which.min(abs(zm)/(1:zl))
cat("# of branches:")
print(hi)
h = zm[hi]
h = h/w/hi
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)

plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t+ind[2],(-b+thc+th[ind[2]] )%% (2*pi),col = "red")

plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
points(x[ind],y[ind],col = "red")
lines(rc*cos(thc),rc*sin(thc),col = "black")

plot(zm/(1:zl))

library(numbers)
primeFactors(610)


w = 61
N = 100*w + 10
a = (3-sqrt(5))*pi
n = 0:(N-1)
th = n*a
r = sqrt(n)

x = r*cos(th)
y = r*sin(th)
#plot(x,y,asp = 1)

w = 33
st = 1
l = 5
ind = seq(st,N,w)
h = diff(th[ind],l) %% (2*pi)
h = h[1]
if(h > pi) h = h - 2*pi
h = h/w/l
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)


plot(th %% (2*pi),col = "gray",main = toString(w))
points(ind, th[ind] %%(2*pi),pch = 3)
lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t+ind[2],(-b+thc+th[ind[2]] )%% (2*pi),col = "red")

plot(x,y,asp = 1,col = "gray")
points(0,0,pch=3)
points(x[ind],y[ind],col = "red")

lines(rc*cos(thc),rc*sin(thc),col = "black")



w = 8*18
st = 3
ind = seq(st,N,w)
zl = min((w-1),length(ind)-1)
zm = sapply(1:zl, function(l){
  h = diff(th[ind],l) %% (2*pi)
  h = h[1]
  if(h > pi) h = h - 2*pi
  return(h)
})
plot(x[ind],y[ind],col = "red",asp = 1)


w =18
st = 3
ind = seq(st,N,w)
plot(x[ind],y[ind],col = "red",asp = 1,main = toString(w))
w =8
ind = seq(st,N,w)
points(x[ind],y[ind],pch = 3,col = "gray")
w =8*18
ind = seq(st,N,w)
points(x[ind],y[ind],pch = 2,col = "blue")

sprintf("%.50f",phi*8*18) #144
sprintf("%.50f",phi*233)

ps = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)

w = 377
sth = function(w){
  N = 30*w + 10
  #print(N)
  a = (3-sqrt(5))*pi
  n = 0:(N-1)
  th = n*a
  r = sqrt(n)
  
st = 1
ind = seq(st,N,w)
h = diff(th[ind[1:2]] %% (2*pi)) %% (2*pi)
if(h > pi) h = h - 2*pi
h = h/w
b = th[st] %% (2*pi)
t = seq(0,N,length.out = 1e4)
thc = h*(t-ind[1]) + b #t = ind[1] => thc = b with slop h
rc = sqrt(t)
x = r*cos(th)
y = r*sin(th)


#plot(th %% (2*pi),col = "gray",main = toString(w))
#points(ind, th[ind] %%(2*pi),pch = 3)
plot(ind, th[ind] %%(2*pi),pch = 3,main = toString(w))
#lines(ind, th[ind] %%(2*pi),col = "gray")
lines(t,thc %% (2*pi),col = "red")

#plot(x,y,asp = 1,col = "gray")
#points(0,0,pch=3)
#points(x[ind],y[ind],col = "red")
plot(x[ind],y[ind],col = "red",main = toString(w),asp = 1)
lines(rc*cos(thc),rc*sin(thc),col = "black")
}

