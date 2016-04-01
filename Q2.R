maindir <- "E:\\Project3\\Datasets"
subdir <- list.files(maindir)

fit.weibull.eclipse = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 2.4 , k = 7.1))
}

fit.weibull.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 1.3 , k = 4.1))
}

fit.weibull = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list( l = 1.5, k = 4))
}

fit.rayleigh = function(dat) {
  nls(y ~ n*dweibull(t, scale = l, shape = 2), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(n = sum(dat), l = which.max(dat)))
} 

fit.rayleigh.jetspeed2 = function(dat) {
  nls(y ~ n*dweibull(t, scale = l, shape = 2), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(n = sum(dat),l = 2.6))
}

fit.gamma.eclipse = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 0.5  , k = 2))
}

fit.gamma.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dgamma(t, scale = l, shape = k ), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 0.8, k = 5))
}

fit.gamma.openbsd = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 1  , k = 1.8))
}

fit.gamma.tomcat = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 3.5  , k = 1))
}

r2 = function(d1,d2){
  r=1 - var(d2-d1)/var(d2)
  r
}

list1 = list2 = list3 = list4 = list5 = list6 = list()
setwd(file.path(maindir,paste(subdir[1])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[1]=="Eclipse")
{
  data.list<-list()
  for(k in 1:6)
  {
    filename = paste('E',k,'.txt', sep='')
    data.list[[k]] = scan(filename)
  }
  for(i in 1:6)
  {
    wei = fit.weibull.eclipse(data.list[[i]])
    ray = fit.rayleigh(data.list[[i]])
    gam = fit.gamma.eclipse(data.list[[i]])
    list1[i] = AIC(wei)
    list2[i] = AIC(ray)
    list3[i] = AIC(gam)
    list4[i] = r2(fitted.values(wei),data.list[[i]])
    list5[i] = r2(fitted.values(ray),data.list[[i]])
    list6[i] = r2(fitted.values(gam),data.list[[i]])
  }
  a=t.test(unlist(list1),unlist(list2),paired=T)
  b=t.test(unlist(list2),unlist(list3),paired=T)
  c=t.test(unlist(list3),unlist(list1),paired=T)
  a1=t.test(unlist(list4),unlist(list5),paired=T)
  b1=t.test(unlist(list5),unlist(list6),paired=T)
  c1=t.test(unlist(list4),unlist(list6),paired=T)
  
}


list11 = list12 = list13 = list14 = list15 = list16 = list()
setwd(file.path(maindir,paste(subdir[2])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[2]=="JetSpeed-2")
{
  data.list1<-list()
  for(k in 1:4)
  {
    filename = paste('J',k,'.txt', sep='')
    data.list1[[k]] = scan(filename)
  }
  for ( i in 1:4)
  {
    wei = fit.weibull.jetspeed2(data.list1[[i]])
    ray = fit.rayleigh.jetspeed2(data.list1[[i]])
    gam = fit.gamma.jetspeed2(data.list1[[i]])
    list11[i] = AIC(wei)
    list12[i] = AIC(ray)
    list13[i] = AIC(gam)
    list14[i] = r2(fitted.values(wei),data.list1[[i]])
    list15[i] = r2(fitted.values(ray),data.list1[[i]])
    list16[i] = r2(fitted.values(gam),data.list1[[i]])
  }
  x=t.test(unlist(list11),unlist(list12),paired=T)
  y=t.test(unlist(list12),unlist(list13),paired=T)
  z=t.test(unlist(list13),unlist(list11),paired=T)
  x1=t.test(unlist(list14),unlist(list15),paired=T)
  y1=t.test(unlist(list15),unlist(list16),paired=T)
  z1=t.test(unlist(list14),unlist(list16),paired=T)  
}



list21 = list22 = list23 = list24 = list25 = list26 = list()
setwd(file.path(maindir,paste(subdir[3])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[3]=="OpenBSD")
{
  data.list2<-list()
  for(k in 1:7)
  {
    filename = paste('B',k,'.txt', sep='')
    data.list2[[k]] = scan(filename)
  }
  for ( i in 1:7)
  {
    
    wei = fit.weibull(data.list2[[i]])
    ray = fit.rayleigh(data.list2[[i]])
    gam = fit.gamma.openbsd(data.list2[[i]])
    list21[i] = AIC(wei)
    list22[i] = AIC(ray)
    list23[i] = AIC(gam)
    list24[i] = r2(fitted.values(wei),data.list2[[i]])
    list25[i] = r2(fitted.values(ray),data.list2[[i]])
    list26[i] = r2(fitted.values(gam),data.list2[[i]])
  }
  x2=t.test(unlist(list21),unlist(list22),paired=T)
  y2=t.test(unlist(list22),unlist(list23),paired=T)
  z2=t.test(unlist(list23),unlist(list21),paired=T)
  x21=t.test(unlist(list24),unlist(list25),paired=T)
  y21=t.test(unlist(list25),unlist(list26),paired=T)
  z21=t.test(unlist(list24),unlist(list26),paired=T)

}


list31 = list32 = list33 = list34 = list35 = list36 = list()
setwd(file.path(maindir,paste(subdir[4])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[4]=="Tomcat")
{
  data.list3<-list()
  for(k in 1:4)
  {
    filename = paste('T',k,'.txt', sep='')
    data.list3[[k]] = scan(filename)
  }
  for ( i in 1:4)
  {
    wei = fit.weibull(data.list3[[i]])
    ray = fit.rayleigh(data.list3[[i]])
    gam = fit.gamma.tomcat(data.list3[[i]])
    list31[i] = AIC(wei)
    list32[i] = AIC(ray)
    list33[i] = AIC(gam)
    list34[i] = r2(fitted.values(wei),data.list3[[i]])
    list35[i] = r2(fitted.values(ray),data.list3[[i]])
    list36[i] = r2(fitted.values(gam),data.list3[[i]])
    
  }
  x3=t.test(unlist(list31),unlist(list32),paired=T)
  y3=t.test(unlist(list32),unlist(list33),paired=T)
  z3=t.test(unlist(list33),unlist(list31),paired=T)
  x31=t.test(unlist(list34),unlist(list35),paired=T)
  y31=t.test(unlist(list35),unlist(list36),paired=T)
  z31=t.test(unlist(list34),unlist(list36),paired=T)
  print(cbind(a,b,c,a1,b1,c1))
  print(cbind(x,y,z,x1,y1,z1))
  print(cbind(x2,y2,z2,x21,y21,z21))
  print(cbind(x3,y3,z3,x31,y31,z31))
  
}
       