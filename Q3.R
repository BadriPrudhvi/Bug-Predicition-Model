maindir <- "E:\\Project3\\Datasets"
subdir <- list.files(maindir)
br = read.csv("E:\\Project3\\Datasets\\BugRate.csv",sep=",",header = TRUE, row.names=1)

predict.scale = function(dat,i){
  scale = lm(scale ~ type + cycle + cost,data = dat)
  scale1 = scale[[1]][1] + scale[[1]][2]+ (scale[[1]][4] * i)
  scale1
}

predict.scale.openbsd = function(dat,i){
  scale = lm(scale ~ shape + type + cycle + cost,data = dat)
  scale1 = scale[[1]][1] + scale[[1]][2]+ scale[[1]][4] + scale[[1]][5] * i
  scale1
}

predict.shape = function(dat,i){
  shape1 = lm(shape ~ scale+type + cycle + cost,data = dat)
  shaped = shape1[[1]][1] + shape1[[1]][2] + shape1[[1]][4] + shape1[[1]][5] * i
  shaped
}

predict.scale.js2 = function(dat,i){
  scale = lm(scale~ shape + type + cycle + cost,data = dat)
  print(scale)
  scale1 = scale[[1]][1] + scale[[1]][2] + scale[[1]][3]+scale[[1]][5] * i
  scale1
}

predict.shape.js2 = function(dat,i){
  shape = lm(shape ~ type+scale + cycle + cost,data = dat)
  print(shape)
  shaped = shape[[1]][1] +shape[[1]][2]+ shape[[1]][4]+ shape[[1]][5] * i
  shaped
}

predict.defect = function(dat, l, k = 2) {
  len = length(dat)
  pred.volume = rep(0, len)
  pred.count = rep(0, len)
  pred.error = rep(0, len)
  actual.volume = sum(dat)
  
  pred.volume[2] = dat[2]/dweibull(2, scale = l, shape = k)
  pred.count[2] = pred.volume[2] * dweibull(2, scale = l, shape = k)
  pred.error[2] = abs(pred.volume[2] - actual.volume)/actual.volume
  
  for (i in 3:len) {
    model = nls(y ~ n*dweibull(t, scale = l, shape = k), 
                data = data.frame(t = 1:(i-1), y = dat[1:(i-1)]), 
                start = list(n = sum(dat[1:(i-1)])))
    
    pred.volume[i] = summary(model)$coefficients[1,1]
    pred.count[i] = pred.volume[i] * dweibull(i, scale = l, shape = k)
    pred.error[i] = abs(pred.volume[i] - actual.volume)/actual.volume
  }
  list(volume = pred.volume, count = pred.count, rae = pred.error)
}


r2= function(count,dat)
{
  res=1-var(count-dat)/var(dat)
  res
}

for(f in 2:5)
{
  if(subdir[f]=="Eclipse"||subdir[f]=="JetSpeed-2"||subdir[f]=="OpenBSD"||subdir[f]=="Tomcat") 
  {
    setwd(file.path(maindir,paste(subdir[f])))
    files = dir(getwd(),pattern=".txt",recursive=TRUE)
    if(subdir[f]=="Eclipse")
    {
      data.list<-list()
      eclipse_r2 <- list()
      for(k in 1:6)
      {
        filename = paste('E',k,'.txt', sep='')
        data.list[[k]] = scan(filename)  
      }
      myData <- as.data.frame(br[-(9:12), ])
      names(myData) <- names(br)
      x=predict.scale(myData,4)
      E1=predict.defect(data.list[[1]],x)
      E2=predict.defect(data.list[[2]],br[8,2],br[8,1])
      E3=predict.defect(data.list[[3]],mean(rbind(br[8,2],br[9,2])),mean(rbind(br[8,1],br[9,1])))
      E4=predict.defect(data.list[[4]],mean(rbind(br[8,2],br[9,2],br[10,2])),mean(rbind(br[8,1],br[9,1],br[10,1])))
      E5=predict.defect(data.list[[5]],mean(rbind(br[8,2],br[9,2],br[10,2],br[11,2])),mean(rbind(br[8,1],br[9,1],br[10,1],br[11,1])))
      E6=predict.defect(data.list[[6]],mean(rbind(br[8,2],br[9,2],br[10,2],br[11,2],br[12,2])),mean(rbind(br[8,1],br[9,1],br[10,1],br[11,1],br[12,1])))
      r2_E1= r2(E1$count,data.list[[1]])
      r2_E2= r2(E2$count,data.list[[2]])
      r2_E3= r2(E3$count,data.list[[3]])
      r2_E4= r2(E4$count,data.list[[4]])
      r2_E5= r2(E5$count,data.list[[5]])
      r2_E6= r2(E6$count,data.list[[6]])
      eclipse_r2[1]= r2_E1
      eclipse_r2[2]= r2_E2
      eclipse_r2[3]= r2_E3
      eclipse_r2[4]= r2_E4 
      eclipse_r2[5]= r2_E5 
      eclipse_r2[6]= r2_E6  
      
      
      plot(data.list[[1]],type='l',col='red',main='Eclipse - E1',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E1$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list[[2]],type='l',col='red',main='Eclipse - E2',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E2$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list[[3]],type='l',col='red',main='Eclipse - E3',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E3$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list[[4]],type='l',col='red',main='Eclipse - E4',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E4$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list[[5]],type='l',col='red',main='Eclipse - E5',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E5$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list[[6]],type='l',col='red',main='Eclipse - E6',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(E6$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      
      plot(E1$rae ,type='l', col = 'blue',main='Eclipse-E1',lwd='2',xlab='Quarters',ylab='RAE')
      lines(E2$rae ,type='l', col = 'green',lwd='2')
      lines(E3$rae ,type='l', col = 'DEEPPINK',lwd='2')
      lines(E4$rae ,type='l', col = 'red',lwd='2')
      lines(E5$rae ,type='l', col = 'cyan',lwd='2')
      lines(E6$rae ,type='l', col = 'black',lwd='2')
      legend("topright",legend=c("E1","E2","E3","E4","E5","E6"),lty=1,lwd=2,
             col=c("BLUE","green","red","DEEPPINK","RED","CYAN","BLACK"),
             cex=0.8,bty="n",text.col=c("BLUE","green","red","DEEPPINK","RED","CYAN","BLACK"),inset=0.01)
    }
    if(subdir[f]=="JetSpeed-2")
    {
      data.list1<-list()
      JS2_r2 <- list()
      for(k in 1:4)
      {
        filename = paste('J',k,'.txt', sep='')
        data.list1[[k]] = scan(filename)  
      }
      myData1 <- as.data.frame(br[1:28, ])
      names(myData1) <- names(br)
      new_br <- br      
      names(new_br) <- names(br)
      sc1=predict.scale.js2(myData1,4)
      sh1=predict.shape.js2(myData1,4)
      new_br <- rbind(myData1,data.frame(row.names = "J1",shape = sh1, scale = sc1, peak=0, type="MW", cycle=4, cost="Free"))
      sc2=predict.scale.js2(new_br,4)
      sh11=predict.shape.js2(new_br,4)
      new_br1 <- rbind(new_br,data.frame(row.names = "J2",shape = sh11, scale = sc2, peak=0, type="MW", cycle=4, cost="Free"))
      
      J1=predict.defect(data.list1[[1]],sc1)
      J2=predict.defect(data.list1[[2]],sc2)
      J3=predict.defect(data.list1[[3]],sc3<-mean(rbind(new_br1[29,2],new_br1[30,2])),
                        sh12<-mean(rbind(new_br1[29,1],new_br1[30,1])))
      new_br1 <- rbind(new_br1,data.frame(row.names = "J3",shape = sh12, scale = sc3, peak=0, type="MW", cycle=4, cost="Free"))
      J4=predict.defect(data.list1[[4]],mean(rbind(new_br1[29,2],new_br1[30,2],new_br1[31,2])),
                        mean(rbind(new_br1[29,1],new_br1[30,1],new_br1[31,1])))
      
      r2_J1= r2(J1$count,data.list1[[1]])
      r2_J2= r2(J2$count,data.list1[[2]])
      r2_J3= r2(J3$count,data.list1[[3]])
      r2_J4= r2(J4$count,data.list1[[4]])
      JS2_r2[1]= r2_J1
      JS2_r2[2]= r2_J2
      JS2_r2[3]= r2_J3
      JS2_r2[4]= r2_J4      
        
      plot(data.list1[[1]],type='l',col='red',main='JetSpeed2 - J1',lwd='2',ylim=c(0,35),
           xlab='Quarters',ylab='Number of Defects')
      lines(J1$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list1[[2]],type='l',col='red',main='JetSpeed2 - J2',lwd='2',ylim=c(0,34),
           xlab='Quarters',ylab='Number of Defects')
      lines(J2$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list1[[3]],type='l',col='red',main='JetSpeed2 - J3',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(J3$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list1[[4]],type='l',col='red',main='JetSpeed2 - J4',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(J4$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)       
      
      plot(J1$rae ,type='l', col = 'blue',,main='JETSPEED-2',lwd='2',xlab='Quarters',ylab='RAE',ylim=c(0,1.7))
      lines(J2$rae,type='l',col='green',lwd='2')
      lines(J3$rae,type='l',col='red',lwd='2')
      lines(J4$rae,type='l',col='black',lwd='2')
      legend("topright",legend=c("J1","J2","J3","J4"),lty=1,lwd=2,col=c("BLUE","green","red","black"),
             cex=0.8,bty="n",text.col=c("BLUE","green","red","black"),inset=0.01)
      
      
    }

    if(subdir[f]=="OpenBSD")
    {
      data.list2<-list()
      bsd_r2 <- list()
      for(k in 1:7)
      {
        filename = paste('B',k,'.txt', sep='')
        data.list2[[k]] = scan(filename)  
      }
      myData2 <- as.data.frame(br[-(2:7),])
      names(myData2) <- names(br)
      x2 =predict.scale.openbsd(myData2,2)
      sh=predict.shape(myData2,2)
      b1=predict.defect(data.list2[[1]],x2,sh)
      b2=predict.defect(data.list2[[2]],br[1,2],br[1,1])
      b3=predict.defect(data.list2[[3]],mean(rbind(br[1,2],br[2,2])),mean(rbind(br[1,1],br[2,1])))
      b4=predict.defect(data.list2[[4]],mean(rbind(br[1,2],br[2,2],br[3,2])),mean(rbind(br[1,1],br[2,1],br[3,1])))
      b5=predict.defect(data.list2[[5]],mean(rbind(br[1,2],br[2,2],br[3,2],br[4,2])),
                        mean(rbind(br[1,1],br[2,1],br[3,1],br[4,1])))
      b6=predict.defect(data.list2[[6]],mean(rbind(br[1,2],br[2,2],br[3,2],br[4,2],br[5,2])),
                        mean(rbind(br[1,1],br[2,1],br[3,1],br[4,1],br[5,1])))
      b7=predict.defect(data.list2[[7]],mean(rbind(br[1,2],br[2,2],br[3,2],br[4,2],br[5,2],br[6,2])),
                        mean(rbind(br[1,1],br[2,1],br[3,1],br[4,1],br[5,1],br[6,1])))
      r2_B1= r2(b1$count,data.list2[[1]])
      r2_B2= r2(b2$count,data.list2[[2]])
      r2_B3= r2(b3$count,data.list2[[3]])
      r2_B4= r2(b4$count,data.list2[[4]])
      r2_B5= r2(b5$count,data.list2[[5]])
      r2_B6= r2(b6$count,data.list2[[6]])
      r2_B7= r2(b7$count,data.list2[[7]])
      bsd_r2[1]= r2_B1
      bsd_r2[2]= r2_B2
      bsd_r2[3]= r2_B3
      bsd_r2[4]= r2_B4 
      bsd_r2[5]= r2_B5 
      bsd_r2[6]= r2_B6 
      bsd_r2[7]= r2_B7       
      plot(data.list2[[1]],type='l',col='red',main='OpenBSD-B1',lwd='2',ylim=c(0,140),
           xlab='Quarters',ylab='Number of Defects')
      lines(b1$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[2]],type='l',col='red',main='OpenBSD-B2',lwd='2',ylim=c(0,90),
           xlab='Quarters',ylab='Number of Defects')
      lines(b2$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[3]],type='l',col='red',main='OpenBSD-B3',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(b3$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[4]],type='l',col='red',main='OpenBSD-B4',lwd='2',ylim=c(0,120),
           xlab='Quarters',ylab='Number of Defects')
      lines(b4$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[5]],type='l',col='red',main='OpenBSD-B5',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(b5$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[6]],type='l',col='red',main='OpenBSD-B6',lwd='2',ylim=c(0,90),
           xlab='Quarters',ylab='Number of Defects')
      lines(b6$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list2[[7]],type='l',col='red',main='OpenBSD-B7',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(b7$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(b1$rae ,type='l', col = 'blue',,main='OpenBSD',lwd='2',xlab='Quarters',ylab='RAE')
      lines(b2$rae,type='l',col='green',lwd='2')
      lines(b3$rae ,type='l', col = 'purple',lwd='2')
      lines(b4$rae ,type='l', col = 'red',lwd='2')
      lines(b5$rae ,type='l', col = 'cyan',lwd='2')
      lines(b6$rae ,type='l', col = 'black',lwd='2')  
      lines(b7$rae ,type='l', col = 'deeppink',lwd='2') 
      legend("topright",legend=c("B1","B2","B3","B4","B5","B6","B7"),lty=1,lwd=2,col=c("BLUE","green","purple","RED","CYAN","BLACK","DEEPPINK"),
             cex=0.8,bty="n",text.col=c("BLUE","green","purple","RED","CYAN","BLACK","DEEPPINK"),inset=0.01)
    
    }
    
    if(subdir[f]=="Tomcat")
    {
      data.list3<-list()
      tom_r2 <- list()
      for(k in 1:4)
      {
        filename = paste('T',k,'.txt', sep='')
        data.list3[[k]] = scan(filename)  
      }
      myData3 <- as.data.frame(br[1:25, ])
      names(myData3) <- names(br)
      x3=predict.scale(myData3,4)
      T1=predict.defect(data.list3[[1]],x3)
      T2=predict.defect(data.list3[[2]],br[25,2],br[25,1])
      T3=predict.defect(data.list3[[3]],mean(rbind(br[25,2],br[26,2])),mean(rbind(br[25,1],br[26,1])))
      T4=predict.defect(data.list3[[4]],mean(rbind(br[25,2],br[26,2],br[27,2])),mean(rbind(br[25,1],br[26,1],br[27,1])))
      r2_T1= r2(T1$count,data.list3[[1]])
      r2_T2= r2(T2$count,data.list3[[2]])
      r2_T3= r2(T3$count,data.list3[[3]])
      r2_T4= r2(T4$count,data.list3[[4]])
      tom_r2[1]= r2_T1
      tom_r2[2]= r2_T2
      tom_r2[3]= r2_T3
      tom_r2[4]= r2_T4       
      
      plot(data.list3[[1]],type='l',col='red',main='TOMCAT - T1',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(T1$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list3[[2]],type='l',col='red',main='TOMCAT - T2',lwd='2',ylim=c(0,370),
           xlab='Quarters',ylab='Number of Defects')
      lines(T2$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list3[[3]],type='l',col='red',main='TOMCAT - T3',lwd='2',ylim=c(0,320),
           xlab='Quarters',ylab='Number of Defects')
      lines(T3$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(data.list3[[4]],type='l',col='red',main='TOMCAT - T4',lwd='2',
           xlab='Quarters',ylab='Number of Defects')
      lines(T4$count,col='green',lwd='2')
      legend("topright",legend=c("Actual Curve","Predicted Curve"),lty=1,lwd=2,col=c("red","green"),
             cex=0.8,bty="n",text.col=c("red","green"),inset=0.01)
      plot(T1$rae ,type='l', col = 'blue',,main='Tomcat',lwd='2',xlab='Quarters',ylab='RAE',ylim=c(0,0.5))
      lines(T2$rae,type='l',col='green',lwd='2')
      lines(T3$rae,type='l',col='red',lwd='2')
      lines(T4$rae,type='l',col='purple',lwd='2')
      legend("topright",legend=c("T1","T2","T3","T4"),lty=1,lwd=2,col=c("BLUE","green","red","purple"),
             cex=0.8,bty="n",text.col=c("BLUE","green","red","purple"),inset=0.01)
      
      
     print(unlist(eclipse_r2))
    print(unlist(JS2_r2))
    print(unlist(bsd_r2))
      print(unlist(tom_r2))
          
    }  
  }
}



  