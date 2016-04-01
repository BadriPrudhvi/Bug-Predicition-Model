maindir <- "E:\\Project3\\Datasets"
subdir <- list.files(maindir)

jsd = function(x, y) 
{
  maxlen = max(length(x), length(y))
  p = rep(0, maxlen)
  for (i in 1:length(x)) p[i] = x[i]
  p[p==0] = 0.1  
  p = p/sum(p)
  
  q = rep(0, maxlen)
  for (i in 1:length(y)) q[i] = y[i]	
  q[q==0] = 0.1
  q = q/sum(q)
  
  m = 0.5 * (p + q)
  0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m)))
}


for(f in 1:4)
{
  if(subdir[f]=="Eclipse"||subdir[f]=="JetSpeed-2"||subdir[f]=="OpenBSD"||subdir[f]=="Tomcat") 
  {
    value = 1
    list1 <- list()
    setwd(file.path(maindir,paste(subdir[f])))
    files = dir(getwd(),pattern=".txt",recursive=TRUE)
    print(files)
    if(subdir[f]=="Eclipse")
    {
      data.list<-list()
      for(k in 1:6)
      {
        filename = paste('E',k,'.txt', sep='')
        data.list[[k]] = scan(filename)
      }
      for ( i in 1:5)
      {
        for (j in (i+1):6)
        {
          list1[value]=jsd(data.list[[i]],data.list[[j]])
          value=value+1
        }
      }
      boxplot(unlist(list1),ylab="JSD Score",xlab="Versions",main=subdir[f],col=c("cyan"))
    }
    
    if(subdir[f]=="JetSpeed-2")
    {
      data.list1<-list()
      for(k in 1:4)
      {
        filename = paste('J',k,'.txt', sep='')
        data.list1[[k]] = scan(filename)
      }
      for ( i in 1:3)
      {
        for (j in (i+1):4)
        {
          list1[value]=jsd(data.list1[[i]],data.list1[[j]])
          value=value+1
        }
      }
      boxplot(unlist(list1),ylab="JSD Score",xlab="Versions",main=subdir[f],col=c("limegreen"))
    }

    if(subdir[f]=="OpenBSD")
    {
      data.list2<-list()
      for(k in 1:7)
      {
        filename = paste('B',k,'.txt', sep='')
        data.list2[[k]] = scan(filename)
      }
      for ( i in 1:6)
      {
        for (j in (i+1):7)
        {
          list1[value]=jsd(data.list2[[i]],data.list2[[j]])
          value=value+1
        }
      }
      boxplot(unlist(list1),ylab="JSD Score",xlab="Versions",main=subdir[f],col=c("yellow"))
      
    }
    if(subdir[f]=="Tomcat")
    {
      data.list3<-list()
      for(k in 1:4)
      {
        filename = paste('T',k,'.txt', sep='')
        data.list3[[k]] = scan(filename)
      }
      for ( i in 1:3)
      {
        for (j in (i+1):4)
        {
          list1[value]=jsd(data.list3[[i]],data.list3[[j]])
          value=value+1
        }
      }
      boxplot(unlist(list1),ylab="JSD Score",xlab="Versions",main=subdir[f],col=c("firebrick"))
    }
      
  }
}

