measure<-function(t){
  # input t is confusion matrix
  n<-length(t[1,])
  s<-sum(t)
  x<- matrix(0,nrow=n, ncol=11)
  di<-0
  fp<-0
  fn<-0
  for(i in 1:n){
    x[i,1]=t[i,i]#tp
    x[i,2]=sum(t[i,],na.rm=TRUE)-t[i,i]#fn
    x[i,3]=sum(t[,i],na.rm=TRUE)-t[i,i]#fp
    x[i,4]=t[i,i]/(t[i,i]+sum(t[i,],na.rm=TRUE)-t[i,i])#recall=TP/(TP+FN) 
    x[i,5]=t[i,i]/(t[i,i]+sum(t[,i],na.rm=TRUE)-t[i,i])#precision=TP/(TP+FP) 
    x[i,6]=t[i,i]/sum(t,na.rm=TRUE)#accuracy
    x[i,7]=(2*x[i,5]*x[i,4])/(x[i,5]+x[i,4])#f-measure
  }
  #macro Recall and Precision
  maRecall<-0
  maPrecision<-0
  maRecall <- sum(x[,4], na.rm=TRUE)/n
  maPrecision <- sum(x[,5], na.rm=TRUE)/n
  
  #micro Recall and Precision
  miRecallA <- sum(x[,1], na.rm = TRUE)#sum(TP)
  miRecallB <- sum(x[,1], na.rm = TRUE) + sum(x[,2], na.rm = TRUE)#sum(TP)+sum(FN)
  miPrecisionA <- sum(x[,1], na.rm = TRUE)#sum(TP)
  miPrecisionB <- sum(x[,1], na.rm = TRUE) + sum(x[,3], na.rm = TRUE)#sum(TP)+sum(FP)
  miRecall<-miRecallA/miRecallB
  miPrecision<-miPrecisionA/miPrecisionB
  x[,8]<-miRecall
  x[,9]<-maRecall
  x[,10]<-miPrecision
  x[,11]<-maPrecision

  x<-data.frame(x)
  colnames(x) <- c("TP", "FN","FP","recall","precision","accuracy","f-measure",
                   "microRecall","macroRecall","microPrecision","macroPrecision"
  )
  rownames(x) <- colnames(t)
  print(sum(x[,6]))
  x <- as.data.frame(x)
  return(x)
}
