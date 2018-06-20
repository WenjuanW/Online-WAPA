# distributed online WAPA
# input: streamData: Matrix, label: Vector, lambda: non negative value
# output: prediction vector
# Copyright (C) 2017  Wenjuan Wang

library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
distributedOnlineWAPAII <- function(streamData,label,c,rho){
  streamData <- as.matrix(streamData)
  y <- as.vector(label)
  
  Nrows <- nrow(streamData)
  Ncols <- ncol(streamData)
  Nworkers <- 2
  i<- 1
  
  wt <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  dev1 <- NULL
  dev2 <- NULL
  # while(i <= Nrows){
  #  
  #   if(j%%(Nworkers+1)==0){
  #       wt<- wt + dev1 + dev2 
  #       i <- i
  #   }else if (j%%(Nworkers+1)==1) {
  #     #worker1
  #     pred1 <- sign(t(wt)%*%streamData[i,])
  #     prediction[i] <- pred1
  #     
  #     lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
  #     stepw <-  min(c,lt/(Norm(streamData[i,])^2))
  #     dev1 <- rho*stepw*y[i]*streamData[i,]
  #     i < - i+1
  #   } else {
  #     #worker2
  #     pred2 <- sign(t(wt)%*%streamData[i,])
  #     prediction[i] <- pred2
  #     
  #     lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
  #     stepw <-  min(c,lt/(Norm(streamData[i,])^2))
  #     dev2 <- rho*stepw*y[i]*streamData[i,]
  #     i <- i+1
  #   }
  #   j <- j+1
  # }
  
  for (i in 1:Nrows){
    if(!is.null(dev1) && !is.null(dev2) ){
      wt<- wt + dev1 + dev2 
      dev1 <- NULL
      dev2 <- NULL
    }
    if (i%%(Nworkers)==0) {
      #worker1
      
      pred1 <- sign(t(wt)%*%streamData[i,])
      prediction[i] <- pred1
      
      lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
      stepw <-   lt/(Norm(streamData[i,])^2+1/(2*c))
      dev1 <- rho*stepw*y[i]*streamData[i,]
      
    } else {
      #worker2
      pred2 <- sign(t(wt)%*%streamData[i,])
      prediction[i] <- pred2
      
      lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
      stepw <-   lt/(Norm(streamData[i,])^2+1/(2*c))
      dev2 <- rho*stepw*y[i]*streamData[i,]
      
    }  

  }
  
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  return(a)
}

### call the function

DOWAPA <- distributedOnlineWAPAII(x_test,y_test,0.008,0.26)
DOWAPA

XX <- array(0,c(10,100))
for (i in 1:10) {
  c <- 5^(3-i)
  for (j in 1:100){
    rho <- j/100
    
    XX[i,j] <- distributedOnlineWAPAII(x,y,c,rho)
    
  }
  print(i)
}


max(XX)
which(XX== max(XX), arr.ind = TRUE)[,]
