# distributed online WAPA
# input: streamData: Matrix, label: Vector, lambda: non negative value
# output: prediction vector
# Copyright (C) 2017  Wenjuan Wang

Data300Coils <- read.csv("/home/wenjuan/Documents/Proteus/sub4/Data298coils.csv")
x <- Data300Coils[,c(-1,-2,-3)]

C28coils <- read.csv("/home/wenjuan/Documents/Proteus/sub4/MappedC28.csv",encoding = "UTF-8")
y1 <- C28coils[,c(-1)]
y1[which(y1<=60)] <- 1
y1[which(y1>60)] <- -1
y <- y1

flat <- C28coils[which(C28coils[,2]<60),2]
nonflat <- C28coils[which(C28coils[,2]>60),2]
size(flat)
size(C28coils)



distributedOnlineWAPA <- function(streamData,label,rho){
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
    
    if (i%%(Nworkers)==0) {
      #worker1
      
      pred1 <- sign(t(wt)%*%streamData[i,])
      prediction[i] <- pred1
      
      lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
      stepw <-  lt/(norm(streamData[i,],"2")^2)
      dev1 <- rho*stepw*y[i]*streamData[i,]
      
    } else {
      #worker2
      pred2 <- sign(t(wt)%*%streamData[i,])
      prediction[i] <- pred2
      
      lt <- max(0,1-y[i]*(streamData[i,] %*% wt))
      stepw <-  lt/(norm(streamData[i,],"2")^2)
      dev2 <- rho*stepw*y[i]*streamData[i,]
      
    }  
    if(!is.null(dev1) && !is.null(dev2) ){
      wt<- wt + dev1 + dev2 
      dev1 <- NULL
      dev2 <- NULL
    }
  }
  
#  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  return(prediction)
}

### call the function

prediction <- distributedOnlineWAPA(x,y,1)

a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)

XX <- array(0,c(100))

  for (j in 1:100){
    rho <- j/100
    
    XX[j] <- distributedOnlineWAPA(x,y,rho)
    
  }



max(XX)
which(XX== max(XX), arr.ind = TRUE)[,]
