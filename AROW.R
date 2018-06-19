
AROW_hinge <- function(streamData1,label1,r){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 

   k <- 1
   #k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  
  u <-  matrix(0, nrow = Ncols, ncol = 1)
  Sigma <- diag(1,Ncols,Ncols)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
   loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(u)%*%streamData[i,])
    prediction[i] <- pred1
    
    vt <- t(streamData[i,])%*%Sigma%*%streamData[i,]
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% u))
    loss[i] <- lt
    if (lt>0){
      stepw <-  min(1/(2*r),lt/vt)
      Betat <- 1/(vt+r)
     
      u <- u + stepw*y[i]*Sigma%*%streamData[i,]
      Sigma <- Sigma - Betat[1,1]*Sigma%*%(streamData[i,])%*%t(streamData[i,])%*%Sigma
    }

    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   AE[i] <- count/i
    # }
    
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  newlist <- list("pred" = a, "loss" = loss)
  return(newlist)
}


