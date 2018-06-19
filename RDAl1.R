
RDAl1 <- function(streamData1,label1,lambda,gamma){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
   k <- 1
  # k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  w <- matrix(0, nrow = Ncols, ncol = 1)
  g <- matrix(0, nrow = Ncols, ncol = 1)
  
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
   loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if(lt>0){
      gt <- -y[i]*streamData[i,]
    }else{
      gt <- 0
    }
    g <- (i-1)/i*g + 1/i*gt
    
    
    indd <- which(abs(g) > lambda)
    if (length(indd)>0){
      w[indd] <- -sqrt(i)/gamma*(g[indd]-lambda*sign(g[indd]))
      w[-indd] <- 0
    }else{
      w <- matrix(0, nrow = Ncols, ncol = 1)
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

#RDAl1(x_train,y_train,1,1)$pred





