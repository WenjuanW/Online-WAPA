
adaRDAl1 <- function(streamData1,label1,lambda,eta,delta){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
   k <- 1
  # k <- 3
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)

  w <- matrix(0, nrow = Ncols, ncol = 1)
  g <- matrix(0, nrow = Ncols, ncol = 1)
  g1t <- matrix(0, nrow = Ncols, ncol = 1)
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
    g1t <- g1t + gt^2 
    st <- sqrt(g1t)
    Ht <- delta + st
    indd <- which(abs(g) > lambda)
    if (length(indd)>0){
      w[indd] <- sign(-g[indd])*eta*i/Ht[indd]
      w[-indd] <- 0
    }else{
      w <- matrix(0, nrow = Ncols, ncol = 1)
    }
    

    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   count <- count
    #   AE[i] <- count/i
    # }

  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  newlist <- list("pred" = a, "loss" = loss)
  return(newlist)
}

#adaRDA <- adaRDAl1(x_train,y_train,0.01,0.0016,1) 
