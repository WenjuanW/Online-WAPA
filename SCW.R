
SCWI <- function(streamData1,label1,c,eta){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
   k <- 1
  # k <- 3
  
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
    fai <- 1/pnorm(eta)
    
    ## print(Norm(streamData[i,]))
    ## print(Sigma)
    ## print(streamData[i,])
    ## print(vt)
   
    
    lt <- max(0,fai*sqrt(vt)-y[i]*(streamData[i,] %*% u))
    
    l1 <- max(0,1-y[i]*(streamData[i,] %*% u))
    loss[i] <- l1
    if (lt>0){
      
      mt <- y[i]*t(u)%*%streamData[i,]
      pa <- 1+fai^2/2
      xi <- 1+fai^2
      
      stepw <-  min(c,max(0,1/(vt*xi)*(-mt*pa+sqrt(mt^2*fai^4/4+vt*fai^2*xi))))
      
      ut <- 0.25*(-stepw*vt*fai+sqrt(stepw^2*vt^2*fai^2+4*vt))^2
      Betat <- stepw*fai/(sqrt(ut)+vt*stepw*fai)
      
      u <- u + stepw*y[i]*Sigma%*%streamData[i,]
      
      ##print(is.positive.semi.definite(Sigma, tol=1e-250))
      ##print(is.positive.semi.definite(Sigma%*%((streamData[i,])%*%t(streamData[i,]))%*%Sigma, tol=1e-250))
      
      
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

#sc <- SCWI(x_train,y_train,25,0.8)$pred
