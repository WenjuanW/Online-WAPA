
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

WAPAm <- function(streamData,label,gamma,rho){
  streamData <- as.matrix(streamData)
  y <- as.vector(label)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  #w <- w0
  w <- matrix(0, nrow = Ncols, ncol = 1)
  prediction <- NULL
  
  loss <- matrix(0, nrow = Nrows, ncol = 1)
  AE <- matrix(0, nrow = Nrows, ncol = 1)
  count <- 0
  
  Pm <- 0
  Nm <- 0
  m_plus <- 0
  m_minus <- 0
  
  for(i in 1:Nrows){
    
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      m_plus <- (Pm*m_plus+w)/(Pm+1)
    }else{
      m_minus <- (Nm*m_minus+w)/(Nm+1)
    }
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      m <- m_plus - m_minus
      stepw <-  (lt+gamma*(1-y[i]*(t(m) %*% w)))/(Norm(streamData[i,])^2)
      wt <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])
      w <- (1-rho)*w + rho*wt
    }
    
    if(pred1 !=y[i]){
      count <- count +1
      AE[i] <- count/i
    }else{
      AE[i] <- count/i
    }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(newlist)
}



WAPAmI <- function(streamData,label,c,gamma,rho){
  streamData <- as.matrix(streamData)
  y <- as.vector(label)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  #w <- w0
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- NULL
  
  loss <- matrix(0, nrow = Nrows, ncol = 1)
  AE <- matrix(0, nrow = Nrows, ncol = 1)
  count <- 0
  
  Pm <- 0
  Nm <- 0
  m_plus <- 0
  m_minus <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      m_plus <- (Pm*m_plus+w)/(Pm+1)
    }else{
      m_minus <- (Nm*m_minus+w)/(Nm+1)
    }
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      m <- m_plus - m_minus
      stepw <-  min(c,((lt+gamma*(1-y[i]*(t(m) %*% w)))/(Norm(streamData[i,])^2)))
      wt <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])
      w <- (1-rho)*w + rho*wt
    }
    
    
    if (pred1!=y[i]){
      count <- count +1
      AE[i] <- count/i
    }else{
      AE[i] <- count/i
    }
    
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(newlist)
}



WAPAmII <- function(streamData,label,c,gamma,rho){
  streamData <- as.matrix(streamData)
  y <- as.vector(label)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  w <- matrix(0, nrow = Ncols, ncol = 1)
  #w <- w/Norm(w)
  prediction <- NULL
  loss <- matrix(0, nrow = Nrows, ncol = 1)
  AE <- matrix(0, nrow = Nrows, ncol = 1)
  count <- 0
  
  Pm <- 0
  Nm <- 0
  m_plus <- 0
  m_minus <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      m_plus <- (Pm*m_plus+w)/(Pm+1)
    }else{
      m_minus <- (Nm*m_minus+w)/(Nm+1)
    }
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      m <- m_plus - m_minus
      stepw <-  (lt+gamma*(1-y[i]*(t(m) %*% w)))/(Norm(streamData[i,])^2+(1+gamma)/(2*c))
      wt <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])
      w <- (1-rho)*w + rho*wt
    }
    
    if (pred1==y[i]){
      AE[i] <- count/i
      
    }else{
      count <- count +1
      AE[i] <- count/i
    }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(newlist)
}

WAPAm <- WAPAm(x_test,y_test,0.2,0.02)
WAPAmI <- WAPAmI(x_test,y_test,0.2,1,0.02)
WAPAmII <- WAPAmII(x_test,y_test,0.2,1,0.02)


#crossvalidation for WAPAm
res_WAPAm <- matrix(0,nrow = 10,ncol = 100)
for (i in 1:10) {
  gamma <- 5^(5-i)
  for (j in 1:100){
    rho <- j/100
    res_WAPAm[i,j] <- WAPAm(x_train,y_train,gamma,rho)$pred
  }
}


max(res_WAPAm)
which(res_WAPAm == max(res_WAPAm), arr.ind = TRUE)


#double crossvalidation for WAPAmI
res_WAPAmI <- array(0,c(10,10,100))
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    for (z in 1:100){
      rho <- z/100
      res_WAPAmI[i,j,z] <- WAPAmI(x_train,y_train,c,gamma,rho)$pred
    }
  }
}


max(res_WAPAmI)
which(res_WAPAmI == max(res_WAPAmI), arr.ind = TRUE)


#double crossvalidation for WAPAmII
res_WAPAmII <- array(0,c(10,10,100))
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    for (z in 1:100){
      rho <- z/100
      res_WAPAmII[i,j,z] <- WAPAmII(x_train,y_train,c,gamma,rho)$pred
    }
  }
}


max(res_WAPAmII)
which(res_WAPAmII == max(res_WAPAmII), arr.ind = TRUE)

