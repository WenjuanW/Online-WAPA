
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

WAPAmLDA <- function(streamData,label,gamma,rho){
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
  psum <- matrix(0, nrow = Ncols, ncol = 1)
  nsum <- matrix(0, nrow = Ncols, ncol = 1)
  psumSq <- matrix(0, nrow = Ncols, ncol = 1)
  nsumSq <- matrix(0, nrow = Ncols, ncol = 1)
  splus <- matrix(0, nrow = Ncols, ncol = 1)
  sminus <- matrix(0, nrow = Ncols, ncol = 1)
  
  for(i in 1:Nrows){
    
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      Pm <- Pm+1
      psum <- psum + streamData[i,]
      psumSq <- psumSq+ streamData[i,]^2
      m_plus <- (psum)/(Pm)
      splus <- psumSq/(Pm-1) - m_plus^2*(Pm/(Pm-1)) 
    }else{
      Nm <- Nm+1
      nsum <- nsum + streamData[i,]
      nsumSq <- nsumSq+ streamData[i,]^2
      m_minus <- (nsum)/(Nm)
      sminus <- nsumSq/(Nm-1) - m_minus^2*(Nm/(Nm-1)) 
    }
    wpt <- (m_plus-m_minus)/(splus+sminus)
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      stepw <-  (lt-gamma*y[i]*(t(wpt) %*% streamData[i,]))/(Norm(streamData[i,])^2)
      wt <- w + gamma*wpt +  stepw*y[i]*streamData[i,]
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



WAPAmLDAI <- function(streamData,label,c,gamma,rho){
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
  psum <- matrix(0, nrow = Ncols, ncol = 1)
  nsum <- matrix(0, nrow = Ncols, ncol = 1)
  psumSq <- matrix(0, nrow = Ncols, ncol = 1)
  nsumSq <- matrix(0, nrow = Ncols, ncol = 1)
  splus <- matrix(0, nrow = Ncols, ncol = 1)
  sminus <- matrix(0, nrow = Ncols, ncol = 1)
  
  for(i in 1:Nrows){
    
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      Pm <- Pm+1
      psum <- psum + streamData[i,]
      psumSq <- psumSq+ streamData[i,]^2
      m_plus <- (psum)/(Pm)
      splus <- psumSq/(Pm-1) - m_plus^2*(Pm/(Pm-1)) 
    }else{
      Nm <- Nm+1
      nsum <- nsum + streamData[i,]
      nsumSq <- nsumSq+ streamData[i,]^2
      m_minus <- (nsum)/(Nm)
      sminus <- nsumSq/(Nm-1) - m_minus^2*(Nm/(Nm-1)) 
    }
    wpt <- (m_plus-m_minus)/(splus+sminus)
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      stepw <-  min(c,(lt-gamma*(y[i]*(t(wpt) %*% streamData[i,])))/(Norm(streamData[i,])^2))
      wt <- w + gamma*wpt + stepw*y[i]*streamData[i,]
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



WAPAmLDAII <- function(streamData,label,c,gamma,rho){
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
  psum <- matrix(0, nrow = Ncols, ncol = 1)
  nsum <- matrix(0, nrow = Ncols, ncol = 1)
  psumSq <- matrix(0, nrow = Ncols, ncol = 1)
  nsumSq <- matrix(0, nrow = Ncols, ncol = 1)
  splus <- matrix(0, nrow = Ncols, ncol = 1)
  sminus <- matrix(0, nrow = Ncols, ncol = 1)
  
  for(i in 1:Nrows){
    
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction <- rbind(prediction,pred1)
    
    if (y[i]>0){
      Pm <- Pm+1
      psum <- psum + streamData[i,]
      psumSq <- psumSq+ streamData[i,]^2
      m_plus <- (psum)/(Pm)
      splus <- psumSq/(Pm-1) - m_plus^2*(Pm/(Pm-1)) 
    }else{
      Nm <- Nm+1
      nsum <- nsum + streamData[i,]
      nsumSq <- nsumSq+ streamData[i,]^2
      m_minus <- (nsum)/(Nm)
      sminus <- nsumSq/(Nm-1) - m_minus^2*(Nm/(Nm-1)) 
    }
    wpt <- (m_plus-m_minus)/(splus+sminus)
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    if (lt >0){
      stepw <-  (lt-gamma*y[i]*(t(wpt) %*% streamData[i,]))/(Norm(streamData[i,])^2+1/(2*c))
      wt <- w + gamma*wpt+ stepw*y[i]*streamData[i,]
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

WAPAmLDA <- WAPAmLDA(x_test,y_test,0.2,0.02)
WAPAmLDAI <- WAPAmLDAI(x_test,y_test,0.2,1,0.02)
WAPAmLDAII <- WAPAmLDAII(x_test,y_test,0.2,1,0.02)


#crossvalidation for WAPAmLDA
res_WAPALDAm <- matrix(0,nrow = 10,ncol = 100)
for (i in 1:10) {
  gamma <- 5^(5-i)
  for (j in 1:100){
    rho <- j/100
    res_WAPAmLDA[i,j] <- WAPAmLDA(x_train,y_train,gamma,rho)$pred
  }
}


max(res_WAPAmLDA)
which(res_WAPAmLDA == max(res_WAPAmLDA), arr.ind = TRUE)


#double crossvalidation for WAPAmLDAI
res_WAPAmLDAI <- array(0,c(10,10,100))
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    for (z in 1:100){
      rho <- z/100
      res_WAPAmLDAI[i,j,z] <- WAPAmLDAI(x_train,y_train,c,gamma,rho)$pred
    }
  }
}


max(res_WAPAmLDAI)
which(res_WAPAmLDAI == max(res_WAPAmLDAI), arr.ind = TRUE)


#double crossvalidation for WAPAmLDAII
res_WAPAmLDAII <- array(0,c(10,10,100))
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    for (z in 1:100){
      rho <- z/100
      res_WAPAmLDAII[i,j,z] <- WAPAmLDAII(x_train,y_train,c,gamma,rho)$pred
    }
  }
}


max(res_WAPAmLDAII)
which(res_WAPAmLDAII == max(res_WAPAmLDAII), arr.ind = TRUE)

