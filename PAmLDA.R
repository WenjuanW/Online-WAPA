
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

PAmLDA <- function(streamData,label,gamma){
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
    
    print(lt)
    
    if (lt >0){
      stepw <-  (lt-gamma*y[i]*(t(wpt) %*% streamData[i,]))/(Norm(streamData[i,])^2)
      w <- w + gamma*wpt +  stepw*y[i]*streamData[i,]
      
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



PAmLDAI <- function(streamData,label,c,gamma){
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
      w <- w + gamma*wpt + stepw*y[i]*streamData[i,]
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



PAmLDAII <- function(streamData,label,c,gamma){
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
      w <- w + gamma*wpt+ stepw*y[i]*streamData[i,]
      
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

PAmLDA <- PAmLDA(x_test,y_test,0.002)
PAmLDAI <- PAmLDAI(x_test,y_test,0.2,1)
PAmLDAII <- PAmLDAII(x_test,y_test,0.32,1)



#crossvalidation for PAmLDA
res_PAmLDA <- matrix(0,nrow = 10,ncol = 1)
for (i in 1:5) {
  gamma <- 2^(3-i)
  res_PAmLDA[i] <- PAmLDA(x_train,y_train,gamma)$pred
}


max(res_PAmLDA)
which(res_PAmLDA == max(res_PAmLDA), arr.ind = TRUE)


#double crossvalidation for PAmLDAI
res_PAmLDAI <- matrix(0,nrow = 10,ncol = 10)
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    res_PAmLDAI[i,j] <- PAmLDAI(x_train,y_train,c,gamma)$pred
  }
}


max(res_PAmLDAI)
which(res_PAmLDAI == max(res_PAmLDAI), arr.ind = TRUE)


#double crossvalidation for PAmLDAII
res_PAmLDAII <- matrix(0,nrow = 100,ncol = 30)
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    res_PAmLDAII[i,j] <- PAmLDAII(x_train,y_train,c,gamma)$pred
  }
}

max(res_PAmLDAII)
which(res_PAmLDAII == max(res_PAmLDAII), arr.ind = TRUE)



