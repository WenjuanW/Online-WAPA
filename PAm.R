
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

PAm <- function(streamData,label,gamma){
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
      m_plus <- (Pm*m_plus+w )/(Pm+1)
    }else{
      m_minus <- (Nm*m_minus+w)/(Nm+1)
    }
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    
    print(lt)
    
    if (lt >0){
      m <- m_plus - m_minus
      stepw <-  (lt+gamma*(1-y[i]*(t(m) %*% w)))/(Norm(streamData[i,])^2)
      w <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])

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



PAmI <- function(streamData,label,c,gamma){
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
      w <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])
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



PAmII <- function(streamData,label,c,gamma){
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
      w <- (1/(1+gamma))*(w + gamma*m+ stepw*y[i]*streamData[i,])

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

PAm <- PAm(x_test,y_test,0.2)
PAmI <- PAmI(x_test,y_test,0.2,1)
PAmII <- PAmII(x_test,y_test,0.2,1)



#crossvalidation for PAm
res_PAm <- matrix(0,nrow = 10,ncol = 1)
for (i in 1:10) {
  gamma <- 5^(5-i)
  res_PAm[i] <- PAm(x_train,y_train,gamma)$pred
}


max(res_PAm)
which(res_PAm == max(res_PAm), arr.ind = TRUE)


#double crossvalidation for PAmI
res_PAmI <- matrix(0,nrow = 10,ncol = 10)
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    res_PAmI[i,j] <- PAmI(x_train,y_train,c,gamma)$pred
  }
}


max(res_PAmI)
which(res_PAmI == max(res_PAmI), arr.ind = TRUE)


#double crossvalidation for PAmII
res_PAmII <- matrix(0,nrow = 100,ncol = 30)
for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:10) {
    gamma <- 5^(5-i)
    res_PAmII[i,j] <- PAmII(x_train,y_train,c,gamma)$pred
  }
}

max(res_PAmII)
which(res_PAmII == max(res_PAmII), arr.ind = TRUE)



