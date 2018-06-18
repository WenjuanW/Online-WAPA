library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

PAII <- function(streamData1,label1,c){
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
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
  # loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    #loss[i] <- lt
    stepw <-  lt/(Norm(streamData[i,])^2+1/(2*c))
    
    w <- w + stepw*y[i]*streamData[i,]
    
    # if (pred1==y[i]){
    #   AE[i] <- count/i
    # }else{
    #   count <- count +1
    #   AE[i] <- count/i
    # }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(a)
}


APAII <- function(streamData1,label1,c){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
    k <- 1
   # k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  #w <- w0
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
  # loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  for(i in 1:Nrows){
    #print(w)
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    #print(pred1)
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    #loss[i] <- lt
    stepw <-  lt/(Norm(streamData[i,])^2+1/(2*c))
    w <- w + (1/i)*stepw*y[i]*streamData[i,]
    ##w <- (1-1/i)*w + (1/i)*wt
    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   AE[i] <- count/i
    # }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(a)
}



WAPAII <- function(streamData1,label1,c,rho){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 

     k <- 1
    #k <- 3  
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  #w <- w0
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
  # loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    #print(w)
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    #print(pred1)
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    #loss[i] <- lt
    stepw <-  lt/(Norm(streamData[i,])^2+1/(2*c))
    w <- w + (rho)*stepw*y[i]*streamData[i,]
    ##w <- (1-rho)*w + (rho)*wt
    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   AE[i] <- count/i
    # }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  return(a)
}


PAII <- PAII(x_test,y_test,1)
APAII <- APAII(x_test,y_test,1)
WAPAII <- WAPAII(x_test,y_test,1,0.18)  #correct changed
PAII
APAII
WAPAII


plot(PAII$AverageE,type = "l")
lines(APAII$AverageE,col="green")
lines(WAPAII$AverageE,col="red")


plot(cumsum(PAII$loss),type = "l")
lines(cumsum(APAII$loss),col="green")





#find the best c
T <- 10
a <- 5

respred_PAII <- matrix(0,ncol=1,nrow=T)
respred_APAII <- matrix(0,ncol=1,nrow=T)
#res_oSVM <- matrix(0,ncol=1,nrow=T)

for (j in 1:T){
  c <- 5^(a-j)
  
  respred_PAII[j] <- PAII(x_train,y_train,c)
  respred_APAII[j] <- APAII(x_train,y_train,c)
  #res_oSVM[j] <- onlineSvm(x,y,w0,c)
  
}

5^(a-which(respred_PAII == max(respred_PAII)))
5^(a-which(respred_APAII == max(respred_APAII)))
#2^(a-which(res_oSVM == max(res_oSVM)))

max(respred_PAII)
max(respred_APAII)
#max(respred_oSVM)


plot(respred_PAII,type = "l")
#lines(respred_APAII,col = "green")



#double crossvalidation for WAPAII
res_WAPAII <- matrix(0,nrow = 100,ncol = 10)
for (j in 1:100) {
  rho <- j/100
  for (i in 1:10) {
    c <- 5^(5-i)
    res_WAPAII[j,i] <- WAPAII(x_train,y_train,c,rho)
  }
  print(j)
}


max(res_WAPAII)
which(res_WAPAII == max(res_WAPAII), arr.ind = TRUE)


############
###plot rho
res_WAPAII <- matrix(0,nrow = 100,ncol = 1)


for (j in 1:100) {
  rho <- j/100
  res_WAPAII[j] <- WAPAII(x,y,1,rho)
}


plot(as.vector(0.01*(1:100)),res_WAPAII,type = "l",col="blue", ylim = c(0.65,1),xlab = expression(paste(rho," values")),ylab = "Predictive accuracy",main = "MNIST (0%)")
points(1,res_WAPAII[100],col="red")
legend(0,0.785,legend = c("WAPAII","PAII"), lwd=1,lty=c(1,NA),cex=0.8,pch = c(NA,1), col = c("blue","red"))




