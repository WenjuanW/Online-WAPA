
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

PA <- function(streamData1,label1){
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
    #print(w)
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    #print(pred1)
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    #loss[i] <- lt
    stepw <-  lt/(Norm(streamData[i,])^2)
    w <- w + stepw*y[i]*streamData[i,]
    
    # if(pred1 !=y[i]){
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


APA <- function(streamData1,label1){
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
  w <- matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
  # loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
   # loss[i] <- lt
    stepw <-  lt/(Norm(streamData[i,])^2)
    
    w <- w + 1/i*stepw*y[i]*streamData[i,]
    
    #w <- (1-1/i)*w + 1/i*wt
    
    # if(pred1 !=y[i]){
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

WAPA <- function(streamData1,label1,rho){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
    k <- 1
  #   k <- 3
  
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
    stepw <-  lt/(Norm(streamData[i,])^2)
    
    w <- w + rho*stepw*y[i]*streamData[i,]
    
   # #w <- (1-rho)*w + rho*wt
    
    # if(pred1 !=y[i]){
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

PA <- PA(x_test,y_test)
APA <- APA(x_test,y_test)
WAPA <- WAPA(x_test,y_test,0.18)


PA
APA
WAPA
#oSvm <- onlineSvm(x,y,w0,1000)
#onlineP <- onlinePerceptron(x,y,w0)
s <- rep(NA,1000)
plot(PA$AverageE,type = "o",lty=1,pch=c(1,s),cex=1, xlab = "Number of samples t",ylab = "Average error",ylim = c(0,1))
lines(PAI$AverageE,type = "o",lty=1,pch=c(2,s,rep(NA,200)),cex=1)
lines(PAII$AverageE,type = "o",lty=1,pch=c(3,s,rep(NA,300)),cex=1)
lines(APA$AverageE,type = "o",lty=1,pch=c(4,s,rep(NA,500)),cex=1,col="green")
lines(APAI$AverageE,type = "o",lty=1,pch=c(5,s,rep(NA,200)),cex=1,col="green")
lines(APAII$AverageE,type = "o",lty=1,pch=c(6,s,rep(NA,700)),cex=1,col="green")
lines(WAPA$AverageE,type = "o",lty=1,pch=c(7,rep(NA,800)),cex=1,col="red")
lines(WAPAI$AverageE,type = "o",lty=1,pch=c(8,s),cex=1,col="red")
lines(WAPAII$AverageE,type = "o",lty=1,pch=c(9,rep(NA,700)),cex=1,col="red")
lines(AROW_h$AverageE,type = "o",lty=1, pch=c(10,rep(NA,700)),cex=1,col="pink")
lines(SCW$AverageE,type = "o",lty=1, pch=c(11,rep(NA,600)),cex=1,col="blue")
lines(RDA$AverageE,type = "o",lty=1, pch=c(12,s,rep(NA,200)),cex=1.5,col="yellow")
lines(adaRDA$AverageE,type = "o",lty=1, pch=c(13,s),cex=1,col="purple")
legend(5000,1.05,legend = c("PA","PA-I","PA-II","APA","APA-I","APA-II","WAPA","WAPA-I","WAPA-II","AROW","SCW","RDA","ada-RDA"), lty=1,cex=0.8,pch=c(1:13),col = c("black","black","black","green","green","green","red","red","red","pink","blue","yellow","purple"))



plot(cumsum(PAI$loss),type = "l")
lines(cumsum(APAI$loss),col="green")
lines(cumsum(adaRDA$loss),col="red")


## calculate rho
res_WAPA <- matrix(0,nrow = 100,ncol = 1)
for (j in 1:100) {
  rho <- j/100
  res_WAPA[j] <- WAPA(x_train,y_train,rho)
  #print(j)
}
plot(as.vector(0.01*(1:100)),res_WAPA,type = "l",col="blue", ylim = c(0.65,1),xlab = expression(paste(rho," values")),ylab = "Predictive accuracy",main = "MNIST (20%)")
points(1,res_WAPA[100],col="red")
legend(0.5,0.985,legend = c("WAPA","PA"), lwd=1,lty=c(1,NA),cex=0.8,pch = c(NA,1), col = c("blue","red"))

max(res_WAPA)
which(res_WAPA == max(res_WAPA))

