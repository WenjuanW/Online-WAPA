library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

PAI <- function(streamData1,label1,c){
  
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
   k <- 1
  #k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
   loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    stepw <-  min(c,lt/Norm(streamData[i,])^2)
    w <- w + stepw*y[i]*streamData[i,]

    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   AE[i] <- count/i
    # }
    
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  newlist <- list("pred" = a, "loss" = loss)
  return(newlist)
}


APAI <- function(streamData1,label1,c){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
     k <- 1
  # k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
  # loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    #loss[i] <- lt
    stepw <-  min(c,lt/(Norm(streamData[i,])^2))
    w <- w + (1/i)*stepw*y[i]*streamData[i,]
    ## w <- (1-1/i)*w + (1/i)*wt
    # if (pred1!=y[i]){
    #   count <- count +1
    #   AE[i] <- count/i
    # }else{
    #   AE[i] <- count/i
    # }
  }
  a <- length(which(prediction==y[1:nrow(prediction)]))/nrow(prediction)
  #newlist <- list("pred" = a, "loss" = loss,"AverageE" = AE)
  #return(newlist)
  return(a)
}



WAPAI <- function(streamData1,label1,c,rho){
  streamData1 <- as.matrix(streamData1)
  label1 <- as.matrix(label1) 
  
    k <- 1
   #k <- 3
  
  streamData <- apply(streamData1, 2, rep, k)
  y <- apply(label1, 2, rep, k)
  
  Ncols <- ncol(streamData)
  Nrows <- nrow(streamData)
  i <- 1
  w <-  matrix(0, nrow = Ncols, ncol = 1)
  prediction <- matrix(0, nrow = Nrows, ncol = 1)
  
   loss <- matrix(0, nrow = Nrows, ncol = 1)
  # AE <- matrix(0, nrow = Nrows, ncol = 1)
  # count <- 0
  
  for(i in 1:Nrows){
    pred1 <- sign(t(w)%*%streamData[i,])
    prediction[i] <- pred1
    
    lt <- max(0,1-y[i]*(streamData[i,] %*% w))
    loss[i] <- lt
    stepw <-  min(c,lt/(Norm(streamData[i,])^2))
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
  newlist <- list("pred" = a, "loss" = loss)
  return(newlist)
  #return(a)
}

PAI <- PAI(x_test,y_test,1)
APAI <- APAI(x_test,y_test,1)
WAPAI <- WAPAI(x_test,y_test,1,0.15)   #changed
AROW_h <- AROW_hinge(x_test,y_test,1)
SCW <- SCWI(x_test,y_test,0.008,0.5)    #does not work with big c
RDA <- RDAl1(x_test,y_test,0.000064,625)  #changed
adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.3)


s <- rep(NA,500)
plot(cumsum(PAI$loss),type = "o",lty=1, pch=c(4,s),col="red",cex=1,xlab = "Number of samples t",ylab = "Cumulative loss",ylim = c(0,52000),main = "Occupancy (10%)")
#lines(cumsum(APAI$loss),type = "o",lty=1, pch=c(5,s),cex=1,col="red")
lines(cumsum(WAPAI$loss),type = "o",lty=1, pch=c(6,rep(NA,700)),cex=1,col="red")
lines(cumsum(AROW_h$loss),type = "o",lty=1, pch=c(7,rep(NA,700)),cex=1)
lines(cumsum(SCW$loss),type = "o",lty=1, pch=c(8,rep(NA,800)),cex=1,col="black")
lines(cumsum(RDA$loss),type = "o",lty=1, pch=c(9,rep(NA,700)),cex=1,col="green")
lines(cumsum(adaRDA$loss),type = "o",lty=1, pch=c(10,rep(NA,900)),cex=1,col="purple")
legend(0,52000,legend = c("PA-I","WAPA-I","AROW","SCW","RDA","ada-RDA"), lty=1,cex=0.8,pch=c(4,6:10),col = c("red","red","black","blue","green","purple"))



PAI
APAI
WAPAI
AROW_h
SCW
RDA
adaRDA


s <- rep(NA,1000)	
plot(PAI$AverageE,type = "o",lty=1, pch=c(4,s),col="red",cex=1,xlab = "Number of samples t",ylab = "Average error",ylim = c(0.4,0.6))
#lines(APAI$AverageE,type = "o",lty=1, pch=c(5,rep(NA,800)),cex=1,col="red")
lines(WAPAI$AverageE,type = "o",lty=1, pch=c(6,rep(NA,800)),cex=1,col="red")
lines(AROW_h$AverageE,type = "o",lty=1, pch=c(7,rep(NA,700)),cex=1)
lines(SCW$AverageE,type = "o",lty=1, pch=c(8,rep(NA,600)),cex=1,col="blue")
lines(RDA$AverageE,type = "o",lty=1, pch=c(9,s,rep(NA,200)),cex=1.5,col="green")
lines(adaRDA$AverageE,type = "o",lty=1, pch=c(10,s),cex=1,col="purple")
legend(4000,0.6,legend = c("PA-I","APA-I","WAPA-I","AROW","SCW","RDA","ada-RDA"), lty=1,cex=0.8,pch=c(4:10),col = c("red","red","red","black","blue","green","purple"))

# lines(PA$AverageE,type = "l",lty=1,col="red")
# lines(APA$AverageE,type = "l",col="red",lty=2)
# lines(WAPA$AverageE,type = "l",col="green",lty=3)
# lines(PAII$AverageE,type = "l",lty=1,col="green")
# lines(APAII$AverageE,col="green",lty=2)
# lines(WAPAII$AverageE,col="green",lty=3)
# legend(3000,0.9,legend = c("PA-I","WAPA-I","PA","WAPA","PA-II","WAPA-II"), lty=1:2,cex=0.8,col = c("black","black","red","red","green","green"))



plot(cumsum(RDA$loss),type = "o",lty=1, pch=c(9,s),cex=1.5,col="green")



#find the best c
T <- 10
a <- 5

respred_PAI <- matrix(0,ncol=1,nrow=T)
respred_APAI <- matrix(0,ncol=1,nrow=T)
respred_AROW <- matrix(0,ncol=1,nrow=T)

for (j in 1:T){
  c <- 5^(a-j)
  
  respred_PAI[j] <- PAI(x_train,y_train,c)
  respred_APAI[j] <- APAI(x_train,y_train,c)
  respred_AROW[j] <- AROW_hinge(x_train,y_train,c)
  print(j)
}

5^(a-which(respred_PAI == max(respred_PAI)))
5^(a-which(respred_APAI == max(respred_APAI)))
5^(a-which(respred_AROW == max(respred_AROW)))

max(respred_PAI)
max(respred_APAI)
max(respred_AROW)


plot(respred_PAI,type = "l")
lines(respred_APAI,col = "green")
lines(respred_AROW,col = "red")


#double crossvalidation for WAPAI
res_WAPAI <- matrix(0,nrow = 10,ncol = 100)

for (i in 1:10) {
  c <- 5^(5-i)
  for (j in 1:100) {
    rho <- j/100
    res_WAPAI[i,j] <- WAPAI(x,y,c,rho)
  }
  print(i)
}


max(res_WAPAI)
which(res_WAPAI == max(res_WAPAI), arr.ind = TRUE)


###plot rho
res_WAPAI <- matrix(0,nrow = 100,ncol = 1)


  for (j in 1:100) {
    rho <- j/100
    res_WAPAI[j] <- WAPAI(x,y,1,rho)
  }

max(res_WAPAI)
which(res_WAPAI == max(res_WAPAI), arr.ind = TRUE)

plot(as.vector(0.01*(1:100)),res_WAPAI,type = "l",col="blue", ylim = c(0.6,1),xlab = expression(paste(rho," values")),ylab = "Predictive accuracy",main = "MNIST (0%)")
points(1,res_WAPAI[100],col="red")
legend(0,0.785,legend = c("WAPAI","PAI"), lwd=1,lty=c(1,NA),cex=0.8,pch = c(NA,1), col = c("blue","red"))







#double crossvalidation for SCW
res_SCWI <- matrix(0,nrow = 10,ncol = 10)
eta <- 0.5
for (j in 1:10) {
  for (i in 1:10) {
    c <- 5^(5-i)
    res_SCWI[j,i] <- SCWI(x_train,y_train,c,eta)
  }
  eta <- eta + 0.05
  print(j)
}


max(res_SCWI)
which(res_SCWI == max(res_SCWI), arr.ind = TRUE)



#double crossvalidation for RDA
res_RDA <- matrix(0,nrow = 10,ncol = 10)
for (j in 1:10) {
  lambda <- 5^(3-j)
  for (i in 1:10) {
    gamma <- 5^(5-i)
    res_RDA[j,i] <- RDAl1(x_train,y_train,lambda,gamma)
  }
  print(j)
}

max(res_RDA)
ind <- which(res_RDA == max(res_RDA), arr.ind = TRUE)

5^(3-ind[,1])
5^(5-ind[,2])


#double crossvalidation for adaRDA
#have to change lambda for different data set
res_adaRDA <- array(0,c(10,10,10))
for (j in 1:10) {
  lambda <-   5^(3-j)
  for (i in 1:10) {
    eta <- 5^(5-i)
    for (z in 1:10){
      delta <- z/10
      res_adaRDA[j,i,z] <- adaRDAl1(x_train,y_train,lambda,eta,delta)
    }
  }
  print(j)
}

max(res_adaRDA)
which(res_adaRDA == max(res_adaRDA), arr.ind = TRUE)[,]
