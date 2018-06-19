
##PA synthetic dataset
library(MASS)

Sigma <- matrix(c(0.2,0,0,2),ncol = 2,nrow = 2)
DataP <- mvrnorm(n = 5000, c(1,1), Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
DataN <- mvrnorm(n = 5000, c(-1,-1), Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

yP <- rep(1,5000)
yN <- rep(-1,5000)

Data <- rbind(DataP,DataN)
Y <- as.matrix(c(yP,yN))
ind <- sample.int(10000,10000, replace = FALSE, prob = NULL)

data1 <- Data[ind,]
y <- Y[ind]

Wd <- as.data.frame(cbind(data1,y))

plot(Wd$V1,Wd$V2,col=ifelse(y==1,"red","black"))


#contaminate instance
# WAPA <- WAPA(x_test,y_test,0.01)
# PAI <- PAI(x_test,y_test,0.008)
# APAI <- APAI(x_test,y_test,5)
# WAPAI <- WAPAI(x_test,y_test,0.008,0.15)
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.04,0.6)
# RDA <- RDAl1(x_test,y_test,0.79,25)
# adaRDA <- adaRDAl1(x_test,y_test,0.754,0.008,0.1)
# PAII <- PAII(x_test,y_test,0.0016)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,0.008,0.11)

noise <- mvrnorm(n = 10000, c(0,0), Sigma = matrix(c(1,0,0,1),ncol = 2,nrow = 2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
plot(noise)

x <- data1[1:10000,] + noise

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:10000,]
y_test <- y[5001:10000]

#contaminate label
#noise10
# WAPA <- WAPA(x_test,y_test,0.01)
# PAI <- PAI(x_test,y_test,0.25)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,0.04,0.4)
# AROW_h <- AROW_hinge(x_test,y_test,1)
# SCW <- SCWI(x_test,y_test,0.2,0.6)
# RDA <- RDAl1(x_test,y_test,0.754,25)
# adaRDA <- adaRDAl1(x_test,y_test,0.754,0.005,0.1)
# PAII <- PAII(x_test,y_test,0.625)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,0.04,0.13)


ind1 <- sample.int(10000, 3000, replace = FALSE, prob = NULL)
y1 <- y[1:10000]
y1[ind1] <- -1*y1[ind1]

x_train <- data1[1:5000,]
y_train <- y1[1:5000]
x_test <- data1[5001:10000,]
y_test <- y1[5001:10000]


#mnist data


#without noise correct
#WAPA <- WAPA(x_test,y_test,0.18)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.18) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.18)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.04,0.9)
# RDA <- RDAl1(x_test,y_test,0.00032,125)
# adaRDA <- adaRDAl1(x_test,y_test,0.008,0.00032,0.2)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/mnist.csv",header = T,sep = ",")
x1 <- Data[,c(-1,-782)]
y1 <- Data[,782]

y1[which(y1==0)] <- -1
y1[which(y1==1)] <- -1
y1[which(y1==2)] <- -1
y1[which(y1==3)] <- -1
y1[which(y1==4)] <- -1
y1[which(y1==5)] <- -1
y1[which(y1==6)] <- -1
y1[which(y1==7)] <- -1
y1[which(y1==8)] <- -1
y1[which(y1==9)] <- 1
set.seed(12311)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:60000,]
y_test <- y[5001:60000]



#10% noise correct
#WAPA <- WAPA(x_test,y_test,0.1)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.1) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.1)
# AROW_h <- AROW_hinge(x_test,y_test,0.2)
# SCW <- SCWI(x_test,y_test,0.0016,0.55)
# RDA <- RDAl1(x_test,y_test,1,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.3)

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/mnist.csv",header = T,sep = ",")
x1 <- Data[,c(-1,-782)]
y1 <- Data[,782]

y1[which(y1==0)] <- -1
y1[which(y1==1)] <- -1
y1[which(y1==2)] <- -1
y1[which(y1==3)] <- -1
y1[which(y1==4)] <- -1
y1[which(y1==5)] <- -1
y1[which(y1==6)] <- -1
y1[which(y1==7)] <- -1
y1[which(y1==8)] <- -1
y1[which(y1==9)] <- 1
set.seed(12311)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(12312)
ind1 <- sample.int(60000, 6000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:60000,]
y_test <- y[5001:60000]






#20% noise correct
#WAPA <- WAPA(x_test,y_test,0.04)
# PAII <- PAII(x_test,y_test,0.00032)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.04) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.04)
# AROW_h <- AROW_hinge(x_test,y_test,0.0016)
# SCW <- SCWI(x_test,y_test,0.0016,0.95)
# RDA <- RDAl1(x_test,y_test,1,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.2)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/mnist.csv",header = T,sep = ",")
x1 <- Data[,c(-1,-782)]
y1 <- Data[,782]

y1[which(y1==0)] <- -1
y1[which(y1==1)] <- -1
y1[which(y1==2)] <- -1
y1[which(y1==3)] <- -1
y1[which(y1==4)] <- -1
y1[which(y1==5)] <- -1
y1[which(y1==6)] <- -1
y1[which(y1==7)] <- -1
y1[which(y1==8)] <- -1
y1[which(y1==9)] <- 1
set.seed(12311)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(12313)
ind1 <- sample.int(60000, 12000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:60000,]
y_test <- y[5001:60000]





#30% noise correct     done
# WAPA <- WAPA(x_test,y_test,0.02)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.02) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.02)
# AROW_h <- AROW_hinge(x_test,y_test,125)
# SCW <- SCWI(x_test,y_test,0.00032,0.95)
# RDA <- RDAl1(x_test,y_test,0.04,625)
# adaRDA <- adaRDAl1(x_test,y_test,5,0.00032,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/mnist.csv",header = T,sep = ",")
x1 <- Data[,c(-1,-782)]
y1 <- Data[,782]

y1[which(y1==0)] <- -1
y1[which(y1==1)] <- -1
y1[which(y1==2)] <- -1
y1[which(y1==3)] <- -1
y1[which(y1==4)] <- -1
y1[which(y1==5)] <- -1
y1[which(y1==6)] <- -1
y1[which(y1==7)] <- -1
y1[which(y1==8)] <- -1
y1[which(y1==9)] <- 1
set.seed(12311)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(12314)
ind1 <- sample.int(60000, 18000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:60000,]
y_test <- y[5001:60000]




## sequential dataset Eyestate         plot done 530*490
# works with my algorithms with noise
# very unsensitive to C
# does not work without noise
# with noise WAPA is better than AROW, SCW
# RDA 5+40*j  1:10


#without noise correct
#WAPA <- WAPA(x_test,y_test,0.51)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.51) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.51)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.008,0.95)
# RDA <- RDAl1(x_test,y_test,0.04,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.04,1,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/EyeState.csv",header = F,sep = ";")
x <- Data[,c(-15)]
y <- Data[,15]  
y[which(y==0)] <- -1

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:14980,]
y_test <- y[5001:14980]

# #noise 10 correct
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.21)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.21) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.21)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,5,0.7)
# RDA <- RDAl1(x_test,y_test,0.04,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/EyeState.csv",header = F,sep = ";")
x <- Data[,c(-15)]
y <- Data[,15]
y[which(y==0)] <- -1

set.seed(1111)
ind1 <- sample.int(14980,1498, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:14980,]
y_test <- y[5001:14980]

#noise 20 correct
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.11)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.11) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.11)
# AROW_h <- AROW_hinge(x_test,y_test,125)  changed
# SCW <- SCWI(x_test,y_test,0.2,0.55)  changed
# RDA <- RDAl1(x_test,y_test,0.008,25)
# adaRDA <- adaRDAl1(x_test,y_test,25,0.008,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/EyeState.csv",header = F,sep = ";")
x <- Data[,c(-15)]
y <- Data[,15]
y[which(y==0)] <- -1

set.seed(1112)
ind1 <- sample.int(14980,3000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:14980,]
y_test <- y[5001:14980]
#noise30 correct
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.06)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.06) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.06)
# AROW_h <- AROW_hinge(x_test,y_test,0.2)
# SCW <- SCWI(x_test,y_test,25,0.6)
# RDA <- RDAl1(x_test,y_test,0.008,625)
# adaRDA <- adaRDAl1(x_test,y_test,5,0.0016,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/EyeState.csv",header = F,sep = ";")
x <- Data[,c(-15)]
y <- Data[,15]
y[which(y==0)] <- -1

set.seed(1113)
ind1 <- sample.int(14980,4500, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:14980,]
y_test <- y[5001:14980]




summary(Data)

plot(x_test,type = "l")

cor(x, use="complete.obs", method="kendall") 

summary(x)
var(x)
boxplot(x,ylim=c(1,10000),ylab="Feature values",xlab="Features")
library(corrplot)
M <- cor(x)
corrplot(M, method = "circle")

# ind1 <- sample.int(13980, 3000, replace = FALSE, prob = NULL)
# y_test[ind1] <- -1*y_test[ind1]

##Nursery contaminated

#RDA lambda <- j/10


Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/nursery.csv",header = F,sep = ";")
x <- Data[,c(-9)]
y <- Data[,9]
#y[which(y==0)] <- -1
#x <- scale(x, center = TRUE, scale = TRUE)
ind <- sample.int(nrow(x), size = nrow(x), replace = FALSE, prob = NULL)
x <- x[ind,]
y<- y[ind]


ind1 <- sample.int(12960,1300, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]


x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:12960,]
y_test <- y[5001:12960]

##sequential dataset MovementAAL                                                      plot done 530*490
# without noise, does not work for APA and AROW, PA and WAPA works equaliy good,
# with noise, WAPA is the best  ##This should be the best example
#RDA lamda is important and different for each data
#RDA 0.01*j 1:100

# without noise correct
# WAPA <- WAPA(x_test,y_test,0.94)
# PAII <- PAII(x_test,y_test,5)
# APAII <- APAII(x_test,y_test,5)
# WAPAII <- WAPAII(x_test,y_test,5,0.99) #correct
# PAI <- PAI(x_test,y_test,5)
# APAI <- APAI(x_test,y_test,5)
# WAPAI <- WAPAI(x_test,y_test,5,0.99)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.00032,0.95)
# RDA <- RDAl1(x_test,y_test,0.0000128,0.00032)
# adaRDA <- adaRDAl1(x_test,y_test,0.000064,1,0.2)  changed
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/MovementAAL/MovementAAL_whole.csv",header = F,sep = ",")
x <- Data[,c(-5)]
y <- Data[,5]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:13197,]
y_test <- y[5001:13197]

#noise 10 correct
# WAPA <- WAPA(x_test,y_test,0.09)
# PAII <- PAII(x_test,y_test,0.2)
# APAII <- APAII(x_test,y_test,0.0016)
# WAPAII <- WAPAII(x_test,y_test,1,0.31) #correct
# PAI <- PAI(x_test,y_test,0.2)
# APAI <- APAI(x_test,y_test,0.04)
# WAPAI <- WAPAI(x_test,y_test,5,0.17)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.00032,0.9)
# RDA <- RDAl1(x_test,y_test,0.0000128,0.04) #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0000128,0.04,0.9)  #changed

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/MovementAAL/MovementAAL_whole.csv",header = F,sep = ",")
x <- Data[,c(-5)]
y <- Data[,5]

set.seed(111)
ind1 <- sample.int(13197, 1319, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:13197,]
y_test <- y[5001:13197]

#noise 20 correct

# WAPA <- WAPA(x_test,y_test,0.07) #changed
# PAII <- PAII(x_test,y_test,0.04)
# APAII <- APAII(x_test,y_test,1)  #changed
# WAPAII <- WAPAII(x_test,y_test,5,0.09)  #correct  changed
# PAI <- PAI(x_test,y_test,0.2)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,5,0.08)  #changed
# AROW_h <- AROW_hinge(x_test,y_test,0.04)
# SCW <- SCWI(x_test,y_test,0.00032,0.85)  #changed
# RDA <- RDAl1(x_test,y_test,0.0000128,0.2)
# adaRDA <- adaRDAl1(x_test,y_test,0.0000128,0.04,0.8) #changed
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/MovementAAL/MovementAAL_whole.csv",header = F,sep = ",")
x <- Data[,c(-5)]
y <- Data[,5]

set.seed(112)
ind1 <- sample.int(13197, 2640, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:13197,]
y_test <- y[5001:13197]

# noise 30 correct
# WAPA <- WAPA(x_test,y_test,0.04)
# PAII <- PAII(x_test,y_test,0.04)
# APAII <- APAII(x_test,y_test,25)
# WAPAII <- WAPAII(x_test,y_test,1,0.08)  #correct
# PAI <- PAI(x_test,y_test,0.2)
# APAI <- APAI(x_test,y_test,25)
# WAPAI <- WAPAI(x_test,y_test,25,0.06)
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,625,0.65)
# RDA <- RDAl1(x_test,y_test,0.000064,0.04)
# adaRDA <- adaRDAl1(x_test,y_test,0.000064,0.008,0.4)  #changed

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/MovementAAL/MovementAAL_whole.csv",header = F,sep = ",")
x <- Data[,c(-5)]
y <- Data[,5]

set.seed(113)
ind1 <- sample.int(13197, 3960, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:13197,]
y_test <- y[5001:13197]


##covtype
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/covtype.data",header = F,sep = ",")
x <- Data[,c(-55)]
y <- Data[,55]
y[which(y==2)] <- -1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- -1
y[which(y==6)] <- -1
y[which(y==7)] <- -1

ind1 <- sample.int(581012, 80000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]


x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:581012,]
y_test <- y[5001:581012]

##grammatical_facial_expression                                   plot 
# works with my algo
# does not work without noise
# WAPA is similar with AROW, but slightly better
# RDA lamda <- j   1:10

# without noise                      
# WAPA <- WAPA(x_test,y_test,0.51)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.51)   #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.51)
# AROW_h <- AROW_hinge(x_test,y_test,125)
# SCW <- SCWI(x_test,y_test,0.0625,0.95)   #changed  #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.008,625)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0000128,0.04,0.1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/facial.csv")
x <- Data[,c(-1,-303)]
y <- Data[,303]
y[which(y==0)] <- -1

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:19116,]
y_test <- y[5001:19116]

# noise 10
# WAPA <- WAPA(x_test,y_test,0.4)  #changed
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.4)  #correct changed
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.4)  #changed
# AROW_h <- AROW_hinge(x_test,y_test,125)  #changed
# SCW <- SCWI(x_test,y_test,1,0.8)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.00032,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.0016,0.1) 
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/facial.csv")
x <- Data[,c(-1,-303)]
y <- Data[,303]
y[which(y==0)] <- -1

set.seed(11111)
ind1 <- sample.int(19116,1911, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:19116,]
y_test <- y[5001:19116]
# noise 20
# WAPA <- WAPA(x_test,y_test,0.31)  #changed
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.31) #correct changed
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.31)  #changed
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,1,0.95)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.000064,1)   #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.1) 
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/facial.csv")
x <- Data[,c(-1,-303)]
y <- Data[,303]
y[which(y==0)] <- -1

set.seed(11112)
ind1 <- sample.int(19116,2822, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:19116,]
y_test <- y[5001:19116]

# noise 30
# WAPA <- WAPA(x_test,y_test,0.24) #changed
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.24) #correct changed
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.24)   #changed
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.008,0.95)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.04,125)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.1) #changed

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/facial.csv")
x <- Data[,c(-1,-303)]
y <- Data[,303]
y[which(y==0)] <- -1

set.seed(11113)
ind1 <- sample.int(19116,5733, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:19116,]
y_test <- y[5001:19116]


##gesture phase
#does not work
Data <- read.csv( "/home/wenjuan/Documents/DataSets/DataSets_Classification/gesture_phase_dataset/gesturephase_va3.csv")

x <- Data[,c(-1,-34)]
y <- Data[,34]

y[which(y==2)] <- 1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- 1


ind1 <- sample.int(9873,1000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:9873,]
y_test <- y[1001:9873]


##waveform
#does not work better than PA for both WAPA and AROW
# This one works great 
# have to try this one !!!!!!!!!! saturday  RDA 1:50 lambda <- 0.02*j

#without nose
# WAPA <- WAPA(x_test,y_test,0.09)
# PAII <- PAII(x_test,y_test,0.00032)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,0.04,0.08)
# PAI <- PAI(x_test,y_test,0.0016)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,0.008,0.09)
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.2,0.95)
# RDA <- RDAl1(x_test,y_test,0.0016,25)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.8)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/waveform_noise.data",header = F,sep = ",")
x1 <- Data[,c(-41)]
y1 <- Data[,41]
y1[which(y1==0)] <- -1
y1[which(y1==2)] <- -1
set.seed(123)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

x_train <- x[1:2000,]
y_train <- y[1:2000]
x_test <- x[2001:5000,]
y_test <- y[2001:5000]

#noise 10
# WAPA <- WAPA(x_test,y_test,0.03)
# PAII <- PAII(x_test,y_test,0.00032)
# APAII <- APAII(x_test,y_test,25)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.39)  #correct
# PAI <- PAI(x_test,y_test,0.0016)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,0.008,0.07)
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.04,0.95)
# RDA <- RDAl1(x_test,y_test,0.0016,25)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.2)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/waveform_noise.data",header = F,sep = ",")
x1 <- Data[,c(-41)]
y1 <- Data[,41]
y1[which(y1==0)] <- -1
y1[which(y1==2)] <- -1
set.seed(123)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(221)
ind1 <- sample.int(5000, 500, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:2000,]
y_train <- y[1:2000]
x_test <- x[2001:5000,]
y_test <- y[2001:5000]

#noise 20

# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,0.00032)
# APAII <- APAII(x_test,y_test,25)
# WAPAII <- WAPAII(x_test,y_test,0.008,0.02)  #correct
# PAI <- PAI(x_test,y_test,0.00032)
# APAI <- APAI(x_test,y_test,1)  
# WAPAI <- WAPAI(x_test,y_test,0.008,0.02)
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.04,0.95)
# RDA <- RDAl1(x_test,y_test,0.0016,125)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.4)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/waveform_noise.data",header = F,sep = ",")
x1 <- Data[,c(-41)]
y1 <- Data[,41]
y1[which(y1==0)] <- -1
y1[which(y1==2)] <- -1
set.seed(123)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(222)
ind1 <- sample.int(5000, 1000, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]


x_train <- x[1:2000,]
y_train <- y[1:2000]
x_test <- x[2001:5000,]
y_test <- y[2001:5000]

#noise 30

# WAPA <- WAPA(x_test,y_test,0.02)
# PAII <- PAII(x_test,y_test,0.00032)
# APAII <- APAII(x_test,y_test,5)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.19)  #correct
# PAI <- PAI(x_test,y_test,0.00032)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,0.008,0.02)
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.2,0.6)  #changed
# RDA <- RDAl1(x_test,y_test,0.0000128,25) #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0016,0.0016,0.8)

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/waveform_noise.data",header = F,sep = ",")
x1 <- Data[,c(-41)]
y1 <- Data[,41]
y1[which(y1==0)] <- -1
y1[which(y1==2)] <- -1
set.seed(123)
ind <- sample.int(nrow(x1), size = nrow(x1), replace = FALSE, prob = NULL)
x <- x1[ind,]
y<- y1[ind]

set.seed(223)
ind1 <- sample.int(5000, 1500, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]


x_train <- x[1:2000,]
y_train <- y[1:2000]
x_test <- x[2001:5000,]
y_test <- y[2001:5000]


## wine quality
#does not work
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/winequality_white.csv",header = F,sep = ",")
x <- Data[,c(-12)]
y <- Data[,12]
y[which(y==0)] <- -1

ind <- sample.int(nrow(x), size = nrow(x), replace = FALSE, prob = NULL)
x <- x[ind,]
y<- y[ind]

ind1 <- sample.int(4898, 500, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:4898,]
y_test <- y[1001:4898]


##damla
#does not work
x <- read.table("/home/wenjuan/Documents/DataSets/DataSets_Classification/aurba_damla/train1_1.fts",header = F,sep = ",")
y <- read.table("/home/wenjuan/Documents/DataSets/DataSets_Classification/aurba_damla/train1_1.lbls",header = F,sep = ",")
x <- as.matrix(x)
y <- as.vector(y)

y[which(y[,1]==0),] <- 1
y[which(y[,1]==2),] <- 1
y[which(y[,1]==3),] <- 1
y[which(y[,1]==4),] <- 1
y[which(y[,1]==5),] <- -1
y[which(y[,1]==6),] <- -1
y[which(y[,1]==7),] <- -1
y[which(y[,1]==8),] <- -1
y[which(y[,1]==9),] <- -1
y[which(y[,1]==10),] <- -1
y[which(y[,1]==11),] <- -1


x_train <- x[1:5000,]
y_train <- y[1:5000,]
x_test <- x[5001:215819,]
y_test <- y[5001:215819,]


##Cargo 2000 Freight Tracking and Tracing                                                   plot done 530*490
# works without noise even
# C is very unsensitive
# WAPA is similar with SCW and adaRDA, better than AROW and PA and APA and RDA with and without noise

# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,32768)
# SCW <- SCWI(x_test,y_test,0.00001,0.6)
# RDA <- RDAl1(x_test,y_test,8,1)
# adaRDA <- adaRDAl1(x_test,y_test,2,0.00045,0.6)

# #double crossvalidation for RDA
# res_RDA <- matrix(0,nrow = 50,ncol = 30)
# for (j in 1:50) {
#   lambda <- j/5
#   for (i in 1:30) {
#     gamma <- 2^(10-i)
#     res_RDA[j,i] <- RDAl1(x_train,y_train,lambda,gamma)$pred
#   }
# }


#without noise correct
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.01) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,125)
# SCW <- SCWI(x_test,y_test,1,0.65)  changed
# RDA <- RDAl1(x_test,y_test,0.04,625) changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0016,0.0016,1)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/c2k_data_comma.csv",header = T,sep = ",")
x <- Data[,c(-26)]
y <- Data[,26]
y[which(y==2)] <- 1
y[which(y==3)] <- -1

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:3942,]
y_test <- y[1001:3942]


#noise 10 correct
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.01) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,0.2)
# SCW <- SCWI(x_test,y_test,1,0.7)
# RDA <- RDAl1(x_test,y_test,0.04,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.008,0.0016,0.5)
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/c2k_data_comma.csv",header = T,sep = ",")
x <- Data[,c(-26)]
y <- Data[,26]
y[which(y==2)] <- 1
y[which(y==3)] <- -1

set.seed(330)
ind1 <- sample.int(3942,394, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:3942,]
y_test <- y[1001:3942]


#noise 20 correct
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.01)  #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,125)
# SCW <- SCWI(x_test,y_test,0.2,0.7)  changed
# RDA <- RDAl1(x_test,y_test,0.04,625)
# adaRDA <- adaRDAl1(x_test,y_test,25,0.0016,0.1) changed
Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/c2k_data_comma.csv",header = T,sep = ",")
x <- Data[,c(-26)]
y <- Data[,26]
y[which(y==2)] <- 1
y[which(y==3)] <- -1

set.seed(334)
ind1 <- sample.int(3942,800, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:3942,]
y_test <- y[1001:3942]


#noise 30 correct
# WAPA <- WAPA(x_test,y_test,0.01) 
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.01)  #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,0.2)
# SCW <- SCWI(x_test,y_test,0.008,0.65)
# RDA <- RDAl1(x_test,y_test,0.008,625)
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.1)


Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/c2k_data_comma.csv",header = T,sep = ",")
x <- Data[,c(-26)]
y <- Data[,26]
y[which(y==2)] <- 1
y[which(y==3)] <- -1

set.seed(332)
ind1 <- sample.int(3942,1200, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:1000,]
y_train <- y[1:1000]
x_test <- x[1001:3942,]
y_test <- y[1001:3942]


summary(x)
var(x)
boxplot(x,ylim=c(1,10000),ylab="Feature values",xlab="Features")
library(corrplot)
M <- cor(x)
corrplot(M, method = "circle")

##online retail
require(gdata)
Data <- read.xls("/home/wenjuan/Documents/DataSets/DataSets_Classification/Online Retail.xlsx",header = T,sep = ",")
x <- Data[,c(-3)]
y <- Data[,3]
summary(y)



###HT sensor
# Data1 <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/HTsensor/HT_Sensor_dataset.dat",header = T,sep = "")
# Data2 <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/HTsensor/HT_Sensor_metadata.dat",header = T,sep = "")
# 
# Data3 <- cbind(Data1,0)
# for (i in 0:68) {
#   Data3[which(Data1$id==i),13] <- ifelse(Data2[which(Data2$id==i),3]=="banana",1,-1)
# }
# 
# Data3 <- Data3[-which(Data3[,13]==0),]
# 
# write.csv(Data3,file = "/home/wenjuan/Documents/DataSets/DataSets_Classification/HTsensor/HTsensor.csv")
##not sensitive to everything, do not understand
#maybe better with noise?

#without noise
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.01) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,1)
# SCW <- SCWI(x_test,y_test,1,0.5)
# RDA <- RDAl1(x_test,y_test,0.1,25)   # lamda <- j/10
# adaRDA <- adaRDAl1(x_test,y_test,0.01,0.04,0.1)

#noise 10
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.2)
# WAPAII <- WAPAII(x_test,y_test,1,0.01) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,0.00032)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,1)
# SCW <- SCWI(x_test,y_test,0.008,0.5)
# RDA <- RDAl1(x_test,y_test,0.6,625)  
# adaRDA <- adaRDAl1(x_test,y_test,1,0.008,0.1)

#noise 20
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.2)
# WAPAII <- WAPAII(x_test,y_test,1,0.01) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,0.00032)
# WAPAI <- WAPAI(x_test,y_test,1,0.01)
# AROW_h <- AROW_hinge(x_test,y_test,1)
# SCW <- SCWI(x_test,y_test,0.008,0.5)
# RDA <- RDAl1(x_test,y_test,0.6,625)   # lamda <- j/10
# adaRDA <- adaRDAl1(x_test,y_test,1,0.008,0.1)

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/HTsensor/HTsensor.csv")
x <- Data[,-c(1,2,3,14)]
y <- Data[,14]

ind1 <- sample.int(652024,65202, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:10000,]
y_train <- y[1:10000]
x_test <- x[10001:652024,]
y_test <- y[10001:652024]


##Epileptic Seizure Recognition Data Set                    plot 
##works without noise 
#works with noise
# WAPA = SCW = APA gives little better results comparing to AROW = PA 
#WAPA better than PAm
# RDA lambda <- j/200 1:10

#without noise
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.00032)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.01) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01) 
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,0.00032,0.65)
# RDA <- RDAl1(x_test,y_test,1,625)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.2,0.00032,0.2) 

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/Epileptic Seizure Recognition.csv")
x <- Data[,-c(1,180)]
y <- Data[,180]
y[which(y==2)] <- 1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- -1

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:11500,]
y_test <- y[5001:11500]

#noise 10
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.00032)
# WAPAII <- WAPAII(x_test,y_test,0.0016,0.01) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01) 
# AROW_h <- AROW_hinge(x_test,y_test,625)
# SCW <- SCWI(x_test,y_test,5,0.5)    #changed
# RDA <- RDAl1(x_test,y_test,0.0000128,125)
# adaRDA <- adaRDAl1(x_test,y_test,0.00032,0.00032,0.1) 

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/Epileptic Seizure Recognition.csv")
x <- Data[,-c(1,180)]
y <- Data[,180]
y[which(y==2)] <- 1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- -1

set.seed(441)
ind1 <- sample.int(11500,1150, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:11500,]
y_test <- y[5001:11500]


#noise 20
# WAPA <- WAPA(x_test,y_test,0.01)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.0016)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.01) #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.01) 
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.008,0.95)
# RDA <- RDAl1(x_test,y_test,0.04,625)  #changed
#  adaRDA <- adaRDAl1(x_test,y_test,5,0.00032,0.1) 

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/Epileptic Seizure Recognition.csv")
x <- Data[,-c(1,180)]
y <- Data[,180]
y[which(y==2)] <- 1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- -1

set.seed(442)
ind1 <- sample.int(11500,2300, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:11500,]
y_test <- y[5001:11500]

#noise 30
# WAPA <- WAPA(x_test,y_test,0.9)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.9)  #correct
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.9) 
# AROW_h <- AROW_hinge(x_test,y_test,0.2)
# SCW <- SCWI(x_test,y_test,1,0.95)
# RDA <- RDAl1(x_test,y_test,0.2,125)
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.0016,0.6) 

Data <- read.csv("/home/wenjuan/Documents/DataSets/DataSets_Classification/Epileptic Seizure Recognition.csv")
x <- Data[,-c(1,180)]
y <- Data[,180]
y[which(y==2)] <- 1
y[which(y==3)] <- -1
y[which(y==4)] <- -1
y[which(y==5)] <- -1

set.seed(450)
ind1 <- sample.int(11500,3450, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:11500,]
y_test <- y[5001:11500]




##Occupancy Detection Data Set
Data1 <- read.table("/home/wenjuan/Documents/DataSets/DataSets_Classification/occupancy_data/datatest.txt",header = T, sep = ",")
Data2 <- read.table("/home/wenjuan/Documents/DataSets/DataSets_Classification/occupancy_data/datatraining.txt",header = T, sep = ",")
Data3<- read.table("/home/wenjuan/Documents/DataSets/DataSets_Classification/occupancy_data/datatest2.txt",header = T, sep = ",")



#without noise
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.84)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.84) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.84)   #changed
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,25,0.8)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.04,125)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.00032,0.2,0.1)


Data <- rbind(Data1,Data2,Data3)
x <- Data[,-c(1,2,8)]
y <- Data[,8]
y[which(y==0)] <- -1

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:20560,]
y_test <- y[5001:20560]


#10 % noise
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.15)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,0.00032)
# WAPAII <- WAPAII(x_test,y_test,1,0.12) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.15)   #changed
# AROW_h <- AROW_hinge(x_test,y_test,1)
# SCW <- SCWI(x_test,y_test,0.008,0.5)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.000064,625)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.04,0.008,0.3)


Data <- rbind(Data1,Data2,Data3)
x <- Data[,-c(1,2,8)]
y <- Data[,8]
y[which(y==0)] <- -1

set.seed(45011)
ind1 <- sample.int(20560,2056, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:20560,]
y_test <- y[5001:20560]


#20 % noise
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.09)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,1,0.09) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.09)   #changed
# AROW_h <- AROW_hinge(x_test,y_test,25)
# SCW <- SCWI(x_test,y_test,0.0016,0.55)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.008,625)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0000128,0.008,0.4)


Data <- rbind(Data1,Data2,Data3)
x <- Data[,-c(1,2,8)]
y <- Data[,8]
y[which(y==0)] <- -1

set.seed(45012)
ind1 <- sample.int(20560,4112, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:20560,]
y_test <- y[5001:20560]


#30 % noise
# PA <- PA(x_test,y_test)
# APA <- APA(x_test,y_test)
# WAPA <- WAPA(x_test,y_test,0.04)
# PAII <- PAII(x_test,y_test,1)
# APAII <- APAII(x_test,y_test,1)
# WAPAII <- WAPAII(x_test,y_test,0.00032,0.04) 
# PAI <- PAI(x_test,y_test,1)
# APAI <- APAI(x_test,y_test,1)
# WAPAI <- WAPAI(x_test,y_test,1,0.04)   #changed
# AROW_h <- AROW_hinge(x_test,y_test,125)
# SCW <- SCWI(x_test,y_test,0.008,0.5)    #does not work with big c
# RDA <- RDAl1(x_test,y_test,0.0000128,625)  #changed
# adaRDA <- adaRDAl1(x_test,y_test,0.0000128,0.008,0.5)


Data <- rbind(Data1,Data2,Data3)
x <- Data[,-c(1,2,8)]
y <- Data[,8]
y[which(y==0)] <- -1

set.seed(45013)
ind1 <- sample.int(20560,6168, replace = FALSE, prob = NULL)
y[ind1] <- -1*y[ind1]

x_train <- x[1:5000,]
y_train <- y[1:5000]
x_test <- x[5001:20560,]
y_test <- y[5001:20560]


