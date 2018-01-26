iris <- read.table("iris.txt", sep =",")
train_index <- sample(1:nrow(iris), 70)
irisTrain <- iris[train_index,]
irisTest <- iris[-train_index,]

park <-  read.table("parkinsons.data", header = T, sep=",")
train_index <- sample(1:nrow(park), 120)
parkTrain <- park[train_index,]
parkTest <- park[-train_index,]


best_attr_iris <- function(iris){
	for(i in c(1:4)){
      	max <- 0
		mean.0 <- mean(as.numeric(iris[(iris[,5]==-1),i]))
		mean.1 <- mean(as.numeric(iris[(iris[,5]==1),i]))
		st.dev <- sd(as.numeric(iris[,i]))
		a <- abs(mean.1-mean.0)/st.dev
		if (a> max){
			max <- a
			best1 <- i
		}
	}
	for(i in c(1:4)){
      	if(i==best1)
			next
		else{
			max <- 0
			mean.0 <- mean(as.numeric(iris[(iris[,5]==-1),i]))
			mean.1 <- mean(as.numeric(iris[(iris[,5]==1),i]))
			st.dev <- sd(as.numeric(iris[,i]))
			a <- abs(mean.1-mean.0)/st.dev
			if (a> max){
				max <- a
				best2 <- i
			}
	}
	mylist <-c(best1,best2)
	return(mylist)
}
best_attr_park <- function(park){
	for(i in c(2:24)){
      	if(i == 18){
			next	
		}else{
			max <- 0
			mean.0 <- mean(as.numeric(park[(park[,18]==0),i]))
			mean.1 <- mean(as.numeric(park[(park[,18]==1),i]))
			st.dev <- sd(as.numeric(park[,i]))
			a <- abs(mean.1-mean.0)/st.dev
			if (a> max){
				max <- a
				best1 <- i
			}
		}
	}
	for(i in c(2:24)){
      	if(i == 18 || i == best1){
			next	
		}else{
			max <- 0
			mean.0 <- mean(as.numeric(park[(park[,18]==0),i]))
			mean.1 <- mean(as.numeric(park[(park[,18]==1),i]))
			st.dev <- sd(as.numeric(park[,i]))
			a <- abs(mean.1-mean.0)/st.dev
			if (a> max){
				max = a
				best2 = i
			}
		}
	}

	mylist <-c(best1,best2)
	return(mylist)
}

#two best attributes
bestIris <- best_attr_iris(iris)
best1_iris <- bestIris[1]
best2_iris <- bestIris[2]
bestPark <- best_attr_park(park)
best1_park <- bestPark[1]
best2_park <- bestPark[2]

irisTrain.X <- irisTrain[,best1_iris]
irisTrain.Y <- irisTrain[,5]
irisTest.X <- irisTest[,best1_iris]
irisTest.Y <- irisTrain[,5]


parkTrain.X <- parkTrain[,best1_park]
parkTrain.Y <- parkTrain[,18]
parkTest.X <- parkTest[,best1_park]
parkTest.Y <- parkTest[,18]

irisPredict <- lda(irisTrain.X,irisTrain.Y,irisTest.X, data="iris", p=1)
accuracy_iris_lda_1 <- accuracyPercentage(irisTest.Y, irisPredict)
parkPredict <- lda(parkTrain.X,parkTrain.Y,parkTest.X, data ="park", p=1)
accuracy_park_lda_1 <- accuracyPercentage(parkTest.Y, parkPredict)


irisPredict <- qda(irisTrain.X,irisTrain.Y,irisTest.X, data="iris", p=1)
accuracy_iris_qda_1 <- accuracyPercentage(irisTest.Y, irisPredict)
parkPredict <- qda(parkTrain.X,parkTrain.Y,parkTest.X, data ="park", p=1)
accuracy_park_qda_1 <- accuracyPercentage(parkTest.Y, parkPredict)

irisTrain.X <- irisTrain[,c(best1_iris,best2_iris)]
irisTrain.Y <- irisTrain[,5]
irisTest.X <- irisTest[,c(best1_iris,best2_iris)]

parkTrain.X <- parkTrain[,c(best1_park,best2_park)]
parkTrain.Y <- parkTrain[,18]
parkTest.X <- parkTest[,c(best1_park,best2_park)]

irisTest.Y <- lda(irisTrain.X,irisTrain.Y,irisTest.X, data="iris", p=2)
accuracy_iris_lda_2 <- accuracyPercentage(irisTest.Y, irisPredict)
parkTest.Y <- lda(parkTrain.X,parkTrain.Y,parkTest.X, data ="park", p=2)
accuracy_park_lda_2 <- accuracyPercentage(parkTest.Y, parkPredict)

irisTest.Y <- qda(irisTrain.X,irisTrain.Y,irisTest.X, data="iris", p=2)
accuracy_iris_qda_2 <- accuracyPercentage(irisTest.Y, irisPredict)
parkTest.Y <- qda(parkTrain.X,parkTrain.Y,parkTest.X, data ="park", p=2)
accuracy_park_qda_2 <- accuracyPercentage(parkTest.Y, parkPredict)




lda <- function(Train.X,Train.Y,Test.X, data, p){
	total <- length(Train.Y)
	if (data=="iris")
		num_0 <- length(Train.Y[Train.Y == -1])
      else
		num_0 <- length(Train.Y[Train.Y == 0])
	num_1 <- length(Train.Y[Train.Y == 1])	
	
	prob_0 <- num_0/total
	prob_1 <- num_1/total
	
	#count_X_0  contains Training attribute data where label is -1 ,similarly count_X_1
	if(data=="iris")
		count_X_0 <- as.numeric(Train.X[Train.Y == -1])
	else
		count_X_0 <- as.numeric(Train.X[Train.Y == 0])
	count_X_1 <- as.numeric(Train.X[Train.Y == 1])
	
	if(p == 1){
		mean_0 <- sum(count_X_0)/length(count_X_0)
      	mean_1 <- sum(count_X_1)/length(count_X_1)
		
		exp0 <- sum((count_X_0 - mean_0)^2)
		exp1 <- sum((count_X_1 - mean_1)^2)
		exp_sum <- exp0 + exp1
      	var <- exp_sum/(total - 2)
		
		Test.Y <- c()
      		
		for(i in c(1:length(Test.X))){
			q <- as.numeric(Test.X[i])
			
			#calculating each class discriminants
			disc_0 <- ((mean_0/var)*q)-((mean_0^2)/(2*var))+ log(prob_0)
			disc_1 <- ((mean_1/var)*q)-((mean_1^2)/(2*var))+ log(prob_1)
           	
			if(disc_0 > disc_1){
				if(data=="iris")
					Test.Y <- c(Test.Y, -1)			
                  	else
                  		Test.Y <- c(Test.Y, 0)
			}else{
				Test.Y <- c(Test.Y, 1)
			}	
		}
		return(Test.Y)
	}
	if(p==2){
		mean_0 <- sum(count_X_0[,1])/nrow(count_X_0)
      	mean <-  sum(count_X_0[,2])/nrow(count_X_0)
      	mean_0 <- data.frame(mean_0,mean)
      
		mean_1 <- sum(count_X_1[,1])/nrow(count_X_1)
		mean <-  sum(count_X_0[,2])/nrow(count_X_0)
      	mean_1 <- data.frame(mean_1,mean)

		covMat <- cov(Train.X[,1],Train.X[,2])
 
		Test.Y <- c()
      	for(i in c(1:nrow(Test.X))){
			q <- as.numeric(Test.X[i,])
			
			disc_0 <- t(q)%*%(solve(covMat))%*%mean_0 - (t(mean_0)%*%solve(covMat)%*%t(mean_0))/2 +log(prob_0) 
			disc_1 <- t(q)%*%(solve(covMat))%*%mean_1 - (t(mean_1)%*%solve(covMat)%*%t(mean_1))/2 +log(prob_1) 

           		if(disc_0> disc_1){
				if(data=="iris")
					Test.Y <- c(Test.Y, -1)			
                  	else
                  		Test.Y <- c(Test.Y, 0)
			}else{
				Test.Y <- c(Test.Y, 1)
			}	
		}	
		return(Test.Y)
	}
}

qda <- function(Train.X,Train.Y,Test.X){
	total <- length(Train.Y)
	if (data=="iris")
		num_0 <- length(Train.Y[Train.Y == -1])
      else
		num_0 <- length(Train.Y[Train.Y == 0])
	num_1 <- length(Train.Y[Train.Y == 1])	
	
	prob_0 <- num_0/total
	prob_1 <- num_1/total
	
	if(data=="iris")
		count_X_0 <- as.numeric(Train.X[Train.Y == -1])
	else
		count_X_0 <- as.numeric(Train.X[Train.Y == 0])
	count_X_1 <- as.numeric(Train.X[Train.Y == 1])

	
	if(p==1){
		mean_0 <- sum(count_X_0)/length(count_X_0)
      	mean_1 <- sum(count_X_1)/length(count_X_1)
      
		var_0 <- sum((count_X_0 - mean_0)^2)/(num_0-1)
		var_1 <- sum((count_X_1 - mean_1)^2)/(num_1-1)
      
      	Test.Y <- c()
      
		for(i in c(1:length(Test.X))){
			q <- as.numeric(Test.X[i])
			disc_0 <- -(1/2)*(log(var_0)) - (1/2)*((q-mean_0)^2)/var_0 + log(prob_0)
            	disc_1 <- -(1/2)*(log(var_1)) - (1/2)*((q-mean_1)^2)/var_1 + log(prob_1)
            
			if(disc_0> disc_1){
				if(data=="iris")
					Test.Y <- c(Test.Y, -1)			
                  	else
                  		Test.Y <- c(Test.Y, 0)
			}else{
				Test.Y <- c(Test.Y, 1)
			}	
		}
		return(Test.Y)
	}
	if(p==2){
		mean_0 <- sum(count_X_0[,1])/nrow(count_X_0)
      	mean <-  sum(count_X_0[,2])/nrow(count_X_0)
      	mean_0 <- data.frame(mean_0,mean)
      
		mean_1 <- sum(count_X_1[,1])/nrow(count_X_1)
		mean <-  sum(count_X_0[,2])/nrow(count_X_0)
      	mean_1 <- data.frame(mean_1,mean)

		cov_0 <- cov(count_X_0[,1],count_X_0[,2])
		cov_1 <- cov(count_X_1[,1],count_X_1[,2])

		Test.Y <- c()
      	for(i in c(1:nrow(Test.X))){
			q <- as.numeric(Test.X[i,])
		
			disc_0 <- -(1/2)*(log(det(cov_0))) - (1/2)*(t(q))%*%(solve(cov_0))%*%(q) 
                     	   + t(q)%*%(solve(cov_0))%*%mean_0 - (1/2)*t(mean_0)%*%(solve(cov_0))%*%mean_0 + log(prob_0)        
            
			disc_1 <- -(1/2)*(log(det(cov_1))) - (1/2)*(t(q))%*%(solve(cov_1))%*%(q) 
                     	   + t(q)%*%(solve(cov_1))%*%mean_1 - (1/2)*t(mean_1)%*%(solve(cov_1))%*%mean_1 + log(prob_1)
            
            	if(disc_0> disc_1){
				if(data=="iris")
					Test.Y <- c(Test.Y, -1)			
                  	else
                  		Test.Y <- c(Test.Y, 0)
			}else{
				Test.Y <- c(Test.Y, 1)
			}	
 		}
		return(Test.Y)
	}
}
}
accuracyPercentage <- function(test.Y, predict){
	accurate <- 0
	for (i in c(1:length(test.Y))){
		
		if (test.Y[i] == predict[i]){
			accurate <- accurate + 1
		}
	}	
	percentage <- accurate/length(test.Y) * 100
	return(percentage) 
}





