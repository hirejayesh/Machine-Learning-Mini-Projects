#calculate euclidean distance
euclideanDistMetric <- function(row1, row2){
	distance = 0
	for(i in c(1:length(row1))){
		distance = distance + (as.numeric(row1[i])-as.numeric(row2[i]))^2
	}
	
	distance = sqrt(distance)
	return(distance)
}
knnAlgorithm <- function(train.X, train.Y, test.X, K){
	predict <- c()
	for(i in c(1:nrow(test.X))){
		distArray = c()
		for(j in c(1:nrow(train.X))){
			distArray <- c(distArray, euclideanDistMetric(test.X[i,], train.X[j,]))
		}
		#prediction for K=1
		if(K == 1){
			minimumDistance = min(distArray)
			indexOfMinimum = which.min(distArray)
			predict <- c(predict, train.Y[indexOfMinimum])
		}
		#prediction for K=3
		else{
			#3 nearest objects' label are found here
			nearestLabels <- c()
			# 1st minimum
			minimumDistance = min(distArray)
			indexOfMinimum = which.min(distArray)
			nearestLabels <- c(nearestLabels, train.Y[indexOfMinimum])
			
			# 2nd minimum
			distArray = distArray[-indexOfMinimum]
			indexOfMinimum = which.min(distArray)
			nearestLabels <- c(nearestLabels, train.Y[indexOfMinimum])
			
			#3rd minimum
			distArray = distArray[-indexOfMinimum]
			indexOfMinimum = which.min(distArray)
			nearestLabels <- c(nearestLabels, train.Y[indexOfMinimum])
			
			count1 = 0 #for label +1
			count2 = 0 #for label -1
			for(m in c(1:3)){
				if (nearestLabels[m] == 1){
					count1 = count1 + 1
				}
				else{
					count2 = count2 + 1 
				}
			}	
			if (count1 > count2){
				predict <- c(predict, +1)
			}
			else{
				predict <- c(predict, -1)
			}
		}
	}
	return(predict)	
				
}
accuracyPercentage <- function(test.Y, predict){
	accurate = 0
	for (i in c(1:length(test.Y))){
		
		if (test.Y[i] == predict[i]){
			accurate = accurate + 1
		}
	}	
	print(accurate)
	percentage = accurate/length(test.Y) * 100
	return(percentage) 
}

iris <- read.table("iris.txt")
irisTrain.X <- iris[1:70,1:4]
irisTrain.Y <- iris[1:70,5]
irisTest.X <- iris[71:100,1:4]
irisTest.Y <- iris[71:100,5]


K = 1 
iris.predictK1 <- knnAlgorithm(irisTrain.X, irisTrain.Y, irisTest.X, K)
irisAccuracyK1 <- accuracyPercentage(irisTest.Y, iris.predictK1)
print(irisAccuracyK1)
 

K = 3
iris.predictK3 <- knnAlgorithm(irisTrain.X, irisTrain.Y, irisTest.X, K)
irisAccuracyK3 <- accuracyPercentage(irisTest.Y, iris.predictK3)
print(irisAccuracyK3)

ionosphere <- read.table("ionosphere.txt", sep = ",")
ionosphereTrain.X <- ionosphere[1:200, 1:34]
ionosphereTrain.Y <- ionosphere[1:200,35]
ionosphereTest.X <- ionosphere[201:351, 1:34]
ionosphereTest.Y <- ionosphere[201:351, 35]

K = 1
ionosphere.predictK1 <- knnAlgorithm(ionosphereTrain.X, ionosphereTrain.Y, ionosphereTest.X, K)
ionosphereAccuracyK1 <- accuracyPercentage(ionosphereTest.Y, ionosphere.predictK1)
print(ionosphereAccuracyK1)

K = 3
ionosphere.predictK3 <- knnAlgorithm(ionosphereTrain.X, ionosphereTrain.Y, ionosphereTest.X, K)
ionosphereAccuracyK3 <- accuracyPercentage(ionosphereTest.Y, ionosphere.predictK3)
print(ionosphereAccuracyK3)




