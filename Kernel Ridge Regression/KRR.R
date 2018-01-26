kernel <- function(x1,x2,d,type){
	k <- 0
	if(type=='radial'){
		k <- exp(-(1/2*d)* sum((x1-x2)^2))
	}else{
		k <- (1+sum(x1*x2))^d
	}
	return(k)
} 

KRR_radial <- function(train.X, train.Y,test.X, d, lambda){
	
	#prediction by radial kernel
	
	predict_radial <- c()
	K_radial <- matrix(nrow=nrow(train.X), ncol=nrow(train.X),byrow=TRUE)
	for(i in 1:nrow(train.X)){
		for(j in 1:nrow(train.X)){
			K_radial[i,j] <- kernel(train.X[i,],train.X[j,],d,'radial')
		}
	}
	I <- diag(nrow(train.X))
	for(i in 1:nrow(test.X)){
		k_radial <- matrix(nrow=nrow(train.X), ncol=1)
		for(m in 1:nrow(train.X)){
			k_radial[m,1] <- kernel(train.X[m,],test.X[i,],d,'radial')
		}
		predict_radial[i] <- t(train.Y) %*% solve(lambda*I + K_radial) %*% k_radial
	}
	return(predict_radial)
}    

KRR_poly <- function(train.X, train.Y,test.X, d, lambda){
	
	#prediction by polynomial kernel
	
	predict_poly <- c()
	K_poly <- matrix(nrow=nrow(train.X), ncol=nrow(train.X),byrow=TRUE)
	for(i in 1:nrow(train.X)){
		for(j in 1:nrow(train.X)){
			K_poly[i,j] <- kernel(train.X[i,],train.X[j,],d,'polynomial')
		}
	}
	I <- diag(nrow(train.X))
	for(i in 1:nrow(test.X)){
		k_poly <- matrix(nrow=nrow(train.X), ncol=1)
		for(m in 1:nrow(train.X)){
			k_poly[m,1] <- kernel(train.X[m,],test.X[i,],d,'polynomial')
		}
		predict_poly[i] <- t(train.Y) %*% solve(lambda*I + K_poly) %*% k_poly
	}
	return(predict_poly)
}

cal_radial_test_mse <- function(train.X, train.Y, test.X, test.Y, d, lambda){
	predict <- KRR_radial(train.X, train.Y,test.X, d, lambda)
	diff_square <- 0
	for(i in 1:nrow(test.X)){
		diff_square <- diff_square + (test.Y[i]- predict[i])^2
	}
	radial_test_mse <- diff_square/nrow(test.X)
	return(radial_test_mse)
}

cal_poly_test_mse <- function(train.X, train.Y, test.X, test.Y, d, lambda){
	predict <- KRR_poly(train.X, train.Y,test.X, d, lambda)
	diff_square <- 0
	for(i in 1:nrow(test.X)){
		diff_square <- diff_square + (test.Y[i]- predict[i])^2
	}
	poly_test_mse <- diff_square/nrow(test.X)
	return(poly_test_mse)
}


#storing data and splitting
data <- read.table("Advertising.csv", header=T, sep=",")
train_index <- sample(1:nrow(data),nrow(data)*(2/3))
train<- data[train_index,]
test<- data[-train_index,]
train.X <- train[,2:4]
train.Y <- train[,5]
test.X <- test[,2:4]
test.Y <- test[,5]


#test mse(radial and polynomial) for different parameters
mse1_rad <- cal_radial_test_mse(train.X, train.Y, test.X, test.Y, 2, 7)
mse1_poly <- cal_poly_test_mse(train.X, train.Y, test.X, test.Y, 2, 7)
print(mse1_rad)
print(mse1_poly)		