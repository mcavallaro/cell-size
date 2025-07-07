
plot_sub<-function(xx, yy, K = 10000, ...){
  rows<-sample(1:length(xx), K)
  plot(xx[rows], yy[rows], ...)
}

points_sub<-function(xx, yy, K = 10000, ...){
  rows <- sample(1:length(xx), K)
  points(xx[rows], yy[rows], ...)
}


# define function to transform data
findTransformCoeff<-function(feature.las1, feature.las2, na.rm=TRUE){
  las1 = c(mean(feature.las1, na.rm=na.rm), sd(feature.las1, na.rm=na.rm))
  las2 = c(mean(feature.las2, na.rm=na.rm), sd(feature.las2, na.rm=na.rm))
  las1<-as.matrix(las1, ncols=1)
  las2<-cbind( c(1, 0), as.matrix(las2, ncols=1))
  solve(las2, las1)
}
findTransformCoeff.boot<-function(feature.las1, feature.las2, n_boot=10, na.rm=TRUE){
  transf<-function(feature.las1, feature.las2, na.rm){
    boot1 = sample(feature.las1, size=length(feature.las1), replace=T)
    boot2 = sample(feature.las2, size=length(feature.las2), replace=T)
    findTransformCoeff(boot1, boot2, na.rm)
  }
  samples = replicate(n_boot, transf(feature.las1, feature.las2, na.rm), simplify = T)
  res = matrix(c(mean(samples[1,]), mean(samples[2,]), sd(samples[1,]), sd(samples[2,])), nrow=2)
  colnames(res) = c("mean (best estim.)", "SE of the mean")
  rownames(res) = c("alpha_i0", "alpha_i1")
  return(res)
}
transform<-function(data, coef){
  alpha_00 = coef[1,1]
  alpha_i0 = coef[2,1]
  return(alpha_i0 * data  + alpha_00)
}

rmse <- function(x,y){
  # root mean squared error
  sqrt( sum((x - y)^2) / length(x))
}

F.test<-function(prediction.1, prediction.2, true.values){
  # test if `prediction.2` is significantly better than `prediction.1` (F-score > 1)
  RSS1 = sum((true.values - prediction.1)^2)
  RSS2 = sum((true.values - prediction.2)^2)
  # RSS is "Residual sum of squares" (a.k.a. sum of squared estimate of errors, SSE)
  n = length(true.values)
  F. = RSS1 / RSS2
  p.value <- pf(F., n-1, n-1, lower.tail = FALSE)
  data.frame(F = F., p.value=p.value)
}


# F.test<-function(prediction.1, prediction.2, true.values){
#   #this second function is consistent with var.test:
#   RSS1 = sum((true.values - prediction.1 -  mean(true.values - prediction.1))^2)
#   n = length(true.values)
#   RSS2 = sum((true.values - prediction.2 - mean(true.values - prediction.2))^2)
#   F. = RSS1 / RSS2
#   p.value <- pf(F., n-1, n-1, lower.tail = FALSE)
#   data.frame(F = F., p.value=p.value)
# }
