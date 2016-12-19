# Test to make sure quantities are correct

theta_1 <- seq(from=-4,to=4,length.out=200)
theta_2 <- seq(from=-4,to=4,length.out=200)
thetas <- matrix(data=c(theta_1,theta_2),ncol=2)

slopes <- income_slope
diff <- income_coefs
out_probs <- matrix(ncol=length(diff),nrow=200)
for(d in 1:length(diff)) {
  
  out_probs[,d] <- plogis(thetas %*% slopes - diff[d])
  
}
out_probs_calc <- t(apply(out_probs,1,function(x) {
  x <- abs(x - c(0,x[1:(length(x)-2)],1))
  x <- c(1-sum(x),x)
}))