gradientDescent <- function(x, alpha, num_iters){
  y_history <- as.data.frame(matrix(ncol=2, nrow=num_iters))
  for (iter in 1:num_iters){
    y <- x^2 +5 * x;
    y_history[iter,] <- c(x, y);
    slope = 2*x+5;
    x = x - alpha * slope;
  }
  y_history
}

res <- gradientDescent(2,0.1,20);
cat("Minimum point in y = ");min(res$V2)
cat("Value of x corresponding to minimum point in y = ");res[which.min(res$V2),]$V1;

x<- seq(-7,2,0.01)
y<-x^2 +5*x
plot(x,y,type="l",col="blue",lwd=2,cex.lab=1.5)
points(res$V1,res$V2,type="p",col="red",lwd=2,cex.lab=1.5)


attach(mtcars)

Y <- mpg
bias <- rep(1, times=length(Y))
X <- as.matrix(cbind(bias, wt, qsec))


# Defining error cost and gradients function

# For calculating error, pass input matrix,
# beta coefficients, output y
err <- function(x,b,y) {
  return((x %*% b) - y )
  }
# For calculating cost, pass error
cost <- function(e) {
  return((1/length(e))*crossprod(e))
  }
# Calculate Gradient matrix, pass input matrix and error
gradients <- function(x,e){
  x <- as.matrix(x)
  return((2/nrow(x))*(crossprod(x,e)))
  }

regression_gradient_descent <- function(X,
                                        Y,
                                        converge_threshold,
                                        alpha,
                                        num_iters,
                                        print_every){
  beta <- matrix(0, ncol = 1, nrow = ncol(X))
  threshold <- converge_threshold
  cost_vector <- c()
  alpha <- alpha
  error <- err(X, beta, Y)
  cost_prev <- cost(error)
  cost_vector <- c(cost_vector, cost_prev)
  beta_prev <- beta
  cost_new <- cost_prev + 1
  cost_check <- abs(cost_new - cost_prev)
  i <- as.integer(0)
  num_iters <- num_iters
  print_every <- ifelse(print_every > 0, print_every, num_iters)
  while( cost_check > threshold && i <= num_iters) {
    beta_new <- beta_prev - alpha * gradients(X, error)
    error_new <- err(X, beta_new, Y)
    cost_new <- cost(error_new)
    cost_vector <- c(cost_vector,cost_new)
    beta_prev <- beta_new
    error <- error_new
    cost_check <- abs(cost_new-cost_prev)
    cost_prev <- cost_new
    i <- as.integer(i+1);
    if( i %% print_every == 0){
      print(c(i, cost_new, cost_check))
      }
  }
  list("Beta_Estmates" = beta_new,
       "Cost"= cost_new,
       "Iterations" = i,
       "Cost_History" = cost_vector)
}

grad_reg_result <- regression_gradient_descent(X,
                                               Y,
                                               converge_threshold = 0,
                                               alpha = 0.003,
                                               num_iters = 300000,
                                               print_every = 50000)
j=seq(10000,grad_reg_result$Iterations,10000)
plot(j,grad_reg_result$Cost_History[j],type="b",col="blue",lwd=2,cex.lab=1.5,ylab = "Cost",xlab="iterations")

cat("Beta estimates using gradient Descent");print(grad_reg_result$Beta_Estmates)
reg<-lm(mpg ~wt+qsec)
print("Beta coffeicients from lm function");reg$coefficients
