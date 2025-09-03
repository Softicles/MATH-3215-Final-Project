# Number of trials
N = 10^5

vector_of_one = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
x <- 1:20
x_squared = x * x
log_x = log(x)

# Store the sum of alpha_hats for each trial
sum_a = vector("double", N)

for (i in seq(1:N)) {
  Y = vector("double", 20)
  
  # Generate Y
  for (j in seq(1:20)) {
    Y[j] = 2 + 1 * j + 1 * j^2 + 2*log(j) + rnorm(1, 0, 2)
  }
  
  # Generate the matrix A
  A = cbind(vector_of_one, x, x_squared, log_x)
  
  # Solve for alpha_hats
  alpha_hats = solve(t(A) %*% A) %*% t(A) %*% Y 
  sum_a[i] = sum(alpha_hats)
}

# Approximate the mean and variance of sum_a
mean(sum_a)
var(sum_a)

# Plot the pdf of sum_a
pdf = density(sum_a)
plot(pdf)