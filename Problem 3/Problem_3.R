# Number of trials
N = 10^5

vector_of_one = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
x <- 1:20

sum_square_a1_a2 = vector("double", N)
r = vector("double", N)

S <- function(v1, v2) {
  # Check if vectors are of the same length
  if (length(v1) != length(v2)) {
    stop("Vectors must be of the same length.")
  }
  
  # Center the vectors by subtracting their means
  v1_centered <- v1 - mean(v1)
  v2_centered <- v2 - mean(v2)
  
  # Compute the dot product
  dot_product <- sum(v1_centered * v2_centered)
  
  return(dot_product)
}

for (i in seq(1:N)) {
  Y = vector("double", 20)
  
  for (j in seq(1:20)) {
    Y[j] = 2 + 1 * j + rnorm(1, 0, 2)
  }
  
  
  A = cbind(vector_of_one, x)
  alphas = solve(t(A) %*% A) %*% t(A) %*% Y
  sum_square_a1_a2[i] = sum(alphas^2)
  r[i] = S(x,Y) / sqrt(S(x,x) * S(Y,Y))
}

# 1. Plot the sum squares of a1_hat and a2_hat. Approximate its mean and
# variance

# Approximate the mean and variance of sum squares of a1_hat and a2_hat
mean(sum_square_a1_a2)
var(sum_square_a1_a2)

# Plot its pdf
pdf_sum = density(sum_square_a1_a2)
plot(pdf_sum)

# 2. Plot the r. Approximate its mean and variance. Approximate P(0.9 < r 
# < 0.95)

# Approximate mean and variance of r
mean(r)
var(r)

# Plot its pdf
pdf_r = density(r)
plot(pdf_r)

count_table_r = table(r)
frequency_table_r <- as.data.frame(count_table_r)
colnames(frequency_table_r) <- c("Element", "Probability")
frequency_table_r$Probability <- frequency_table_r$Probability / N

# Approximate P(0.9 < r < 0.95)

frequency_table_r$Element <- as.numeric(as.character(frequency_table_r$Element))
result <- sum(subset(frequency_table_r, Element > 0.9 & Element < 0.95)$Probability)

