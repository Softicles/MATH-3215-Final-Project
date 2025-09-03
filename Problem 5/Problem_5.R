# The list of eigen values
eigvals = vector("double", 0)

# Number of trials
N = 10^3
dim = 120

# generate values for eigvals list which consists of all the eigval of M
for (i in seq(1:N)) {
  M = matrix(nrow = dim, ncol = dim)
  i = 0
  while(i < dim) {
    j = i
    while(j < dim) {
      M[i + 1, j + 1] = rnorm(1, 0, 1)
      M[j + 1, i + 1] = M[i + 1, j + 1]
      j = j + 1
    }
    i = i + 1
  }
  # Get the eigen values of M
  eigvals_temp_list = eigen(M, only.values = TRUE)

  eigvals <- c(eigvals, eigvals_temp_list$values[sample(dim, 1)])
}

# Approximate the mean and variance of eigvals
mean(eigvals)
var(eigvals)

# We observe that the variance of eigvals is 149.9887 which is approximately
# the dimension of V where M maps a vector from V to V. This holds when I
# change the dimension of V, so I guess that the actual variance of X is the 
# same as dim V.

# Plot the pdf of X
pdf = density(eigvals)
plot(pdf)