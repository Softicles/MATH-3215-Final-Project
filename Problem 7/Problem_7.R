# Number of trials
N = 10^6

# Store the distances for each trial
distances = vector("double", N)

# Generate the distances
for (i in seq(1:N)) {
  dart1 = runif(2, -1, 1)
  dart2 = runif(2, -1, 1)
  
  d = sqrt((dart1[1]-dart2[1])^2 + (dart1[2]-dart2[2])^2)
  distances[i] = d
}

# Store the distances which is less than 1
distances_less_than_one <- distances[distances < 1]

# The probability of distance < 1
probability = length(distances_less_than_one) / N