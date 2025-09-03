# Number of trials
N = 10^5

# Number of car crashes each trial
n = 1000

# Store the number of car crashes before 10 months of each trial
results = vector("integer", N)

# Generate the results vector
for (i in seq(1:N)) {
  interval_vector = rexp(n, 1/3)
    
  timeline_vector = cumsum(interval_vector)
  main_timeline <- timeline_vector[timeline_vector < 10]
  results[i] = length(main_timeline)
}

# Get the approximate for expected number of crashes in 10 months
mean(results)

plot(density(results))
