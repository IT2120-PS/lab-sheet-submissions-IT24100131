##Setting the directory
setwd("C:\\Users\\Kaveesha\\Desktop\\it24100131 Lab 07")

# Q1 – Train arrival (Uniform Distribution)
# Random variable: X = minutes after 8:00 a.m. (Uniform(0, 40))
# We want P(10 <= X <= 25)
punif(25, min = 0, max = 40) - punif(10, min = 0, max = 40)


# Q2 – Software update time (Exponential Distribution)
# Random variable: X = time to complete update (hours)
# X ~ Exponential(rate = λ = 1/3)
# We want P(X <= 2)

pexp(2, rate = 1/3)

# Q3 – IQ scores (Normal Distribution)
# Random variable: X = IQ score
# X ~ Normal(mean = 100, sd = 15)
# (i) Probability IQ > 130
1 - pnorm(130, mean = 100, sd = 15)

# (ii) 95th percentile IQ score
qnorm(0.95, mean = 100, sd = 15)
