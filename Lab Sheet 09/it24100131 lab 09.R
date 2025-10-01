setwd("C:\\Users\\Kaveesha\\Desktop\\it24100131")

# 1. Generate random sample (n = 25) from N(mean=45, sd=2)
set.seed(123)                 # reproducibility
bake_time <- rnorm(25, mean = 45, sd = 2)
bake_time                       # display sample

# Summary
n <- length(bake_time)
xbar <- mean(bake_time)
sd_known <- 2                   # population sd (given)
se <- sd_known / sqrt(n)

cat("n =", n, "\n")
cat("Sample mean =", round(xbar, 6), "\n")
cat("Known sigma =", sd_known, "\n")
cat("Standard error (sigma/sqrt(n)) =", round(se, 6), "\n\n")


# 2. One-sample z-test (H0: mu >= 46  vs  H1: mu < 46)
# test statistic (z) and one-sided p-value (lower tail)
z_stat <- (xbar - 46) / se
p_value <- pnorm(z_stat)        # P(Z <= z_stat) because alternative is "less"

cat("Z test statistic =", round(z_stat, 6), "\n")
cat("One-sided p-value =", format.pval(p_value, digits=6), "\n\n")

# Decision at alpha = 0.05
alpha <- 0.05
if (p_value < alpha) {
  cat("Conclusion: Reject H0 at alpha =", alpha, 
      "=> evidence that mean baking time < 46 minutes.\n")
} else {
  cat("Conclusion: Do NOT reject H0 at alpha =", alpha, 
      "=> insufficient evidence that mean baking time < 46 minutes.\n")
}

# (Optional) For comparison: use t.test (but note sigma known: z-test is more appropriate)
t_res <- t.test(bake_time, mu = 46, alternative = "less")
t_res

# (Optional) If you want a 95% CI for the mean using the known sigma:
# For a one-sided test you might report an upper confidence bound.
z_alpha <- qnorm(1 - alpha)    # 1.644854 for alpha=0.05 (one-sided)
upper_one_sided <- xbar + z_alpha * se
cat("\nOne-sided 95% upper confidence bound for mean (known sigma):\n")
cat("Mean <=", round(upper_one_sided, 6), "\n")