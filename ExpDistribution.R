ToothGrowth## Exponential distribution and comparaison it with the Central Limit Theorem (CTL)

set.seed(1000)
lambda <- 0.2
theo_mean <- 1/lambda #which the result is 5
theo_var <- 1/(lambda^2)/n
iteration <- 10000
n <- 40

# Exponential distribution with lambda = 0.2 and n = 40 without iterations
exp_distrib <- rexp(n, lambda)
length(exp_distrib)

# Exponential distribution with lambda = 0.2 and n = 40 with  10000 iterations
exp_distrib2 <- replicate(iteration,rexp(n, lambda))
dim (exp_distrib2)

# Draw the exponential distribution with density function
hist(exp_distrib2, breaks = 25, col = c('#99CCFF'), xlab ="X", main = expression("Exponential distribution "(lambda *" = 0.2")), freq = FALSE , ylim = c(0,0.17), xlim = c(0,40))
lines(density(exp_distrib2), col = c('#FF6666'), lwd=3)

# Mean and variance for each iteration (such 1000)
colmean <- colMeans(exp_distrib2)

sample_mean <- mean(colmean)
sample_var <- var(colmean)

print(paste("Theorical mean equal to", theo_mean, "and sample mean equal to",round(sample_mean,3)))
print(paste("Theorical variance equal to", theo_var, "and sample variance equal to",round(sample_var,3)))

# Draw the cumulative empirical mean
cumul_mean <-cumsum(colmean)/seq_along(colmean)
cumum_var <- cumsum(var(colmean))
plot(cumul_mean, type="l", col = c('#663300'), lwd =2 ,xlab = "Iterations", ylab = "Mean value", main = expression("Cumulative Emperical Mean " (lambda *" = 0.2")))
abline(h=theo_mean, col = c('#FF6666'), lwd=2)

# Draw the normal distribution

hist(colmean, breaks = 25, freq = FALSE, xlim = c(2,9), ylim = c(0,0.6),col = c('#99CCFF'), main = "Mean distribution : almost Normal", xlab = "Mean")
lines(density(colmean), col = c('#FF6666'), lwd=3)
abline(v=sample_mean, col = c('#FF66FF'), lwd=3)
legend("topright", legend = c("Mean"), col = c('#FF66FF'), lwd =2)
