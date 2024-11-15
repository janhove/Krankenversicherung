# Simulation
p_nbinom <- 0.3
size_nbinom <- 2
p_binom <- 0.72
size_binom <- 200
total_costs <- replicate(50000, {
  K <- rbinom(1, size_binom, p_binom)
  sum(rnbinom(K, size_nbinom, p_nbinom))
})
hist(total_costs, freq = FALSE)

# Characteristic function of Binomial(n,p)
pgf_binomial <- function(x, n, p) {
  ((1-p) + x*p)^n
}

n <- max(total_costs)
T_minus <- matrix(NA, nrow = n, ncol = n)
T_plus <- T_minus
for (j in 1:n) {
  for (k in 1:n) {
    T_minus[j, k] <- exp(complex(real = 0, imaginary = -2*pi*(j-1)*(k-1)/n))
    T_plus[j, k] <- exp(complex(real = 0, imaginary = 2*pi*(j-1)*(k-1)/n))
  }
}

p_vec <- dnbinom(0:(n-1), size_nbinom, p_nbinom)
dens <- (1/n * T_minus %*% pgf_binomial(T_plus %*% p_vec, size_binom, p_binom)) |> Re()

plot(ecdf(total_costs), col = "darkblue", pch = ".")
lines(cumsum(dens), col = "red")
curve(pnorm(x, mean = mean(total_costs), sd = sd(total_costs)), add = TRUE,
      col = "darkgreen")
