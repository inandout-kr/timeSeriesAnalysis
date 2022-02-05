## R
## notice in this case we have chosen to set a seed
## if you set the same seed, our numbers should match
set.seed(123)
## set parameters for the distribution of each of the four
## market states we want to represent
bull_mu <- 0.1
bull_sd <- 0.1

neutral_mu <- 0.02
neutral_sd <- 0.08

bear_mu <- -0.03
bear_sd <- 0.2

panic_mu <- -0.1
panic_sd <- 0.3
## collect these parameters in vectors for easy indexing
mus <- c(bull_mu, neutral_mu, bear_mu, panic_mu)
sds <- c(bull_sd, neutral_sd, bear_sd, panic_sd)
## set some constants describing the time series we will generate
NUM.PERIODS <- 10
SMALLEST.PERIOD <- 20
LONGEST.PERIOD <- 40
## stochastically determine a series of day counts, with
## each day count indicating one 'run' or one state of the market
days <- sample(SMALLEST.PERIOD:LONGEST.PERIOD, NUM.PERIODS,
               replace = TRUE)
## for each number of days in the vector of days
## we generate a time series for that run of days in a particular
## state of the market and add this to our overall time series
returns <- numeric()
true.mean <- numeric()
for (d in days) {
  idx = sample(1:4, 1, prob = c(0.2, 0.6, 0.18, 0.02))
  returns <- c(returns, rnorm(d, mean = mus[idx], sd = sds[idx]))
  true.mean <- c(true.mean, rep(mus[idx], d))
}

install.packages("depmixS4")
require(depmixS4)
hmm.model <- depmix(returns ~ 1, family = gaussian(),
                    nstates = 4, data=data.frame(returns=returns))
model.fit <- fit(hmm.model)
post_probs <- posterior(model.fit)

## 시각화
plot(returns, type = 'l', lwd = 3, col = 1,
     yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     ylim = c(-0.6, 0.6))
lapply(0:(length(returns) - 1), function (i) {
  ## add a background rectangle of the appropriate color
  ## to indicate the state during a given time step
  rect(i,-0.6,(i + 1),0.6,
       col = rgb(0.0,0.0,0.0,alpha=(0.2 * post_probs$state[i + 1])),
       border = NA)
})
