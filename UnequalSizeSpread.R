library(tidyverse)
library(MultNonParam)
library(rmutil)

set.seed(1)
trialNum <- 1000

# Cauchy
KScountC = rep(0, 4)
ADcountC = rep(0, 4)

# Normal
KScountN = rep(0, 4)
ADcountN = rep(0, 4)

# Laplace
KScountL = rep(0, 4)
ADcountL = rep(0, 4)

n <- 10
  print("cv10")
  # determine the critical values for KS and AD tests
  x <- rcauchy(n)
  y <- rcauchy(2*n)*2
  
  times_alt <- c(x, y)
  delta <- rep(1, 3*n)
  grp <- c(rep(0, n), rep(1, 2*n))
  
  cvs <- twosamplesurvpvs(times_alt, delta, grp, plotme = FALSE)$cv
  cvKS <- cvs[1]
  cvAD <- cvs[2]
  
  for (i in 1:trialNum) {
    print(i)
    # generate samples from Cauchy Distribution
    x <- rcauchy(n)
    y <-rcauchy(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountC[1] = KScountC[1] + 1
    }
    if (result[2] > cvAD) {
      ADcountC[1] = ADcountC[1] + 1
    }
    
    # generate samples from Normal Distribution
    x <- rnorm(n)
    y <- rnorm(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountN[1] = KScountN[1] + 1
    }
    if (result[2] > cvAD) {
      ADcountN[1] = ADcountN[1] + 1
    }
    
    # generate samples from Laplace Distribution
    x <- rlaplace(n)
    y <- rlaplace(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountL[1] = KScountL[1] + 1
    }
    if (result[2] > cvAD) {
      ADcountL[1] = ADcountL[1] + 1
    }
  }

n <- 20
  print("cv20")
  # determine the critical values for KS and AD tests
  x <- rcauchy(n)
  y <- rcauchy(2*n)*2
  
  times_alt <- c(x, y)
  delta <- rep(1, 3*n)
  grp <- c(rep(0, n), rep(1, 2*n))
  
  cvs <- twosamplesurvpvs(times_alt, delta, grp, plotme = FALSE)$cv
  cvKS <- cvs[1]
  cvAD <- cvs[2]
  
  for (i in 1:trialNum) {
    print(i)
    # generate samples from Cauchy Distribution
    x <- rcauchy(n)
    y <-rcauchy(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountC[2] = KScountC[2] + 1
    }
    if (result[2] > cvAD) {
      ADcountC[2] = ADcountC[2] + 1
    }
    
    # generate samples from Normal Distribution
    x <- rnorm(n)
    y <- rnorm(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountN[2] = KScountN[2] + 1
    }
    if (result[2] > cvAD) {
      ADcountN[2] = ADcountN[2] + 1
    }
    
    # generate samples from Laplace Distribution
    x <- rlaplace(n)
    y <- rlaplace(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountL[2] = KScountL[2] + 1
    }
    if (result[2] > cvAD) {
      ADcountL[2] = ADcountL[2] + 1
    }
  }

n <- 30
  print("cv30")
  # determine the critical values for KS and AD tests
  x <- rcauchy(n)
  y <- rcauchy(2*n)*2
  
  times_alt <- c(x, y)
  delta <- rep(1, 3*n)
  grp <- c(rep(0, n), rep(1, 2*n))
  
  cvs <- twosamplesurvpvs(times_alt, delta, grp, plotme = FALSE)$cv
  cvKS <- cvs[1]
  cvAD <- cvs[2]
  
  for (i in 1:trialNum) {
    print(i)
    # generate samples from Cauchy Distribution
    x <- rcauchy(n)
    y <-rcauchy(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountC[3] = KScountC[3] + 1
    }
    if (result[2] > cvAD) {
      ADcountC[3] = ADcountC[3] + 1
    }
    
    # generate samples from Normal Distribution
    x <- rnorm(n)
    y <- rnorm(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountN[3] = KScountN[3] + 1
    }
    if (result[2] > cvAD) {
      ADcountN[3] = ADcountN[3] + 1
    }
    
    # generate samples from Laplace Distribution
    x <- rlaplace(n)
    y <- rlaplace(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountL[3] = KScountL[3] + 1
    }
    if (result[2] > cvAD) {
      ADcountL[3] = ADcountL[3] + 1
    }
  }

n <- 40
  print("cv40")
  # determine the critical values for KS and AD tests
  x <- rcauchy(n)
  y <- rcauchy(2*n)*2
  
  times_alt <- c(x, y)
  delta <- rep(1, 3*n)
  grp <- c(rep(0, n), rep(1, 2*n))
  
  cvs <- twosamplesurvpvs(times_alt, delta, grp, plotme = FALSE)$cv
  cvKS <- cvs[1]
  cvAD <- cvs[2]
  
  for (i in 1:trialNum) {
    print(i)
    # generate samples from Cauchy Distribution
    x <- rcauchy(n)
    y <-rcauchy(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountC[4] = KScountC[4] + 1
    }
    if (result[2] > cvAD) {
      ADcountC[4] = ADcountC[4] + 1
    }
    
    # generate samples from Normal Distribution
    x <- rnorm(n)
    y <- rnorm(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountN[4] = KScountN[4] + 1
    }
    if (result[2] > cvAD) {
      ADcountN[4] = ADcountN[4] + 1
    }
    
    # generate samples from Laplace Distribution
    x <- rlaplace(n)
    y <- rlaplace(2*n)*2
    
    times_alt <- c(x, y)
    delta <- rep(1, 3*n)
    grp <- c(rep(0, n), rep(1, 2*n))
    
    result <- twosamplesurvtests(times_alt, delta, grp)
    
    if (result[1] > cvKS) {
      KScountL[4] = KScountL[4] + 1
    }
    if (result[2] > cvAD) {
      ADcountL[4] = ADcountL[4] + 1
    }
  }

KSpowerC = rep(0, 4)
ADpowerC = rep(0, 4)
KSpowerN = rep(0, 4)
ADpowerN = rep(0, 4)
KSpowerL = rep(0, 4)
ADpowerL = rep(0, 4)

for (i in 1:4) {
  KSpowerC[i] = KScountC[i]/trialNum
  ADpowerC[i] = ADcountC[i]/trialNum
  KSpowerN[i] = KScountN[i]/trialNum
  ADpowerN[i] = ADcountN[i]/trialNum
  KSpowerL[i] = KScountL[i]/trialNum
  ADpowerL[i] = ADcountL[i]/trialNum
}

simSpread1.data <- data.frame(
  names = c("Cauchy", "Normal", "Laplace"),
  KS = c(KSpowerC[1], KSpowerN[1], KSpowerL[1]),
  AD = c(ADpowerC[1], ADpowerN[1], ADpowerL[1])
)

simSpread2.data <- data.frame(
  names = c("Cauchy", "Normal", "Laplace"),
  KS = c(KSpowerC[2], KSpowerN[2], KSpowerL[2]),
  AD = c(ADpowerC[2], ADpowerN[2], ADpowerL[2])
)

simSpread3.data <- data.frame(
  names = c("Cauchy", "Normal", "Laplace"),
  KS = c(KSpowerC[3], KSpowerN[3], KSpowerL[3]),
  AD = c(ADpowerC[3], ADpowerN[3], ADpowerL[3])
)

simSpread4.data <- data.frame(
  names = c("Cauchy", "Normal", "Laplace"),
  KS = c(KSpowerC[4], KSpowerN[4], KSpowerL[4]),
  AD = c(ADpowerC[4], ADpowerN[4], ADpowerL[4])
)

write_csv(simSpread1.data, path = "C:/internship/simSpread1.data")
write_csv(simSpread2.data, path = "C:/internship/simSpread2.data")
write_csv(simSpread3.data, path = "C:/internship/simSpread3.data")
write_csv(simSpread4.data, path = "C:/internship/simSpread4.data")