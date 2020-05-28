# Power analysis
# DoD Grant Proposal
#   by: Paul Gordon, Emma Fletcher
#       R. Noah Padgett
# ================================== #
# Created
#   Date: 2020-05-26
#   By: R. Noah Padgett
# Last Edited:
#   Date: 2020-05-27
#   By: R. Noah Padgett
# ================================== #


# 2 Diet groups
# 2 LPS Admin vs. Sham
# 2 Post LPS Admin groups.
## 2x2x2=8 grps in total

f2d <- function(f,ng){
  sqrt((f**2)*2*ng)
}

rs2f <- function(r2){
  sqrt(r2/(1-r2))
}

r2d <- function(r){
  (2*sqrt(r))/(sqrt(1-r))
}

# effect size = 0.313
# convert to d (i.e., beta)
eta = 0.313
d <- r2d(eta)
d

# gamma: significance value
# use Bonferroni correction
gamma = 0.05/7

NREP <- 10000
N <- seq(5, 20, 1)
sim.out <- data.frame(matrix(nrow=length(N)*NREP, ncol=6))
colnames(sim.out) <- c("iter", "N", "Ntotal", "r2", "pvalue", "power")
i <- j <- rep <- 1

for(i in 1:length(N)){
  for(rep in 1:NREP){
    # group IDs
    Nt <- N[i]*8
    n <- N[i]
    A <- c(rep(1,n*4),rep(0,n*4))
    B <- rep(c(rep(1, n*2), rep(0, n*2)),2)
    C <- rep(c(rep(1, n), rep(0, n)),4)
    
    dat <- data.frame(A, B, C)
    
    # Generate regression weights for "off factors"
    b <- c(d, d + rnorm(1, 0, 0.25),
           rnorm(1, 0, 0.25),
           d + rnorm(1, 0, 0.25),
           rnorm(3, 0, 0.25))
    
    # data model
    # y = beta0 + beta1*A + beta2*B + beta3*C + beta4*A:B + beta5*A:C + beta6*B:C + beta7*A:B:C
    e <- rnorm(Nt, 0, 1) # random error
    # simulate data
    dat$y = b[1]*A+b[2]*B+b[3]*C+b[4]*A*B+b[5]*A*C+b[6]*B*C+b[7]*A*B*C + e
    # run ANOVA
    fit <- aov(y~A*B*C, data=dat)
    SS <- summary(fit)[[1]]
    r2 <- SS[1,2]/(sum(SS[2:8,2]))
    
    sim.out[j, 1] <- rep
    sim.out[j, 2] <- N[i]
    sim.out[j, 3] <- Nt
    sim.out[j, 4] <- r2
    sim.out[j, 5] <- SS[1, 5]
    sim.out[j, 6] <- as.numeric(SS[1, 5] <= gamma & 
                                  SS[2, 5] <= gamma &
                                  SS[4, 5] <= gamma)
    
    j <- j + 1
  }
}

library(ggplot2)
library(dplyr)
plot.dat <- sim.out %>%
  group_by(N) %>%
  summarise(Power = mean(power),
            R2 = mean(r2))
plot.dat

p <- ggplot(plot.dat, aes(x=N, y=Power))+
  geom_point()+
  geom_hline(yintercept = 0.8, colour="red")+
  labs(x="Number of Rats per Experimental Group",
       y="Estimated Power",
       title="Power Curve of Experimental Design",
       subtitle="17 Rats per Group Needs for Power >= 0.8")+
  scale_x_continuous(breaks=seq(5,20,1))+
  scale_y_continuous(breaks = seq(0,1, 0.2), limits = c(0,1))+
  theme_classic()
p
