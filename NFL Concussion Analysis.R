#NFL Concussions

dat <- read.csv("http://webpages.math.luc.edu/~ebalderama/bayes_resources/data/ConcussionsByTeamAndYear.csv", header=T)

attach(dat)


#=====================================================
# Compare years
#=====================================================

# Model for year t:
# Y_t | lambda_t ~ Poisson(N*lambda_t)
# lambda_t ~ Gamma(a,b)


# 32 teams and each plays 16 games, so N=32*16=512 team games
N <- 512
a <- 0.01 
b <- 0.01

a2012 <- sum(X2012) + a
b2012 <- N + b

a2013 <- sum(X2013) + a
b2013 <- N + b

a2014 <- sum(X2014) + a
b2014 <- N + b

a2015 <- sum(X2015) + a
b2015 <- N + b

grid  <- seq(0.15,0.55,.001)

p2012 <- dgamma(grid,a2012,b2012)
p2013 <- dgamma(grid,a2013,b2013)
p2014 <- dgamma(grid,a2014,b2014)
p2015 <- dgamma(grid,a2015,b2015)

plot(grid,p2014,type="l",xlab="Concussion rate (per game)",ylab="Posterior density")
lines(grid,p2015,lty=2)
legend("topright",c("2014","2015"),lty=1:2,inset=0.05)

plot(grid,p2012,type="l",xlab="Concussion rate (per game)",ylab="Posterior density",ylim=c(0,18))
lines(grid,p2013,lty=2)
lines(grid,p2014,lty=3)
lines(grid,p2015,lty=4)

legend("topright",c("2012","2013","2014","2015"),lty=1:4,inset=0.05)


#use Monte Carlo sampling to compute the posterior probability that the rate increased from 2014-2015
lambda_2012 <- rgamma(1000000,a2012,b2012)
lambda_2013 <- rgamma(1000000,a2013,b2013)
lambda_2014 <- rgamma(1000000,a2014,b2014)
lambda_2015 <- rgamma(1000000,a2015,b2015)
mean(lambda_2013<lambda_2012)
mean(lambda_2014<lambda_2013)
mean(lambda_2015>lambda_2014)