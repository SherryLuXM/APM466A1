## set up the libraries
library(readxl)
library(janitor)
library(lubridate)
library(numDeriv)
library(tidyverse)

## load data and data cleaning
df <- read_excel("C:/Users/i5/Downloads/APM466 Introduction to Mathematical Finance/Assignment 1 Bond Data.xlsx")
bonds <- data.frame(df[2], df[4], df[6:16])
bonds <- bonds[-1,]
names <-  c("coupon", "mat_date", "issue_date", "cp10", "cp11", "cp12", "cp13", "cp14", "cp17", "cp18", "cp19", "cp20", "cp21") 
# cpxx stands for closing price for coupon on Jan.xx
colnames(bonds) <- names
head(bonds)
rows <- c(9, 12, 14, 17, 6, 26, 27, 29, 30, 32)
bond <- data.frame(bonds[rows,])

bond$mat_date<- as.Date(bond$mat_date, "%m/%d/%Y")
bond$issue_date<- as.Date(bond$issue_date, "%m/%d/%Y")
bond$coupon <- as.numeric(bond$coupon)
bond$cp10 <- as.numeric(bond$cp10)
bond$cp11 <- as.numeric(bond$cp11)
bond$cp12 <- as.numeric(bond$cp12)
bond$cp13 <- as.numeric(bond$cp13)
bond$cp14 <- as.numeric(bond$cp14)
bond$cp17 <- as.numeric(bond$cp17)
bond$cp18 <- as.numeric(bond$cp18)
bond$cp19 <- as.numeric(bond$cp19)
bond$cp20 <- as.numeric(bond$cp20)
bond$cp21 <- as.numeric(bond$cp21)
finalp <- 100 # bond payment at maturity, last coupon payment not included
dirty_price1 <- vector("double", 10)

####################################################################################################
##################################### YTM Calculator ###############################################
####################################################################################################

############## example for Setting Up YTM Calculator################################################
ex <- bond[1, 1:4]
ex
#                  coupon   mat_date issue_date cp10
# 10 0.0025 2022-08-01 2020-05-04 99.8

last_payment <- ex$mat_date %m-% months(12) # get the last payment date
jan10 <- as.Date("2022-01-10")
days_lastp <- as.numeric(jan10 - last_payment) # get the days since last payment
accrued0 <- days_lastp/365*ex$coupon*100
dirty_price1[1] <- accrued0 + ex$cp10
# dirty_price1[1] == 99.91096

coupon0 <- ex$coupon*100/2
n0 <- 12/6
ytm0 <- function(r){
  coupon0*(1-exp(-r*n0))/(exp(r)-1) + finalp*exp(-r*n0) - dirty_price1[1] 
}
curve(ytm, xlim=c(0,0.1), col='blue', lwd=2, lty=2, ylab='ytm(x)')
abline(h=0)
abline(v=0)

newton0 <- function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}

root0 <- newton0(ytm0, 0.00001, 0.02)
root0
# output:
# $`root approximation`
#[1]  0.001694901
################################################################################
# a function for getting the accrued interest, given maturity date[m] and day of buying[today]
accrued_interest <- function(m, today, c){
  cur <- m
  k <- vector("double", 10)
  for (i in 1:10){
    while (cur[i] > today) {
      cur[i] <- cur[i] %m-% months(6)
    } # getting the last payment date
    k[i] <- as.numeric(today - cur[i])/365*c[i]*100
  }
  return(k)
}

dirty_price1 <- accrued_interest(bond$mat_date, jan10, bond$coupon) + bond$cp10
dirty_price1

coupon <- vector("double", 10)
coupon <-  bond$coupon*100/2 # semi-annual coupon rate = coupon rate*100/2
# get n, the number of terms left for coupon payments
terms1 <- function(m, n){
  cur <- m
  for (i in 1:10){
    while (cur[i] > jan10) {
      cur[i] <- cur[i] %m-% months(6)
    } # getting the last payment date
    n[i] <- interval(cur[i], m[i]) %/% months(6)
  }
  return(n)
}
# terms1(ex$mat_date)
# output: 2
term1 <- vector("double", 10)
term1 <- terms1(bond$mat_date, term1)
term1
# calculate ytm for all bond prices collected on Jan.10
ytm_cp10 <- vector("double", 10)
#ytm <- function(r){
#  coupon*(1-exp(-r*n))/(exp(r)-1) + finalp*exp(-r*n) - dirty_price1
#}
newtonytm <- function(f, a = 0.00001, b = 0.02, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      return(root.approx)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}

for (i in 1:10){
  ytmi <- function(r){
    coupon[i]*(1-exp(-r*term1[i]))/(exp(r)-1) + finalp*exp(-r*term1[i]) - dirty_price1[i]
  }
  ytm_cp10[i] <- newtonytm(ytmi)
}
ytm_cp10

################################################################################
### function that calculate the ytm rate at different dates for all bonds#####
# dirty_price1
# output: 99.91096  99.42096  98.91896  99.53288 101.09836 100.08863  96.98945  95.19973  98.01890  98.85863
dirty_price <- matrix(, ncol = 10, nrow = 10)
jan11 <- as.Date("2022-01-11")
jan12 <- as.Date("2022-01-12")
jan13 <- as.Date("2022-01-13")
jan14 <- as.Date("2022-01-14")
jan17 <- as.Date("2022-01-17")
jan18 <- as.Date("2022-01-18")
jan19 <- as.Date("2022-01-19")
jan20 <- as.Date("2022-01-20")
jan21 <- as.Date("2022-01-21")
dates <- c(jan10, jan11, jan12, jan13, jan14, jan17, jan18, jan19, jan20, jan21)
term <- matrix(, ncol = 10, nrow = 10)
terms <- function(m, n, d){
  cur <- m
  for (i in 1:10){
    while (cur[i] > d) {
      cur[i] <- cur[i] %m-% months(6)
    } # getting the last payment date
    n[i] <- interval(cur[i], m[i]) %/% months(6)
  }
  return(n)
}
for (i in 1:10){
  dirty_price[,i] <- accrued_interest(bond$mat_date, dates[i], bond$coupon) + bond[, 3+i]
  term[,i] <- terms(bond$mat_date, term[,i], dates[i])
}

# terms1(ex$mat_date)
# output: 2
term
ytms <- matrix(, ncol = 10, nrow = 10)
for (j in 1:10){
  for (k in 1:10){
    ytmi <- function(r){
      coupon[k]*(1-exp(-r*term[k, j]))/(exp(r)-1) + finalp*exp(-r*term[k, j]) - dirty_price[k, j]
    }
    ytms[k, j] <- newtonytm(ytmi)
  }
}
ytms
as.data.frame(ytms)


############## graphing ################
today <- as.Date("2022-01-10")
to_maturity <- vector("double", 10)
for (i in 1:10){
  to_maturity[i] <- (interval(today, bond$mat_date[i]) %/% months(1))/12
}
to_maturity
adj_ytms <- as.data.frame(matrix(, ncol = 10, nrow = 10))
adj_ytms[1:4,] <- ytms[1:4,]
for (i in 5:10){
  for (j in 1:10){
    adj_ytms[i,j] <- ytms[i-1,j] + (0.5*i - to_maturity[i-1])*
      (ytms[i, j] - ytms[i-1, j])/(to_maturity[i] - to_maturity[i-1])
  }
}
adj_tomaturity <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
adj_ytms <- cbind(adj_tomaturity, adj_ytms)
colnames(adj_ytms) <- c("to_maturity", "Jan.10", "Jan.11", "Jan.12", "Jan.13", 
                    "Jan.14", "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21")
plot(adj_ytms$to_maturity, adj_ytms$Jan.10, type = "o", frame = FALSE, pch = 19, 
     col = "red", xlab = "Time to Maturity(in Years)", ylab = "Yield to Maturity Rate")
lines(adj_ytms$to_maturity, adj_ytms$Jan.11, pch = 19, col = "blue", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.12, pch = 19, col = "green", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.13, pch = 19, col = "orange", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.14, pch = 19, col = "cyan", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.17, pch = 19, col = "violet", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.18, pch = 19, col = "salmon", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.19, pch = 19, col = "coral", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.20, pch = 19, col = "pink", type = "o")
lines(adj_ytms$to_maturity, adj_ytms$Jan.21, pch = 19, col = "brown", type = "o")
legend("bottomright", legend=c("Jan.10", "Jan.11", "Jan.12", "Jan.13", "Jan.14",
                               "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21"),
       col=c("red", "blue", "green", "orange", "cyan", "violet", "salmon", 
             "coral", "pink", "brown"), lty = 1:10, cex=0.8)

################################################################################
############################ Yield/Spot Curve Calculator #######################
################################################################################
spot <- as.data.frame(matrix(, ncol = 10, nrow = 10))
ex2 <- bond[, 1:4]
ex2
spot[1,1] <- log((dirty_price[1,1] - coupon[1]*exp(-ytms[1,1]))/(coupon[1] + finalp))/(-2)
# 0.001694901
log((dirty_price[2,1] - coupon[2]*exp(-ytms[1,1]) - coupon[2]*exp(-2*spot[1,1]))/(coupon[2] + finalp))/(-3)
# 0.003189262

for (j in 1:10){
  for (k in 1:10){
    s <- coupon[k]*exp(-ytms[1, j])
    if (k > 1){
      for (l in 1:(k-1)){
        s <- s + coupon[k] * exp(-(l+1)*spot[l,j])
      }
    }
    spot[k, j] <- log((dirty_price[k,j] - s)/(coupon[k] + finalp))/(- k - 1) 
  }
}

adj_spot <- as.data.frame(matrix(, ncol = 10, nrow = 10))
adj_spot[1:4,] <- spot[1:4,]
for (i in 5:10){
  for (j in 1:10){
    adj_spot[i,j] <- spot[i-1,j] + (0.5*i - to_maturity[i-1])*
      (spot[i, j] - spot[i-1, j])/(to_maturity[i] - to_maturity[i-1])
  }
}
adj_spot*2
adj_spot <- cbind(adj_tomaturity, adj_spot)
colnames(adj_spot) <- c("to_maturity", "Jan.10", "Jan.11", "Jan.12", "Jan.13", 
                        "Jan.14", "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21")
plot(adj_spot$to_maturity, adj_spot$Jan.10, type = "o", frame = FALSE, pch = 19, 
     col = "red", xlab = "Term Time(in Years)", ylab = "Spot Rate")
lines(adj_spot$to_maturity, adj_spot$Jan.11, pch = 19, col = "blue", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.12, pch = 19, col = "green", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.13, pch = 19, col = "orange", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.14, pch = 19, col = "cyan", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.17, pch = 19, col = "violet", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.18, pch = 19, col = "salmon", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.19, pch = 19, col = "coral", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.20, pch = 19, col = "pink", type = "o")
lines(adj_spot$to_maturity, adj_spot$Jan.21, pch = 19, col = "brown", type = "o")
legend("bottomright", legend=c("Jan.10", "Jan.11", "Jan.12", "Jan.13", "Jan.14",
                               "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21"),
       col=c("red", "blue", "green", "orange", "cyan", "violet", "salmon", 
             "coral", "pink", "brown"), lty = 1:10, cex=0.8)

################################################################################
################## Forward Rate Calculator #####################################
################################################################################
forward <- as.data.frame(matrix(, ncol = 10, nrow = 7))
for (i in 1:10){
  for (j in 2:8){
    forward[j-1,i] <- (((1+adj_spot[j+2,i+1]/2)^(j+2)/(1+adj_spot[j,i+1]/2)^(j))^(1/2) - 1) * 2
  }
}
forward
forward_term <- c(1, 1.5, 2, 2.5, 3, 3.5, 4)
forward <- cbind(forward_term, forward)
colnames(forward) <- c("forward_term", "Jan.10", "Jan.11", "Jan.12", "Jan.13", 
                        "Jan.14", "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21")
plot(forward$forward_term, forward$Jan.10, type = "o", frame = FALSE, pch = 19, 
     col = "red", xlab = "Forward Term Time(in Years)", ylab = "Forward Rate")
lines(forward$forward_term, forward$Jan.11, pch = 19, col = "blue", type = "o")
lines(forward$forward_term, forward$Jan.12, pch = 19, col = "green", type = "o")
lines(forward$forward_term, forward$Jan.13, pch = 19, col = "orange", type = "o")
lines(forward$forward_term, forward$Jan.14, pch = 19, col = "cyan", type = "o")
lines(forward$forward_term, forward$Jan.17, pch = 19, col = "violet", type = "o")
lines(forward$forward_term, forward$Jan.18, pch = 19, col = "salmon", type = "o")
lines(forward$forward_term, forward$Jan.19, pch = 19, col = "coral", type = "o")
lines(forward$forward_term, forward$Jan.20, pch = 19, col = "pink", type = "o")
lines(forward$forward_term, forward$Jan.21, pch = 19, col = "brown", type = "o")
legend("bottomright", legend=c("Jan.10", "Jan.11", "Jan.12", "Jan.13", "Jan.14",
                               "Jan.17", "Jan.18", "Jan.19", "Jan.20", "Jan.21"),
       col=c("red", "blue", "green", "orange", "cyan", "violet", "salmon", 
             "coral", "pink", "brown"), lty = 1:10, cex=0.8)

################################################################################
################### Covariance Matrix Calculator ###############################
################################################################################
yield_dis <- as.data.frame(matrix(, ncol = 9, nrow = 5))
for (i in 1:5){
  for (j in 1:9){
    yield_dis[i,j] <- log(adj_ytms[2*i, j+2]/adj_ytms[2*i, j+1]) 
  }
}
yield_dis
coyield <- cov(yield_dis)
coyield
round(coyield, 6)
forward_dis <- as.data.frame(matrix(, ncol = 9, nrow = 4))
for (i in 1:4){
  for (j in 1:9){
    forward_dis[i,j] <- log(forward[2*i-1, j+2]/forward[2*i-1, j+1])
  }
}
forward_dis
coforward <- cov(forward_dis)
coforward
round(coforward, 6)
## calculating the eigenvalues and eigenvectors of the covariane matrix
a <- eigen(coyield)
a$values
a$vectors
round(a$values, 8)
round(a$vectors, 3)
b <- eigen(coforward)
b$values
b$vectors
round(b$values, 8)
round(b$vectors, 3)
