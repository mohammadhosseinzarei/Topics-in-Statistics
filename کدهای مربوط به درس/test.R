set.seed(3685)
n=1000000
x<- runif(n)
y<-runif(n)

d<-x^2+y^2<=1
d_hat<-4*mean(d)
d_hat

x<-runif(n)
mean(exp(x))


set.seed(123)
n <- 1e5
x <- runif(n)
# 
I_hat <- mean(log(x))


n<-1e5
x<-runif(n)
hat_x<-mean(sin(x))
hat_x



n<-10000
x<-rexp(n, rate=1)
g_x=x/2
w_x=exp(0.5*x)
hat_x2=mean(g_x*w_x)


mean(exp(0.5*x))


n=100000
x=rexp(n, rate=1)
pi_x=dgamma(x, shape=1, rate=2)
w_x=pi_x/exp(-x)
mean(w_x)


n=100000
x=rexp(n, rate=1)
pi_x=dgamma(x, shape=1, rate=2)
q_x=dexp(x, rate=1)
w_x=pi_x/q_x
mean(w_x)

rlaplace_manual <- function(n, mu = 0, b = 1) {
  u <- runif(n, -0.5, 0.5)
  mu - b * sign(u) * log(1 - 2 * abs(u))
}

# نمونه‌گیری
set.seed(123)
x <- rlaplace_manual(10000)
hist(x, breaks = 50, probability = TRUE)

n=1e5
x=rexp(n, rate=1)
pi_x=dnorm(x, mean=1, sd=2)
q_x=dexp(x, rate = 0.25)
w_x=pi_x/q_x
hatt_x=mean(w_x)
real_x<-1
real_x-hatt_x

n <- 1e5
x <- rnorm(n, mean=1, sd=3)           
pi_x <- dnorm(x, mean=1, sd=2)      
q_x  <- dnorm(x, mean=1, sd=3)       
w_x  <- pi_x / q_x
real_x=mean(w_x) 


x<-rexp(n, rate = 1)
h<-sin(x)
I_hat<-mean(h)
I_exact<-0.5 # چون قسمت مثبت را میخواهیم باید یک دو شود.


n<-5e4
u<-runif(n)
f_u<-exp(u)
u1<-runif(n)
f_u_1<-exp(1-u1)
hat_u<-mean(f_u + f_u_1)/2

real_u<-exp(1)-1


set.seed(111)
n <- 5e4
# MC
u_standard <- runif(n)
est_standard <- mean(exp(u_standard))
# MC Antithetic
u_antithetic <- runif(n)
est_antithetic <- mean((exp(u_antithetic) +
                          exp(1 - u_antithetic)) / 2)
I_exact <- exp(1) - 1

n=1e6
u<-runif(n)
x<-u/2
u_1<-runif(n)
x1<-1-u_1/2
hat_x<-mean((x + x1)/2)

c=3/2
(1)^c/c
