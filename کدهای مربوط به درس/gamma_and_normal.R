# حل گاما و نرمال به روش مونت کارالو و اختلاف بین مقدار وقتی و تخمین ان
set.seed(3685)
n=1e5
x=rexp(n, rate=1)
pi_x=dgamma(x, shape=1, rate=2)
q_x=dexp(x, rate=1)
w_x=pi_x/q_x
vhat_ga<-mean(w_x)
vreal_ga=1
vreal_ga-vhat_ga


#gamma
n <- 1e5
x <- rnorm(n, mean=1, sd=3)           
pi_x <- dnorm(x, mean=1, sd=2)      
q_x  <- dnorm(x, mean=1, sd=3)       
w_x  <- pi_x / q_x
vhat_no=mean(w_x) 
vreal_no=1
vreal_no-vhat_no