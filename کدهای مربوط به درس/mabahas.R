install.packages("BSDA")
set.seed(123)
x<-rnorm(20, mean=52, sd=4)
mu0<-50
sigma<-4

library("BSDA")
z.test(x=x, mu=mu0, sigma.x=sigma, alternative="two.sided", conf.level=0.95 )

z.test(x=x, mu=mu0, sigma.x = sigma, alternative="greater", conf.level = 0.95)

z.test(x=x, mu=mu0, sigma.x= sigma, alternative = "less", conf.level = 0.95)

#################################################################################
# تنظیم دانه تصادفی برای تکرارپذیری
set.seed(123)

# تولید داده نمونه
x <- rnorm(20, mean = 52, sd = 4)

# پارامترهای آزمون
mu0 <- 50          # میانگین فرض صفر
n <- length(x)     # حجم نمونه
xbar <- mean(x)    # میانگین نمونه
s <- sd(x)         # انحراف معیار نمونه
df <- n - 1        # درجات آزادی

# سطح معنی داری
alpha <- 0.05

# محاسبه آماره t
t_stat <- (xbar - mu0) / (s / sqrt(n))

# محاسبه p-value (دوطرفه)
p_value <- 2 * (1 - pt(abs(t_stat), df))

# مقدار بحرانی t برای فاصله اطمینان
t_crit <- qt(1 - alpha/2, df)

# محاسبه فاصله اطمینان 95%
CI_lower <- xbar - t_crit * s / sqrt(n)
CI_upper <- xbar + t_crit * s / sqrt(n)

# نمایش نتایج
cat("میانگین نمونه:", xbar, "\n")
cat("انحراف معیار نمونه:", s, "\n")
cat("آماره t:", t_stat, "\n")
cat("p-value:", p_value, "\n")
cat("فاصله اطمینان 95%: [", CI_lower, ",", CI_upper, "]\n")


#################################################################
## از راست
# تنظیم تصادف‌پذیری برای تکرارپذیری
set.seed(123)

# داده‌ها
x <- rnorm(20, mean = 52, sd = 4)

# پارامترهای نمونه
xbar <- mean(x)
s <- sd(x)
n <- length(x)
df <- n - 1

# مقدار میانگین فرضی
mu0 <- 50

# سطح معنی‌داری
alpha <- 0.05

# آماره t
t_stat <- (xbar - mu0) / (s / sqrt(n))

# p-value برای آزمون یک‌طرفه راست (H1: μ > μ0)
p_value <- 1 - pt(t_stat, df)

# مقدار بحرانی t برای یک‌طرفه
t_crit <- qt(1 - alpha, df)

# فاصله اطمینان یک‌طرفه راست: [Lower, ∞)
CI_lower <- xbar - t_crit * s / sqrt(n)

# نمایش نتایج
cat("t =", round(t_stat, 3), "\n")
cat("p-value =", round(p_value, 4), "\n")
cat("t_critical =", round(t_crit, 3), "\n")
cat("95% One-sided Confidence Interval: [", round(CI_lower, 3), ", ∞ )\n")

# تصمیم‌گیری
if (p_value < alpha) {
  cat("نتیجه: فرض صفر رد می‌شود (μ > μ0).\n")
} else {
  cat("نتیجه: فرض صفر رد نمی‌شود.\n")
}

##################### از چپ
# تنظیم تصادف‌پذیری برای تکرارپذیری
set.seed(123)

# داده‌ها
x <- rnorm(20, mean = 52, sd = 4)

# پارامترهای نمونه
xbar <- mean(x)
s <- sd(x)
n <- length(x)
df <- n - 1

# مقدار میانگین فرضی
mu0 <- 50

# سطح معنی‌داری
alpha <- 0.05

# آماره t
t_stat <- (xbar - mu0) / (s / sqrt(n))

# p-value برای آزمون یک‌طرفه چپ (H1: μ < μ0)
p_value <- pt(t_stat, df)

# مقدار بحرانی t برای یک‌طرفه چپ
t_crit <- qt(alpha, df)

# فاصله اطمینان یک‌طرفه چپ: (-∞ , Upper]
CI_upper <- xbar + t_crit * s / sqrt(n)

# نمایش نتایج
cat("t =", round(t_stat, 3), "\n")
cat("p-value =", round(p_value, 4), "\n")
cat("t_critical =", round(t_crit, 3), "\n")
cat("95% One-sided Confidence Interval: (-∞ ,", round(CI_upper, 3), "]\n")

# تصمیم‌گیری
if (p_value < alpha) {
  cat("نتیجه: فرض صفر رد می‌شود (μ < μ0).\n")
} else {
  cat("نتیجه: فرض صفر رد نمی‌شود.\n")
}

#####################################################
set.seed(1245)
x<-rnorm(30, mean=13, sd=3)
y<-rnorm(25, mean=12, sd=3)

#aample statsic
n1=length(x) ; n2<-length(y)
xbar<-mean(x) ; ybar<-mean(y)
sd1<-sd(x)^2; sd2<-sd(y)^2

sp<-((n1-1)*sd1 + (n2 -1)*sd2) / (n1+n2-2)
s_sp<-sqrt(sp)

t_test<-(xbar - ybar)/(s_sp*sqrt((1/n1 + 1/n2)))
alpha<-0.05
df<- n1 + n2 -2
t_crit<- qt(1-alpha/2, df )                  
p_value<-2*(1- pt(abs(t_test), df))

CI_lower<-(xbar-ybar) - t_crit * s_sp * sqrt(1/n1 + 1 / n2)
CI_upper<-(xbar-ybar) + t_crit * s_sp * sqrt(1/n1 + 1 / n2)

if (abs(t_test) > t_crit){
  cat ("reject hapothesis H0")
} else {
  cat ("Acc hapothesis H0 ") 
}

###################################آموزن تستی

x<-c(80, 82, 78, 75, 89, 90)
y<-c(85, 83, 80, 79, 88, 92)
D<-x-y
Dbar<-mean(D)
sd<-sqrt(sum(((D-Dbar))^2)/(length(D)-1))
t_test<-Dbar/(sd/sqrt(length(D)))
df<-length(D)-1
p_value<-2*(1- pt(abs(t_test), df))
t.test(x, y, paired = TRUE)
alpha<-0.05
if (p_value< alpha) {
  cat("reject hapothesis H0 ")} else {
    cat('acc hapothesis H0')
  }
##########################################
#شبیه سازی از توزیع وایبل
#########################################
weibull_pdf <- function(x, beta, lambda){
  ifelse(x < 0, 0,
         (beta/lambda)*(x/lambda)^(beta-1)*exp(-(x/lambda)^beta))
}
weibull_cdf <- function(x, beta, lambda){
  ifelse(x < 0, 0,
         1 - exp(-(x/lambda)^beta))
}
weibull_survival <- function(x, beta, lambda){
  ifelse(x < 0, 1,
         exp(-(x/lambda)^beta))
}

weibull_inv <- function(u, beta, lambda){
  lambda * (-log(1-u))^(1/beta)
}
