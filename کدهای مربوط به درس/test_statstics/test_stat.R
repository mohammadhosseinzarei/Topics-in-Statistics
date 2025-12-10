set.seed(123)
x<-rnorm(20, mean=52, sd=20); n<-length(x); mu0<-50 ; alpha<-0.05
x_bar<-mean(x); s<-sqrt((1/(n-1))* sum((x - x_bar)**2))
t<-(x_bar - mu0)/(s/sqrt(n))
df <- n-1
p_value<-2*(1-(pt(abs(t), df)))

#CI
T_alpha<-qt(alpha/2, df)
cI_lower<-x_bar+T_alpha*(s/sqrt(n))
CI_upper<-x_bar-T_alpha*(s/sqrt(n))
t.test(x, mu = 50)
################################################3
################################################
set.seed(3685)
group1<-rnorm(25, mean=10, sd=3)
group2<-rnorm(25, mean=12, sd=3)
n1<-length(group1); n2<-length(group2)
x_bar1<-mean(group1); x_bar2<-mean(group2)
s1<-sd(group1); s2<-sd(group2)
s_p<-sqrt(((n1-1)*(s1**2) + (n2-1)*(s2**2))/(n1+n2-2))
t<-(x_bar1-x_bar2)/(s_p*(sqrt((1/n1)+(1/n2))))
alpha<-0.05
df<-n1+n2-2
p_value1<-2*(1-pt(abs(t), df))
#CI
T_alpha<-qt(alpha/2, df)
CI<-c((x_bar1-x_bar2)+T_alpha*(s_p*(sqrt((1/n1)+(1/n2)))), 
          (x_bar1-x_bar2)-T_alpha*(s_p*(sqrt((1/n1)+(1/n2)))))

t.test(group1, group2, var.equal=TRUE)
#######################################################
#######################################################
set.seed(258)
group1<-rnorm(25, mean=10, sd=3)
group2<-rnorm(25, mean=12, sd=3)
n1<-length(group1); n2<-length(group2)
group1_bar<-mean(group1); group2_bar<-mean(group2)
S12<-(1/(n1-1))*(sum((group1-group1_bar)**2))
S22<-(1/(n2-1))*(sum((group2-group2_bar)**2))
t<-(group1_bar-group2_bar)/(sqrt((S12/n1)+(S22/n2)))

v<-(((S12/n1)+(S22/n2))**2)/((((S12/n1)**2)/(n1-1))+(((S22/n2)**2)/(n2-1)))
p_value1<-2*(1-pt(abs(t), v))
T_alpha<-qt(0.05/2, v)
CI<-c((group1_bar-group2_bar)+T_alpha*(sqrt((S12/n1)+(S22/n2))), 
      (group1_bar-group2_bar)-T_alpha*(sqrt((S12/n1)+(S22/n2))))

# Equal variance assumption
t.test(group1, group2, var.equal = TRUE)
# Unequal variance (Welch's test)
t.test(group1, group2, var.equal = FALSE)
#########################################################
#Paired t-Test
########################################################
before <- c(80, 82, 78, 75, 89, 90)
after <- c(85, 83, 80, 79, 88, 92)
D<-before-after; Dbar<-mean(D); alpha<-0.05
n<-length(D)
SD<-sqrt((1/(n-1))*(sum((D-Dbar)**2)))
t<-(Dbar)/(SD/sqrt(n))
t.test(before, after, paired = TRUE)
#to_side
df<-n-1
p_vlaue<-2*(1-pt(abs(t), df))
T_alpha<-qt(alpha/2, df)
CI<-c(Dbar+T_alpha*(SD/sqrt(n)),
      Dbar-T_alpha*(SD/sqrt(n)))
###############################################################
#One-Sample Proportion Test
################################################################
x <- 55; n <- 120; alpha=0.05; df=1
p_hat<-x/n; p0<-0.5
#z^2~x^2
z<-(p_hat-p0)/sqrt((p0*(1-p0)/n))
#z^2~x^2
X2<-z**2
p_value <- 1 - pchisq(X2, df)
z_alpha<-qnorm(1-(alpha/2), 1)
CI<-c(p_hat-z_alpha*(sqrt((p0*(1-p0))/n)),
      p_hat+z_alpha*(sqrt((p0*(1-p0))/n)))
prop.test(x, n, p = 0.5, alternative = "two.sided", correct = FALSE)

#######################################################################
# Two-Sample Proportion Test
#########################################################################

x1<-45; n1<-100; x2<-55; n2<-120; alpha<-0.05
p1_hat<-x1/n1; p2_hat<-x2/n2; p_hat<-(x1 + x2)/(n1 + n2)
z<-(p1_hat-p2_hat)/sqrt(p_hat*(1-p_hat)*((1/n1) + (1/n2)))
#x^2~z^2
p_vlaue<-2*(1-pnorm(abs(z)))
z_alpha<-qnorm(1-alpha/2)
CI<-c((p1_hat-p2_hat)-z_alpha*(sqrt(p_hat*(1-p_hat)*((1/n1) + (1/n2)))),
      (p1_hat-p2_hat)+z_alpha*(sqrt(p_hat*(1-p_hat)*((1/n1) + (1/n2)))))
#use library
?prop.test
x<-c(x1, x2)
n<-c(n1, n2)
prop.test(x, n, alternative = "two.sided", correct = FALSE)
