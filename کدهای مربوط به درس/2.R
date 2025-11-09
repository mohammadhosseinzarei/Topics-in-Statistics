library(daewr)
?daewr
bread
str(bread)

my_data=data.frame(loaf=c(1,2,3,4,5, 6, 7, 8, 9, 10, 11, 12),
                   time=c(35, 35, 35, 35, 40, 40,40, 40,45, 45, 45, 45),
                  height=c(4.5, 5.00, 5.50, 6.75, 6.50, 6.50, 10.50, 9.50, 9.75, 8.75, 6.50, 8.25))
my_data$time=factor(my_data$time)
str(my_data)
write.csv(my_data, file="E:/deo/book/my_data.csv", row.names = FALSE)

data2=read.csv("E:/deo/book/my_data.csv")
mod0<-lm(height~time, data=my_data)
summary(mod0)
set.seed(3685)
f=factor(rep(c(35, 40, 45), each=4))
fa=sample(f, 12)
eu=1:12
data=data.frame(loaf=eu, time=fa)


library(daewr )
mod0 <- lm( height ~ time, data = bread )
summary( mod0 )

