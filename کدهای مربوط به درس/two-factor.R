t<-factor(rep(c("15", "70", "125"), each=12))
m_ty<-factor(rep(c("1", "2", "3"), each=2, times=6))
bat<-c(130, 74, 150, 159, 138, 168,
       155, 180, 188, 126, 110, 160, 
       34, 80, 136, 106, 174, 150,
       40, 75, 122, 115, 120, 139, 
       20, 82, 25, 58, 96, 82,
       70, 58, 70, 45, 104, 60)
my_data<-data.frame(
  typeperature=t, material_type=m_ty, Battrey=bat
)

mode=aov(Battrey~material_type*typeperature, data=my_data)
summary(mode)

str(my_data)

###### use library table 5.1
library(MontgomeryDAE)
data(package="MontgomeryDAE")
Table5.1                   
my_data1<-Table5.1                   
str(my_data1)
my_data1$MaterialType<-factor(my_data1$MaterialType)
my_data1$Temperature<-as.factor(my_data1$Temperature)
str(my_data1)
mode1<-aov(BatteryLife~MaterialType*Temperature, data=my_data1)
summary(mode1)

### enter method row and col
set.seed(1245)


f1 <- factor(rep(c("1", "2", "3"), each = 12))       
f2 <- factor(rep(rep(c("15", "70", "75"), each = 4), 3)) 

data <- c(
  130, 155, 34, 40, 20, 70, 
  74, 180, 80, 75, 82, 58,
  150, 188, 136, 122, 25, 70,
  159, 126, 106, 115, 58, 45,
  138, 110, 174, 120, 96, 104,
  168, 160, 150, 139, 82, 60
)

data1 <- data.frame(matrial_type=f1, temperature=f2, data=data)
model <- aov(data ~ matrial_type * temperature, data = data1)
summary(model)

library(MontgomeryDAE)
data(paka)

#### enter data metod row

batt<-c(130, 155, 34, 40, 20, 70, 
        74, 180, 80, 75, 82, 58, 
        150, 188, 136, 122, 25, 70, 
        159, 126, 106, 115, 58, 45, 
        138, 110, 174, 120, 96, 104,
        168, 160, 150, 139, 82, 60
)

tt<-factor(rep(c("15", "70", "125"), each=2, times=6))
m_tyy<-factor(rep(c("1", "2", "3"), each=12))
my_dataa<-data.frame(
  typeperaturee=tt, material_typee=m_tyy, Battreyy=batt
)

modee=aov(Battreyy~material_typee*typeperaturee, data=my_dataa)
summary(modee)

####### sloution bock
Surface_Finish <-c(
  74, 79, 82, 99, 
  64, 68, 88, 104, 
  60, 73, 92, 96, 
  92, 98, 99, 104,
  86, 104, 108, 110,
  88, 88, 95, 99,
  99, 104, 108, 114,
  98, 99, 110, 111,
  102, 95, 99, 107
)
Depth_of_cut<-factor(rep(c(0.15, 0.18, 0.20, 0.25), times=9))
feed<-factor(rep(c(0.20, 0.25, 0.30), each=12))

length(Depth_of_cut)==length(Surface_Finish)

my_datasurface<-data.frame(
  Surface_Finish=Surface_Finish, Depth_of_cut=Depth_of_cut, feed=feed
)
str(my_datasurface)
mod12<-aov(Surface_Finish~Depth_of_cut*feed, data=my_datasurface)
table(feed, Depth_of_cut )

summary(mod12)