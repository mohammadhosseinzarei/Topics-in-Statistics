p=c(0.35, 0.5, 0.9, 1)
x=c(1,2,3,4)

z<-1
u=runif(1,0,1)
while(u > p[z])
   z=z+1
end
x[z]

p=0.5
u=runif(1,0,1)
x=u-p
x