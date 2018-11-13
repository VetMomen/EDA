data('cars')

with(cars,plot(x = speed,dist))

#to use base plot you have to follow these order 
# 1 - set parameters 
# 2 - make a plot 
# 3 - annotate the plot 

# what are the pramaters ?
?par
library(datasets)

data("airquality")

hist(airquality$Ozone)

with(airquality,plot(Wind,Ozone))
title(main = 'Ozone and Wind in New York City')

airquality<-transform(airquality,Month=factor(Month))
with(airquality,boxplot(airquality,Ozone~Month,xlab='Month',ylab='Ozone'))

with(airquality[airquality$Month==5,],plot(Wind,Ozone,main = 'Ozone and Wind in New York City'))

with(airquality,plot(Wind,Ozone,type = 'n'))
with(airquality[airquality$Month==5,],points(Wind,Month,col='blue'))
with(airquality[airquality$Month!=5,],points(Wind,Month,col='red'))
legend('topright',col = c('blue','red'),legend = c('may','other banks'))

with(airquality,plot(Wind,Ozone,pch=20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow=c(1,2))
with(airquality,{
  plot(Wind,Ozone,main = 'Ozone and Wind ')
  plot(Solar.R,Ozone,main = 'Solar and Ozone')
})
#end

par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(airquality,{
  plot(Wind,Ozone,main='wind and ozone')
  plot(Solar.R,Ozone,main='Solar and Ozone')
  plot(Temp,Ozone,main = 'temp and ozone')
  mtext('ozone and weather',outer = T)
})
