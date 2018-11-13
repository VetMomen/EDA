?plot.default
#One Variable
#Continuous

library(ggplot2)
data(mpg)

density_obj<-density(mpg$hwy)
par(mfrow=c(1,1))
plot(density_obj)

hist(mpg$hwy,breaks = 10)

#Discrete

tabdat<-table(mpg$drv)
barplot(tabdat)
plot(tabdat)

#Two Variables
#Continuous X, Continuous Y

plot(mpg$cty,mpg$hwy)

with(mpg,{
  x<-jitter(cty)
  y<-jitter(hwy)
  plot(x,y)
  rug(x,side = 1)
  rug(y,side = 2)
})

#Add a Loess Smoother
loess_fit<-lm(data=mpg,hwy~cty)
xdat<-with(mpg,seq(min(cty),max(cty),length.out =100))
ydat<-with(mpg,predict(object = loess_fit,newdata = data.frame(cty=xdat)))
plot(mpg$cty,mpg$hwy)
lines(xdat,ydat)

#Loess smoother with upper and lower 95% confidence bands

loess_fit2<-lm(data = mpg,hwy~cty)
with(mpg,{
  xdat<-(seq(min(cty),max(cty),length.out = 100))
  pfit<-predict(object = loess_fit2,newdata = data.frame(cty=xdat),se=T)
  ydat<-pfit$fit
  upper_limit<-ydat+qnorm(.999)*pfit$se.fit
  lower_limit<-ydat-qnorm(.999)*pfit$se.fit
  plot(cty,hwy)
  lines(xdat,ydat,col='red')
  lines(xdat,upper_limit,lty=5,col='blue')
  lines(xdat,lower_limit,lty=5,col='blue')
})

#Loess smoother with upper and lower 95% confidence bands and that fancy shading from ggplot2.

loess_fit <- loess(hwy ~ cty, data = mpg)
xnew <- seq(min(x), max(x), length = 100)
pfit <- predict(object = loess_fit, newdata = data.frame(cty = xnew), se = TRUE)
ynew <- pfit$fit
upper_bound <- pfit$fit + qnorm(0.975) * pfit$se.fit
lower_bound <- pfit$fit - qnorm(0.975) * pfit$se.fit
xshade <- c(xnew, xnew[length(xnew):1])
yshade <- c(upper_bound, lower_bound[length(lower_bound):1])
plot(x, y)
lines(xnew, ynew, col = 2, lty = 2)
polygon(xshade, yshade, col = "#0000FF33", border = FALSE)

#Add text to a plot

plot(mpg$hwy,mpg$cyl)
text(15,5,'text')

#Discrete X, Continuous Y
#Boxplot

boxplot(data=mpg,hwy~class)

#Discrete X, Discrete Y
#Mosaic Plot
with(mpg,{
  mosaicplot(
    table(fl,drv),
    color = T
  )
})


#Continuous Function
#Line plot

f<-function(x){
  return(x^2)
}
x<-seq(-2,2,length.out = 100)
y<-f(x)

plot(x,y,type = 'l')


#Color Coding and Legends
#Color code a scatterplot by a categorical variable and add a legend.

x <- jitter(mpg$hwy)
y <- jitter(mpg$cty)
z <- factor(mpg$drv)

plot(x,y,col=z)
legend('topleft',legend = levels(z),col=1:length(z),pch=1,title = 'drive')

#Faceting
old_option<-par(mfrow=c(2,3))
plot(mpg$cty, mpg$hwy)
hist(mpg$cty)
plot(density(mpg$cty))
plot(table(mpg$fl))
barplot(table(mpg$fl))
plot(table(mpg$fl, mpg$drv))

