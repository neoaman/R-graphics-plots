library(MASS)
library(tibble)
library(tidyr)  # drop na
library(ggplot2)

Cars <- drop_na(Cars93)
data_type <- data.frame(sapply(Cars,class));


#1.0.0 Histogram________________________________________________________________________________________________
#1.1.1 simple Histogram

hist(x = Cars$Price)

hist(x = Cars$Price,main = "No of Cars and  their Price",
     xlab = "Price",ylab = "No. of cars",sub="Cars Price and their quantity",
     col = "lightblue", col.lab = "blue",col.main = "red",col.sub = "red",col.axis="green")

#1.1.2 simple Histogram with density curve

hist(x = Cars$Price,main = "No of Cars and  their Price",probability = T,
     xlab = "Price",ylab = "No. of cars",sub="Cars Price and their quantity",
     col = "lightblue", col.lab = "blue",col.main = "red",col.axis="green",col.sub = "red")
lines(density(Cars$Price),col="red",lty=3,lwd=3)

#1.2.1 ggplot histogram 

ggplot(data = Cars,aes(x = Cars$Price))+geom_histogram()

ggplot(data = Cars,aes(x = Cars$Price))+
  geom_histogram(binwidth = 6.2,color="blue",fill="lightblue")+
  theme(axis.title = element_text(colour = "blue"),
        title =  element_text(colour = "red"),
        axis.ticks = element_line(colour = "green"),
        axis.text = element_text(colour = "green"),
        axis.line = element_line(colour = "forestgreen"),
        panel.background = element_rect(colour = "blue",fill = "white")
  )+
  labs(x = "Price",y="No. of cars",subtitle = "Cars Price and their quantity",title = "No of Cars and  their Price")
  
#1.2.2 ggplot histogram with density plot

ggplot(data = Cars,aes(x = Cars$Price,y = ..density..))+
  geom_histogram(binwidth = 6.2,color="blue",fill="lightblue")+
  theme(axis.title = element_text(colour = "blue"),
        title =  element_text(colour = "red"),
        axis.ticks = element_line(colour = "green"),
        axis.text = element_text(colour = "green"),
        axis.line = element_line(colour = "forestgreen"),
        panel.background = element_rect(colour = "blue",fill = "white")
        )+
  labs(x = "Price",y="No. of cars",subtitle = "Cars Price and their quantity",title = "No of Cars and  their Price")+
  geom_density(aes(y=..density..),colour="red",lty=3,lwd=1.2)

#2.0.0 Bar Plot_____________________________________________________________________________________________________
#2.1.1 simple Bar plot
table(Cars$Type)
data <- t(table(Cars$Type,Cars$Origin))
barplot(height = data)

barplot(height = table(Cars$Type))

barplot(height = data,legend.text = rownames(data),col = c("lightblue","forestgreen"))

barplot(height = data,col = c("lightblue","forestgreen"))
legend(x="topr",legend = rownames(data),fill = c("lightblue","forestgreen"),title = "Car Origin",bg = "gray")

#2.1.2 simple bar plot with dodge position

barplot(height = data,col = c("lightblue","forestgreen"),beside = T)
legend(x="topr",legend = rownames(data),fill = c("lightblue","forestgreen"),title = "Car Origin",bg = "white",bty = "n") #remove legend box using bty="n"

#2.2.1 ggplot bar plot

ggplot(data = Cars,aes(x = Cars$Type))+geom_bar()

ggplot(data = Cars,aes(x = Cars$Type,fill=Cars$Origin))+geom_bar()

ggplot(data = Cars,aes(x = Cars$Type,fill=Cars$Origin))+geom_bar(colour="black")+
  scale_fill_manual("Car Origin",values = c("non-USA"="forestgreen","USA"="lightblue"))

#2.2.2 ggplot bar plot with dodge position

ggplot(data = Cars,aes(x = Cars$Type,fill=Cars$Origin))+geom_bar(colour="black",position = "dodge")+
  scale_fill_manual("Car Origin",values = c("non-USA"="forestgreen","USA"="lightblue"))

#3.0.0 piechart______________________________________________________________________________________________
#3.1.1 simple pie chart
Type <- table(Cars$Type)[table(Cars$Type)!=0]
pie(x = Type)

pie(x = Type,labels = rownames(Type),col = c("lightblue","forestgreen","yellow","orange","blue")
    ,clockwise = T,init.angle = 0,radius = 1,border = "forestgreen") #init.angle=0 means 3 o' clock
legend(x = "topr",legend = rownames(Type),fill = c("lightblue","forestgreen","yellow","orange","blue"))

#3.2.1 ggplot pie chart

data <- data.frame(table(Cars$Type))
colnames(data) <- c("Type","Frequency")
ggplot(data = data,aes(x ="",y = Frequency,fill=Type ))+geom_bar(stat = "identity")+ coord_polar("y",start=0)+
  theme(axis.text   = element_blank())+
  scale_fill_manual("Type",values = c("Compact"="lightblue","Large"="forestgreen","Midsize" = "yellow","Small" = "orange","Sporty"="blue","Van"="black"))
   
#4.0.0 Line______________________________________________________________________________________________
#4.1.1 simple function plotting

curve(expr = log,from = 0,to = 10,n = 30,type = "b")
curve(expr = exp,from = 0,to = 10,n = 30,type = "b")

X <- seq(0,10,length.out = 30)

plot(x=X,y = log(X),type="b")
plot(x=X,y = exp(X),type = "b")

X <- seq(0,10,length.out = 30)
Y <- log(X)
data <- data.frame(X,Y)
ggplot(data = data,aes(x = X))+geom_line(aes(y = Y))+geom_point(aes(y = Y))

ggplot()

#4.2.1 multiple lines in one plot
X <- seq(0,10,length.out = 30)
plot(x = X,y = sin(X),type = "b",col="green")
lines(x = X,y=cos(X),col="blue",type = "b")

curve(expr = sin,from = 0,to = 10,n = 30,type = "b",col="green")
lines(x = X,y=cos(X),col="blue",type = "b")


y1 <- sin(X)
y2 <- cos(X)
data <- data.frame(X,y1,y2)
ggplot(data = data,aes(X))+geom_line(aes(y=y1),colour="green")+geom_line(aes(y=y2),colour="blue")+xlab("X")+
  geom_point(aes(y=y1),colour="green")+geom_point(aes(y=y2),colour="blue")

#4.3.1 side by side plots

par(mfrow=c(2,2))

curve(expr = log,from = 0,to = 10,n = 30,type = "b")
curve(expr = exp,from = 0,to = 10,n = 30,type = "b")
plot(x = X,y = sin(X),type = "b",col="green")
lines(x = X,y=cos(X),col="blue",type = "b")

curve(expr = sin,from = 0,to = 10,n = 30,type = "b",col="green")
lines(x = X,y=cos(X),col="blue",type = "b")



#5.0.0 Scatter Plot__________________________________________________________________________________________________
#head(Cars,+3)
#Cars$Price,Cars$EngineSize
# 5.1.1 Simple scatter plot
plot(x = Cars$EngineSize,y = Cars$Price,col="blue")
abline(lm(Cars$Price~Cars$EngineSize),col="forestgreen",lwd=2)

plot(x = Cars$EngineSize,y = Cars$Price,col="blue",xlab = "Price",ylab = "Engine size")
abline(lm(Cars$Price~Cars$EngineSize),col="forestgreen",lwd=2)
#5.2.0 ggplot scatter plot
#5.2.1
ggplot(data = Cars,aes(x = EngineSize,y = Price))+geom_point()+geom_smooth(method = lm)
#5.2.2
ggplot(data = Cars,aes(x = EngineSize,y = Price))+geom_point()+geom_smooth(method = lm,se=F)
#5.2.3
ggplot(data = Cars,aes(x = EngineSize,y = Price))+geom_point()+geom_smooth()
#5.2.4
ggplot(data = Cars,aes(x = EngineSize,y = Price))+geom_point()+geom_smooth(method = lm,linetype="dashed",colour="forestgreen",fill="yellow")
#5.3.1 multiple Origin show
ggplot(data = Cars,aes(x = EngineSize,y = Price,colour=Origin))+geom_point()
#5.3.2 multiple Origin lines
ggplot(data = Cars,aes(x = EngineSize,y = Price,colour=Origin))+geom_point()+geom_smooth(method = lm)

ggplot(data = Cars,aes(x = EngineSize,y = Price,colour=Origin))+geom_point()+geom_density_2d()

#6.0.2 Multiple plots in one Place___________________________________________

scatterplot <-ggplot(data = Cars,aes(x = EngineSize,y = Price,colour=Origin))+geom_point()+geom_density_2d()+
  theme(legend.position = "none")


blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

# Marginal density plot of x (top panel)
xdensity <- ggplot(Cars, aes(x=EngineSize)) + 
  geom_density(alpha=.5,fill="green") + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(Cars, aes(x=Price)) + 
  geom_density(alpha=.5,fill="orange") + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
ydensity

library("gridExtra")
grid.arrange(xdensity, blankPlot, scatterplot, ydensity,
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))



