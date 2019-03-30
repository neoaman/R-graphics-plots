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




