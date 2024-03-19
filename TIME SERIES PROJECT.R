usd=read.csv("E:/time data.csv")
View(usd)
attach(usd)

library(forecast)
library(tseries)
library(timeSeries)
library(xts)
library(timeDate)
library(ggplot2)

#plotting two variables x and y (TIME SERIES)
ggplot(usd,aes(x=year,y=Total.Trade.Quantity))+
  geom_line(color="green",size=1,alpha=0.8,linetype=1)+
  scale_x_continuous(limits=c(1924,2022),breaks = scales::breaks_width(20))+ #giving range of years with interval of 20 years
  theme_classic(base_family = "serif")+ #removing grid lines
  labs(title = "Anlaysis of Sales from 1924-2022",y="Total Trade Quantity",x="Year")+
  theme(plot.title = element_text(color = "blue",size = 24,hjust = 0.5,face = "plain",margin=margin(10,10,10)),
        axis.text.x = element_text(color = "purple",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.text.y = element_text(color = "brown",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.title.x = element_text(color = "grey20",size = 24,angle = 0,hjust = 0.5,vjust = 0,face = "bold"),
        axis.title.y = element_text(color = "red",size = 24,angle = 90,hjust = 0.5,vjust = 0.5,face = "bold"))

#plotting multiple variables
ggplot(usd,aes(x=year,y=Total.Trade.Quantity,group=City))+
  geom_line(aes(color=City),size=2,alpha=1.8,linetype=1)+
  scale_linetype_manual(values = c("solid","solid","solid","solid"))+
  scale_color_manual(values = c("red","green","orange","blue"))+
  scale_size_manual(values = c(1,1.5,1,1))+
  theme_classic(base_family = "serif")+
  labs(title = "Anlaysis of Sales from 1924-2022",y="Total Trade Quantity",x="Year")+
  theme(legend.position = "top",
        plot.title = element_text(color = "blue",size = 24,hjust = 0.5,face = "plain",margin=margin(10,10,10)),
        axis.text.x = element_text(color = "purple",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.text.y = element_text(color = "brown",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.title.x = element_text(color = "grey20",size = 24,angle = 0,hjust = 0.5,vjust = 0,face = "bold"),
        axis.title.y = element_text(color = "red",size = 24,angle = 90,hjust = 0.5,vjust = 0.5,face = "bold"))+
  theme(legend.position = c(0.30,0.90),
        legend.justification = c("left","top"),
        legend.box.just = "right",
        legend.margin = margin(2,2,2,2))+
  scale_x_continuous(limits = c(1920,2022),breaks = scales::breaks_width(20))+
  theme(legend.title = element_blank())

#LINEAR REGRESSION
x=year
y=High

relate=lm(y~x)
print(summary(relate))

#predicting the sales in 2023 
prediction=data.frame(x=2023)
result=predict(relate,prediction)
print(result)

#plotting linear regression using ggplot
ggplot(usd,aes(x=year,y=High))+
  geom_point(color="blue",size=3,alpha=0.5)+
  geom_smooth(method=lm,color="green",se=F)+
  theme_classic(base_family = "serif")+
  labs(title = "Linear Regression for Highest value(product) against year",y="Highest value(product)",x="Year")+
  theme(legend.position = "top",
        plot.title = element_text(color = "blue",size = 24,hjust = 0.5,face = "plain",margin=margin(10,10,10)),
        axis.text.x = element_text(color = "purple",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.text.y = element_text(color = "brown",size = 24,angle = 0,hjust = 0.5,vjust = 0.5,face = "bold"),
        axis.title.x = element_text(color = "grey20",size = 24,angle = 0,hjust = 0.5,vjust = 0,face = "bold"),
        axis.title.y = element_text(color = "red",size = 24,angle = 90,hjust = 0.5,vjust = 0.5,face = "bold"))

#MULTIPLE REGRESSION
y=Total.Trade.Quantity
x1=Open
x2=High
x3=Low
x4=Last
x5=Close

relation=lm(y~x1+x2+x3+x4+x5)
print(summary(relation))

#ARIMA MODELS
tsdata=ts(High)
print(tsdata)

adf.test(tsdata)

tsdataModel=auto.arima(tsdata,ic="aic",trace = TRUE)
tsdataModel

acf(tsdataModel$residuals,col="green")
pacf(tsdataModel$residuals,col="red")

forecast=forecast(tsdataModel,level=95,h=100*1)
forecast

plot(forecast,level=95)

#########################################################################################################

#packages for PLOTTING MAPS
library(tidyverse)
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggrepel)
library(ggplot2)
library(cowplot)

vaccine <- read.csv("E:/WHO-COVID-19-global-data.csv")
view(vaccine)

mapdata=map_data("world")
view(mapdata)
mapdata=left_join(mapdata,vaccine,by="region")
view(mapdata)

mapdata1=mapdata%>%filter(!is.na(mapdata$order))
view(mapdata1)

map1=ggplot(mapdata1,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=order),color="black")
map1

map2=map1+scale_fill_gradient(name="% vaccinated",low="yellow",high="red",na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map2

