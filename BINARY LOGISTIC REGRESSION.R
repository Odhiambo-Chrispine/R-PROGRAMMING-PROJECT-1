#loading data set into R for analysis
data <- read.csv("E:/PROJECT/DATA B.csv")
View(data)
attach(data)

########################## BINARY LOGISTIC REGRESSION #########################

#tabulating the number of both age groups
#first 20 years from 1970-1990
AgeGroup=data[c(1:42),5]
AgeGroup

#last 20 years from 2002-2022
AgeGroup2=data[c(65:106),5]
AgeGroup2

table=data.frame(AgeGroup,AgeGroup2)
table

#training the data
# in this case,we let;
#     -> percentage of users to be x, 'where percentage is percentage of
#        uptake from 1970-1990 while percentage2 is uptake from 2002-2022'
#     -> ageCode of users to be y, 'where ageCode is ageGroup
#        from 1970-1990 while ageGroup2 is ageGroup from 2002-2022'

#converting age groups into binary numbers
ageCode=ifelse(AgeGroup=='15-49',1,0)
ageCode

ageCode2=ifelse(AgeGroup=='15-49',1,0)
ageCode2

Percentage=data[c(1:42),6]
Percentage

Percentage2=data[c(65:106),6]
Percentage2

#plotting the percentage of uptake against age group 
#   ->pch is the size of the dots  

plot(Percentage,ageCode,pch=20,col="blue",
     xlab = "Percentage of family planning uptake",
     ylab="AgeGroup(0='15-19',1='15-49')",
     main="family planning uptake in Kenya analyzed from 1970-1990")

plot(Percentage2,ageCode2,pch=20,col="blue",
     xlab = "Percentage of family planning uptake",
     ylab="AgeGroup(0='15-19',1='15-49')",
     main="family planning uptake in Kenya analyzed from 2002-2022")

#building the binary logistic model
#   -> if the probability of obtaining test statistic Pr(>|z|) is greater 
#      than 0.5 at both intercept & slope, then both variables have got 
#      significance impact on each other and accept the H0 hence it is not
#      statistically significant
model=glm(ageCode~Percentage,family=binomial(link = "logit"))
summary(model)

BIC(model)

model2=glm(ageCode~Percentage2,binomial(link = "logit"))
summary(model2)

BIC(model2)

#including the sigmoid curve in the final plot
#    ->'response type' represent the odd ratio, which must also be included 
xv=seq(min(Percentage),max(Percentage),by=0.5)
yv=predict(model,list(Percentage=xv),type = 'response')
lines(xv,yv,col="green")

xv2=seq(min(Percentage2),max(Percentage2),by=0.5)
yv2=predict(model,list(Percentage=xv2),type = 'response')
lines(xv2,yv2,col="green")

#introducing histogram of family planning uptake into the sigmoid curve 
library(popbio)
#using the function of logistic histogram plot
logi.hist.plot(Percentage,ageCode,boxp = FALSE,type = 'count',col="orange",
               xlabel ="percentage of family planning uptake",
               main="combined histogram and sigmoid curve for family planning
               uptake in Kenya analyzed from 1970-1990")

logi.hist.plot(Percentage2,ageCode,boxp = FALSE,type = 'count',col="orange",
               xlabel ="percentage of family planning uptake",
               main="combined histogram and sigmoid curve for family planning
               uptake in Kenya analyzed from 2002-2022")



