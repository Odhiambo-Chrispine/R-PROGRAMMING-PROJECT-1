#loading data into R
data=read.csv("E:/PROJECT/DATA.csv")
View(data)
#attach enable R to identify the variables
attach(data)

#define the colours and ageGroup
colours=c('red','orange','yellow','blue','green','purple','white')
AgeGroup=c('15-19','20-24','25-29','30-34','35-39','40-44','45-49')

#import the library that will enable us plot 3D pie chart
library(plotrix)

#define the percentage family planning uptake in 2022
Percentage_users2=data[c(365:371),7]
Percentage_users2

#simple pie chart
pie(Percentage_users2,labels = paste0(Percentage_users2, "%"),
    col = colours,main="number of users of family planning in Kenya 2022")
#legend is the key
legend('topright',AgeGroup,cex = 0.75,fill = colours)

#3D pie chart
pie3D(Percentage_users2,labels = paste0(Percentage_users2, "%"),
      explode = 0.1,col = colours, 
      main="family planning uptake in Kenya 2022")
legend('topright',AgeGroup,cex=0.8,fill = colours)

#######comparing the number of users from 1970-1973 and from 2019-2022##########

# load library plotly
library(plotly)

#defining data of years
year_2022=data[c(365:371),8]
year_2021=data[c(358:364),8]
year_2020=data[c(351:357),8]
year_2019=data[c(344:350),8]

year_1973=data[c(23:29),8]
year_1972=data[c(16:22),8]
year_1971=data[c(9:15),8]
year_1970=data[c(2:8),8]

# create sample data frame for 1970 and 1971
sample_data1 <- data.frame(group= c('15-19','20-24','25-29','30-34','35-39',
                                    '40-44','45-49'),year_1970,year_1971)
sample_data1

# create pie chart for the sample data frame
plot_ly(sample_data1) %>%
  add_pie(labels = ~`group`, values = ~`year_1970`,
          type = 'pie', hole = 0.7, sort = F,
          marker = list(line = list(width = 2))) %>%
  add_pie(sample_data1, labels = ~`group`, values = ~`year_1971`,
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85)),
          sort = F)%>%
  layout(title = "family planning uptake in 1970 and 1971")

# create sample data frame for 1972 and 1973
sample_data2 <- data.frame(group= c('15-19','20-24','25-29','30-34','35-39',
                                    '40-44','45-49'),year_1972,year_1973)
sample_data2

# create pie chart for the data frame
plot_ly(sample_data2) %>%
  add_pie(labels = ~`group`, values = ~`year_1972`,
          type = 'pie', hole = 0.7, sort = F,
          marker = list(line = list(width = 2))) %>%
  add_pie(sample_data2, labels = ~`group`, values = ~`year_1973`,
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85)),
          sort = F)%>%
  layout(title = "pie chart comparison on usage in 1972 and 1973")

# create sample data frame for 2019 and 2020
sample_data3 <- data.frame(group= c('15-19','20-24','25-29','30-34','35-39',
                                    '40-44','45-49'),year_2019,year_2020)
sample_data3

# create pie chart for data frame
plot_ly(sample_data3) %>%
  add_pie(labels = ~`group`, values = ~`year_2019`,
          type = 'pie', hole = 0.7, sort = F,
          marker = list(line = list(width = 2))) %>%
  add_pie(sample_data3, labels = ~`group`, values = ~`year_2020`,
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85)),
          sort = F)%>%
  layout(title = "pie chart comparison on low rate of usage in 2019 and 2020")


# create sample data frame for 2021 and 2022
sample_data4 <- data.frame(group= c('15-19','20-24','25-29','30-34','35-39',
                                   '40-44','45-49'),year_2021,year_2022)
sample_data4

# create pie chart data frame
plot_ly(sample_data4) %>%
  add_pie(labels = ~`group`, values = ~`year_2021`,
          type = 'pie', hole = 0.7, sort = F,
          marker = list(line = list(width = 2))) %>%
  add_pie(sample_data4, labels = ~`group`, values = ~`year_2022`,
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85)),
          sort = F)%>%
  layout(title = "low rate of uptake in 2021 and 2022")





