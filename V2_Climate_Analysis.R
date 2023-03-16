####### Climate Analysis in R #######


## Dataset: Berkeley Earth
## Link for the documentation: http://berkeleyearth.org/data


## Loading the necessary libraries:
  ## install.packages("readr")
  ## install.packages("data.table")
  ## install.packages("dplyr")
  ## install.packages("ggplot2")
  ## install.packages('hrbrthemes')
  library(viridis)
  library(hrbrthemes)
  library(plotly)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(data.table)
  library(tibble)
  library(RMySQL)

## Creating connection with MySQL
con = dbConnect(
  MySQL(), 
  user = "root", 
  password = "Caiodwc2135", 
  dbname = "climatedb", 
  host = "localhost")



## For this particular project, I just looked into North American temperatures

## Therefore the Country column is no longer necessary, because it would be all 'United States'. Also Latitude and Longitude.

qry <- "
  select 
    dt, 
    AverageTemperature,
    AverageTemperatureUncertainty,
    City 
  from 
    globalclimate
  where
    Country = 'United States';
"

US_Climate <- dbGetQuery(con, qry)


#### Preparating and Organizating Data ####

## Converting Data and creating Month and Year column
US_Climate$dt <- as.POSIXct(US_Climate$dt, format = "%Y-%m-%d")
US_Climate$Month <- month(US_Climate$dt)
US_Climate$Year <- year(US_Climate$dt)

## Percentage of null values
sum(is.na(US_Climate)) / nrow(US_Climate)

## We have 8% of the data as "na", so I chose to just delete them
US_Climate <- na.omit(US_Climate)
sum(is.na(US_Climate)) / nrow(US_Climate)

#### Analysis:
# When looking at the overall temperature graph, we notice that it assumes what we call the "Left Skewed Distribution", as temperatures tend to be greater than 0


## Selecting the capital of the states
Capital_Cities <- 
  US_Climate[US_Climate$City %in% 
               c('Montgomery','Juneau','Phoenix','Little Rock', 'Sacramento',
                 'Denver','Hartford', 'Dover','Tallahassee','Atlanta',
                 'Honolulu', 'Boise','Springfield', 'Indianapolis', 
                 'Des Moines', 'Topeka', 'Frankfort', 'Baton Rouge', 'Augusta',
                 'Annapolis', 'Boston','Lansing','Saint Paul', 'Jackson',
                 'Jefferson City', 'Helena','Lincoln','Carson City','Concord',
                 'Trenton','Santa Fe','Albany','Raleigh','Bismarck','Columbus',
                 'Oklahoma City','Salem','Harrisburg','Providence','Columbia',
                 'Pierre','Nashville','Austin','Salt Lake City','Montpelier',
                 'Richmond','Olympia','Charleston','Madison','Cheyenne'),]


## Analyze the two datasets 
summary(US_Climate)
summary(Capital_Cities)

## As you can see, there is almost no difference between the datasets, so we are going to work with the "Capital_Cities" in order to better look to data

## Another way to confirm that is to look to a histogram
par(mfrow=c(1,2))
hist(
  US_Climate$AverageTemperature, 
  main = "Average Temperature: USA", 
  xlab = "Average Temperature"
)

hist(
  Capital_Cities$AverageTemperature, 
  main = "Average Temperature: Main Cities", 
  xlab = "Average Temperature"
)
par(mfrow=c(1,1))
## The data distribution is similar


## Last 05 year analysis

Capital_Cities$dt <- as.Date(Capital_Cities$dt)

ggplot(Capital_Cities, 
            aes(x=dt, 
                y=AverageTemperature,
                color = City,
                group = City)) +
  geom_line() + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1)) +
  scale_x_date(
    limit = c(
      max(Capital_Cities$dt)-(365*5),
      max(Capital_Cities$dt))) +
  ggtitle('Last 05 Years (Capital Cities)')

#### Analysis:
## It seems that the temperature completes its cycle every year where it returns to following the pattern, where the first semester is warmer and the second semester starts to cool.


## First 10 years (I choose the first 10, because of the data leap between the 2nd bimester of 1745 to 1750)

ggplot(Capital_Cities, 
            aes(x=dt, 
                y=AverageTemperature,
                color = City,
                group = City)) +
  geom_line() + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1)) +
  scale_x_date(
    limit = c(
      min(Capital_Cities$dt),
      min(Capital_Cities$dt)+(365*10))) +
  ggtitle('First 10 Years (Capital Cities)')

#### Analysis:
## The pattern remains the same, content, we perceive the difference in the amplitude ends between the analyzed periods


## In order to check growth over time, let's do an annual analysis

Capital_Cities_Year <- Capital_Cities[, c(2,4,6)]%>%
  group_by(Year, City) %>%
  summarise(Avgtemp = mean(AverageTemperature))

Capital_Cities_Year %>%
  ggplot( aes(x=Year, y=Avgtemp, group=City, color=City)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Anual Temperature (Capital Cities)") +
  theme_ipsum()

## As the years go by, the thermal amplitude decreases, as the temperature gets warmer too

range(Capital_Cities_Year$Avgtemp[Capital_Cities_Year$Year < 1760])
range(Capital_Cities_Year$Avgtemp[Capital_Cities_Year$Year > 1990])

mean(Capital_Cities_Year$Avgtemp[Capital_Cities_Year$Year < 1760])
mean(Capital_Cities_Year$Avgtemp[Capital_Cities_Year$Year > 1990])

## Despite the thermal amplitude not having changed so much, it is possible to conclude that in recent times the average temperatures have been rising, with a difference of approximately 3.5º

## Now, let's compare the results with some Texas cities
Texas_Cities <- c('Austin','Dallas', 'Fort Worth', 'Houston', 'San Antonio')

par(mfrow=c(3,2))
for (cities in Texas_Cities) {
  hist(
    US_Climate$AverageTemperature[US_Climate$City == cities], 
    main = paste("Average Temperature: ", cities),
    xlab = "Average Temperature"
  )
}
par(mfrow=c(1,1))

## create a dataframe for texas cities to analyze dates

Texas_Cities_df <-  US_Climate[US_Climate$City %in% Texas_Cities,]
Texas_Cities_df$dt <- as.Date(Texas_Cities_df$dt)

## Last 05 year analysis
ggplot(Texas_Cities_df, 
       aes(x=dt, 
           y=AverageTemperature,
           color = City,
           group = City)) +
  geom_line() + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1)) +
  scale_x_date(
    limit = c(
      max(Texas_Cities_df$dt)-(365*5),
      max(Texas_Cities_df$dt)))+
  ggtitle('Last 05 Years (Texas Cities)')


## First 05 year analysis
ggplot(Texas_Cities_df, 
       aes(x=dt, 
           y=AverageTemperature,
           color = City,
           group = City)) +
  geom_line() + 
  geom_point() +
  xlab("") +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1)) +
  scale_x_date(
    limit = c(
      min(Texas_Cities_df$dt),
      min(Texas_Cities_df$dt)+(365*5))) +
  ggtitle('First 10 Years (Texas Cities)')


## Statistics

for (city in Texas_Cities) {
  print(city)
  print(
    summary(Texas_Cities_df$AverageTemperature[Texas_Cities_df$City == city]))
}





#### Analysis:
## As we have seen, the state of Texas continues to increase temperatures over the years, as well as having an average temperature higher than the average of the country by about 5º to 7º



## Disclaimer: a good part of this project was largely done in the Data Science Academy, Big Data Analytics with R and Microsoft Azure Machine Learning course (part of the Data Scientist training)