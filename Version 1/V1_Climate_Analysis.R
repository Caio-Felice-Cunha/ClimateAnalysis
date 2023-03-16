####### Climate Analysis in R #######


## Dataset: Berkeley Earth
## Link for the documentation: http://berkeleyearth.org/data


## Loading the necessary libraries:
  ## install.packages("readr")
  ## install.packages("data.table")
  ## install.packages("dplyr")
  ## install.packages("ggplot2")
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(data.table)
  library(tibble)
  library(RMySQL)

## Setting the work directory
setwd("C:\\Users\\Caio\\OneDrive\\Desktop PC\\Desktop\\Portfolio\\Climate")

## Loading the file from the directory
## For this case, the 'fread' is faster and lighter
df <- fread("GlobalClimate.csv")

## For this particular project, I just looked into North American temperatures

## Therefore the Country column is no longer necessary, because it would be all 'United States'. Also Latitude and Longitude.

US_Climate <- subset(df, Country == "United States", select = c(dt, AverageTemperature, AverageTemperatureUncertainty, City))

## Percentage of null values
sum(is.na(US_Climate)) / nrow(US_Climate)

## We have 7% of the data as "na", so I chose to just delete them
US_Climate <- na.omit(US_Climate)
sum(is.na(US_Climate)) / nrow(US_Climate)

str(US_Climate)

#### Preparating and Organizating Data ####

## Converting Data and creating Month and Year column
US_Climate$dt <- as.POSIXct(US_Climate$dt, format = "%Y-%m-%d")
US_Climate$Month <- month(US_Climate$dt)
US_Climate$Year <- year(US_Climate$dt)


## Macro View
hist(
  US_Climate$AverageTemperature, 
  main = "Average Temperature: Macro View", 
  xlab = "Average Temperature"
  )

## Let's see some cities
x <- c('Dallas','Houston', 'Austin', 'New York', 'San Francisco', 'Chicago')

for (n in x) {
  hist(
    US_Climate$AverageTemperature[US_Climate$City == n], 
    main = paste("Average Temperature: ", n),
    xlab = "Average Temperature"
  )
}

###################################################################
###### testando o group by para fazer um plot melhor (com menos dados)
###### no momento eu só agrupei por ano e peguei a media de temperatura
###### usei Dallas de cobaia :D

dallas_df <- US_Climate[US_Climate$City == 'Dallas']

dallas_df2 <- dallas_df[,c(2,6)] %>%
  group_by(Year) %>%
  summarise(Avgtemp = mean(AverageTemperature))

ggplot(dallas_df2, aes( x = Year, y = Avgtemp)) +
  geom_area()

summary(dallas_df2)

View(dallas_df)
View(dallas_df2)

###################################################################
###### tentei stacakr os graficos e deixar interativo
library(viridis)
#install.packages('hrbrthemes')
library(hrbrthemes)
library(plotly)

the_cities_df <-US_Climate#[US_Climate$Country %in% c('Dallas', 'Houston', 'Austin')]

the_cities_df2 <- the_cities_df[the_cities_df$Year > 2000]

the_cities_df3 <- the_cities_df2[, c(2,4,6)]%>%
  group_by(Year, City) %>%
  summarise(Avgtemp = mean(AverageTemperature))

plot(x = the_cities_df3$Year, y = the_cities_df3$Avgtemp)

View(the_cities_df3)

# Plot
p <- the_cities_df3 %>% 
  ggplot( aes(x=Year, y=Avgtemp, fill=City, text=City)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  theme(legend.position="none")

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p

## Disclaimer: a good part of this project was largely done in the Data Science Academy, Big Data Analytics with R and Microsoft Azure Machine Learning course (part of the Data Scientist training)