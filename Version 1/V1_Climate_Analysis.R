####### Climate Analysis in R #######


## Dataset: Berkeley Earth
## Link for the documentation: http://berkeleyearth.org/data


## Loading the necessary libraries:
  ## install.packages("data.table")
  ## install.packages("dplyr")
  ## install.packages("ggplot2")
  library(dplyr)
  library(ggplot2)
  library(data.table)

## Loading the file from the working directory.
## Place GlobalClimate.csv (the Berkeley Earth by-city CSV) in the same folder,
## or set the path below. 'fread' is faster and lighter for a file this size.
df <- fread("GlobalClimate.csv")

## For this particular project, I just looked into North American temperatures

## Therefore the Country column is no longer necessary, because it would be all 'United States'. Also Latitude and Longitude.

US_Climate <- subset(df, Country == "United States", select = c(dt, AverageTemperature, AverageTemperatureUncertainty, City))

## Fraction of rows that contain at least one missing value
## (these are the rows that na.omit will drop). About 7% of rows.
mean(!complete.cases(US_Climate))

## We drop those incomplete rows
US_Climate <- na.omit(US_Climate)
mean(!complete.cases(US_Climate))

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
## Group by year to make a lighter plot with fewer points.
## Group by year and take the mean temperature, using Dallas as a test city.

dallas_df <- US_Climate[US_Climate$City == 'Dallas']

dallas_df2 <- dallas_df[,c(2,6)] %>%
  group_by(Year) %>%
  summarise(Avgtemp = mean(AverageTemperature))

ggplot(dallas_df2, aes( x = Year, y = Avgtemp)) +
  geom_area()

summary(dallas_df2)

###################################################################
## Stacked, interactive area chart of all US cities from 2001 onward.
library(viridis)
## install.packages('hrbrthemes')
library(hrbrthemes)
library(plotly)

## This keeps every US city. To restrict to specific cities, filter on City,
## for example: US_Climate[US_Climate$City %in% c('Dallas','Houston','Austin'),]
the_cities_df <- US_Climate

the_cities_df2 <- the_cities_df[the_cities_df$Year > 2000]

the_cities_df3 <- the_cities_df2[, c(2,4,6)]%>%
  group_by(Year, City) %>%
  summarise(Avgtemp = mean(AverageTemperature))

plot(x = the_cities_df3$Year, y = the_cities_df3$Avgtemp)

# Plot
p <- the_cities_df3 %>%
  ggplot( aes(x=Year, y=Avgtemp, fill=City, text=City)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Average annual temperature by US city (2001+)") +
  theme_ipsum() +
  theme(legend.position="none")

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p

## Disclaimer: a good part of this project was largely done in the Data Science Academy, Big Data Analytics with R and Microsoft Azure Machine Learning course (part of the Data Scientist training)