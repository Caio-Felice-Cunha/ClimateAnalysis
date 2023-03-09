####### Climate Analysis in R #######


## Dataset: Berkeley Earth
## Link for the documentation: http://berkeleyearth.org/data

update.packages(ask = FALSE, checkBuilt = TRUE)

tinytex::tlmgr_update()

tinytex::reinstall_tinytex()

options(tinytex.verbose = TRUE)

install.packages("tinytex")

tinytex::check_installed("framed")

install_tinytex(
  force = FALSE,
  dir = "auto",
  version = "daily",
  bundle = "TinyTeX-1",
  repository = "auto",
  extra_packages = if (is_tinytex()) tl_pkgs(),
  add_path = TRUE
)

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



## Disclaimer: a good part of this project was largely done in the Data Science Academy, Big Data Analytics with R and Microsoft Azure Machine Learning course (part of the Data Scientist training)