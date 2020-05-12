# Udacity Exercise - Bikeshare

## load lubridate package
install.packages("lubridate")

# load libraries (essential libraries)
require(ggplot2)
require(lubridate)
require(RColorBrewer)

## Load the data of our 3 files for each city into workspace
wash <- read.csv("C:/Users/freecomp/Desktop/R/washington.csv", header = TRUE)
ny <- read.csv("C:/Users/freecomp/Desktop/R/new-york-city.csv", header = TRUE)
chi <- read.csv("C:/Users/freecomp/Desktop/R/chicago.csv", header = TRUE)

# get sense of the data by exploring first rows using head function
head(wash)
head(ny)  # include gender and birth year column
head(chic) # include gender and birth year column

# add a NA data for washington data so that all cites have same no.of variables
wash[,c("Gender","Birth.Year")] <- NA

names(wash) # to confirm the new columns are added

# Add city column for each table (total of 10 variables:original 9 +City column)
wash[,"City"] <- "Washington"
ny[,"City"] <- "New York"
chi[,"City"] <- "Chicago"

# New data with all three cities
wnc <- rbind(wash, ny, chi)


### Question 1
# Extract the hour from our data
dates <- as.POSIXlt(wnc$Start.Time)
hours <- as.data.frame(hour(dates)) # extracting hour
wnc2 <- wnc
wnc2[,"Hour"] <- hours # Add new column hours

# Plot the hour of the day when a bike is rented in each day
ggplot(aes(x= Hour),data = wnc2)+
  geom_histogram(binwidth = 1, col = 'light blue')+
  ggtitle('Histogram of Days Hours a bike is rented in \n Newyork, Washinghton and Chicago')+
  ylab('Count of Bikes Rented')+
  scale_x_continuous(limits = c(0,24),breaks= seq(0,24,2))
  # 8-9 is the hour when mosrt commonly a bike is hired

# Descriptive statistics:
summary(wnc2$Hour)
# Numeric variable by using sort function on a table
sort(table(wnc2$Hour),decreasing = TRUE) # 81734 a bike is rented in all 3 cities

# Plot the hour of the day when a bike is rented in each day for each city
ggplot(aes(x= Hour),data = wnc2)+
  geom_histogram(binwidth = 1, col = 'light blue')+
  ggtitle('Histogram of Days Hours a bike is rented in \n Newyork, Washinghton and Chicago')+
scale_x_continuous(limits = c(0,24),breaks= seq(0,24,2))+
facet_wrap(~ City)
# variation in most common hour across 3 cities: 8 in Washigton and 17 in chicago and NY

#Desciptive statistics
by(wnc2$Hour, wnc$City, summary)


### Question 2: Most common month a bike is rented
m <- as.data.frame(month(dates)) # extracting the day
wnc2[,"Month"] <- m # Add new column for month

# Plot the month when a bike is rented in first 6 months of 2017
ggplot(aes(x= Month),data = wnc2)+
  geom_histogram(binwidth = 1, fill = brewer.pal(7, 'Blues'), col = 'black')+
  ggtitle('Histogram of Months a bike is rented in 2017 \n in NewYork, Washinghton and Chicago')+
  ylab('Count of Bikes Rented')+
  scale_x_continuous(limits = c(0,6),breaks= seq(0,6,1))

# Plot the month when a bike is rented in first 6 months of 2017
ggplot(aes(x= Month),data = wnc2)+
  geom_histogram(binwidth = 1, fill = brewer.pal(7, 'Blues'), col = 'black')+
  ggtitle('Histogram of Months a bike is rented in 2017 \n in NewYork, Washinghton and Chicago')+
  ylab('Count of bikes Rented')+
  scale_x_continuous(limits = c(0,6),breaks= seq(0,6,1)) # month 5(May): is the most common month

# same plot for each city
# Plot the month when a bike is rented in first 6 months of 2017
ggplot(aes(x= Month),data = wnc2)+
  geom_histogram(binwidth = 1, fill = "light green", col = 'black')+
  ggtitle('Histogram of Months a bike is rented in 2017 \n in NewYork, Washinghton and Chicago')+
  ylab('Count of bikes Rented')+
  scale_x_continuous(limits = c(0,6),breaks= seq(0,6,1))+
  facet_grid(.~ City) # May is most common in chicago and NY while April is most common in Washington


### Question 3 : Most common start station
sort(table(wnc$Start.Station),decreasing = TRUE)[1]
# we convert the variable of start station of all 3 cites into a table and sort it according to
# the most repeated value which is Streeter Dr & Grand Ave :6911 times it is the starting station


### Question 4:average travel time for users in different cities
by(wnc$Trip.Duration/60, wnc$City, mean)
# divide by 60 so that result is in minute: Washington:20.6 min, NY:14.99, Chicago:15.6


### Question 5: What are the counts of each user type
summary(wnc$User.Type)


#### Question 6: What are the counts of each gender
by(wnc$Gender, wnc$City, summary)
# OR
summary(ny$Gender) # for New york City
summary(chi$Gender) # for Chicago city
