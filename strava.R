# Load libraries
library(ggplot2)
library(dplyr)

## read in the data
activity_data <- read.csv(file='./strava_activity.csv', sep=",")


## Data Exploration ##

# overview of the dataset
str(activity_data)
data_of_interests <- activity_data[,c("athlete.country", "athlete.sex", "average_heartrate", "average_speed", "distance", "elapsed_time", "kilojoules", "location_country", "max_heartrate", "max_speed", "moving_time", "total_elevation_gain")]
View(head(data_of_interests, 20))

# completely_ok obvervation means rows that have no entries missing 
completely_ok <- complete.cases(data_of_interests)
print(paste("Number of completely ok obvervations: ", sum(completely_ok)))


without_heartrate <- subset(data_of_interests, select = -c(average_heartrate, max_heartrate))
print(paste("Number of completely ok obvervations without heartrate data: ", sum(complete.cases(without_heartrate))))

# count the number of males and females
M.num <- nrow(filter(data_of_interests, athlete.sex == 'M'))
F.num <- nrow(filter(data_of_interests, athlete.sex == 'F'))
print(paste("Number of males: ", M.num))
print(paste("Number of females: ", F.num))

# count the number of activities occur/didn't occur in the same country with the athelete's country 
same_country.num <- nrow(filter(data_of_interests, as.character(athlete.country) == as.character(location_country)))
diff_country.num <- nrow(filter(data_of_interests, as.character(athlete.country) != as.character(location_country)))
print(paste("Number of activities occur in the same country with the athelete's country: ", same_country.num))
print(paste("Number of activities occur in the different country from the athelete's origin country: ", diff_country.num))


#Plot distribution of average speed by gender 
ggplot(data_of_interests ,aes(x=average_speed)) + 
  geom_histogram(data=subset(data_of_interests, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.3) +
  geom_histogram(data=subset(data_of_interests, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.3) +
  labs(x = "Average Speed (m/s)", y = "Count", 
       title = "Distribution of Average Speed by Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 500) + xlim(0, 11)

#Plot distribution of max speed by gender 
ggplot(data_of_interests ,aes(x=max_speed)) + 
  geom_histogram(data=subset(data_of_interests, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.3) +
  geom_histogram(data=subset(data_of_interests, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5, binwidth = 0.3) +
  labs(x = "Max Speed (m/s)", y = "Count", 
       title = "Distribution of Max Speed by Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) +
  ylim(0, 200) + xlim(0, 25)

#Plot distribution of distances by gender
ggplot(data_of_interests, aes(x=distance)) + 
  geom_histogram(data=subset(data_of_interests, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  geom_histogram(data=subset(data_of_interests, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Distance (Meters)", y = "Count", 
       title = "Distribution of Distances by Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) + ylim(0, 1000) + 
  xlim(0, 100000)

# moving_ratio is defined as: moving_time / elapsed_time
data_of_interests$moving_ratio <- data_of_interests$moving_time / data_of_interests$elapsed_time

#Plot distribution of kilojoules by gender
ggplot(data_of_interests, aes(x=(kilojoules))) + 
  geom_histogram(data=subset(data_of_interests, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  geom_histogram(data=subset(data_of_interests, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Kilojoules", y = "Count", 
       title = "Distribution of Kilojoules by Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) + ylim(0, 20) + 
  xlim(1500, 6000)

#Plot distribution of moving ratio by gender
ggplot(data_of_interests, aes(x=(moving_ratio))) + 
  geom_histogram(data=subset(data_of_interests, athlete.sex=='F'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  geom_histogram(data=subset(data_of_interests, athlete.sex=='M'), 
                 aes(fill=athlete.sex), alpha=0.5) +
  labs(x = "Moving_Ratios (Moving_time / Elapsed_time)", y = "Count", 
       title = "Distribution of Moving_Ratios by Gender") +
  scale_fill_manual(name="Gender", values=c("red","green"),labels=c("Female","Male")) + ylim(0, 700) + 
  xlim(0, 1)


## Data Preparation ##


# only include attributes of interests
clean_data <- activity_data[,c("athlete.country", "athlete.sex", "average_speed", "distance", "elapsed_time", "kilojoules", "location_country", "max_speed", "moving_time", "total_elevation_gain")]
# remove duplicates and missing values
clean_data <- clean_data[complete.cases(clean_data), ]
clean_data <- unique(clean_data)

# add new aggregated variables
clean_data$moving_ratio <- clean_data$moving_time / clean_data$elapsed_time
clean_data$same_country <- (as.character(clean_data$athlete.country) == as.character(clean_data$location_country))

# encode categorical variables into factors
clean_data$gender.code <- factor(clean_data$athlete.sex)
clean_data$same_country.code <- factor(clean_data$same_country)

str(clean_data)
View(head(clean_data, 20))


## Statistical Modeling ##


# Question 1
gender.manova <- manova(cbind(average_speed, distance, kilojoules, max_speed, moving_ratio, total_elevation_gain) ~ gender.code, data = clean_data)
summary(gender.manova)
summary.aov(gender.manova)

# check each correlation using linear regression to see the trend
kilojoules.gender.fit <- lm(formula=kilojoules ~ gender.code, data=clean_data) 
distance.gender.fit <- lm(formula=distance ~ gender.code, data=clean_data)
max_speed.gender.fit <- lm(formula=max_speed ~ gender.code, data=clean_data) 
moving_ratio.gender.fit <- lm(formula=moving_ratio ~ gender.code, data=clean_data) 

kilojoules.gender.fit
distance.gender.fit
max_speed.gender.fit
moving_ratio.gender.fit

# Question 2
same_country.manova <- manova(cbind(average_speed, distance, kilojoules, max_speed, moving_ratio, total_elevation_gain) ~ same_country.code, data = clean_data)
summary(same_country.manova)
summary.aov(same_country.manova)

# check each correlation using linear regression to see the trend
kilojoules.same_city.fit <- lm(formula=kilojoules ~ same_country.code, data=clean_data) 
distance.same_city.fit <- lm(formula=distance ~ same_country.code, data=clean_data) 
max_speed.same_city.fit <- lm(formula=max_speed ~ same_country.code, data=clean_data) 
moving_ratio.same_city.fit <- lm(formula=total_elevation_gain ~ same_country.code, data=clean_data) 

kilojoules.same_city.fit
distance.same_city.fit
max_speed.same_city.fit
moving_ratio.same_city.fit


# run multiple tests
summary(gender.manova, test="H")
summary(gender.manova, test="R")
summary(gender.manova, test="P")
summary(gender.manova, test="W")

# run multiple tests
summary(same_country.manova, test="H")
summary(same_country.manova, test="R")
summary(same_country.manova, test="P")
summary(same_country.manova, test="W")


