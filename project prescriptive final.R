#----------------Installing Required Libraries------------------------------- 
install.packages("tornado")
install.packages("readr")
install.packages("dplyr")
install.packages("factoextra")
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(tornado)
library(readr)
library(dplyr)
library(ggcorrplot)
library(tornado)
library(factoextra)

#-------------- Get the Data----------------------------
spotify <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO UPDATE tidytuesdayR from GitHub

#----------------------Basic Description------------------------------- 
summary(spotify)
head(spotify)
head(spotify,n=2)
dim(spotify)

#------------------------Removing Missing Values-----------------------------
sapply(spotify, function(col)
  sum(is.na(col)) / length(col)
) %>% round(digits=2)
which(is.na(spotify), arr.ind=TRUE)
# Identify rows with missing values
missing_rows <- which(is.na(spotify), arr.ind = TRUE)[, 1]
# Extract rows with missing values
missing_data <- spotify[missing_rows, ]
# Print the rows with missing values
print(missing_data)
# Remove rows with missing values
spotify <- na.omit(spotify)
summary(spotify)


#-----------(DATA FILTER and Cleaning)--------------------
names(spotify)=  gsub("_","",names(spotify))

spotify2 <- subset(spotify,select =c(4,12,13,14,15,16,17,18,19,20,21,22,23) )
spotify3 <- subset(spotify,select =c(12,13,14,15,16,17,18,19,20,21,22,23) )
#spotify2<- select (spotify,-track_id,-track_name,-track_artist,-track_popularity,-track_album_name)

# Co-relation plot
corelation = round(cor(spotify3),2)
ggcorrplot(corelation, hc.order = TRUE, type = "lower",
           lab = TRUE)
corelation = round(cor(spotify2),2)
ggcorrplot(corelation, hc.order = TRUE, type = "lower",
           lab = TRUE)

#---------------------(Standardizing)----------------------------
# Rescale popularity scores to a scale from 0 to 10
rescaled_popularity <- scale(spotify$trackpopularity, center = min(spotify$trackpopularity), scale = max(spotify$trackpopularity) - min(spotify$trackpopularity)) * 10

# Convert rescaled popularity scores to categorical data
num_categories <- 3
popularity_category <- cut(rescaled_popularity, breaks = num_categories, labels = c("Low", "Medium", "High"))

# Print the result
print(popularity_category)

# Add the rescaled popularity scores as a new column in the spotify dataframe
spotify$rescaled_popularity <- rescaled_popularity
spotify$popularity <- popularity_category

spotify2$rescal <- rescaled_popularity
spotify2$category <- popularity_category

spotify3$popularity_category <- popularity_category
spotify3$rescaled <- rescaled_popularity

#---------------(convetring into Date Datatype)-----------------------

# Convert dates to Date objects
dates_as_date <- as.Date(spotify$trackalbumreleasedate)
# Extract year from each date
years <- year(dates_as_date)
# Print the extracted years
print(years)

spotify$year = years 


#--------------------------------LINEAR REGRESSION-------------------------------

names(spotify2)

model=lm(trackpopularity ~
         danceability +
         key +
         loudness +
         mode +
         speechiness +
         acousticness +
         instrumentalness +
         liveness +
         valence +
         tempo +
         durationms ,
         data = spotify2)

options(scipen = 999)
print(model)
summary(model)

# Get the model residuals
model_residuals = model$residuals
# Plot the result
hist(model_residuals)
# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals)



model2=lm(trackpopularity~
           danceability +
           loudness +
           speechiness +
           acousticness +
           instrumentalness +
           liveness +
           valence +
           tempo +
           durationms ,
         data = spotify2)
print(model2)
summary(model2)

# Get the model residuals
model_residuals2 = model2$residuals
# Plot the result
hist(model_residuals2)
# Plot the residuals
qqnorm(model_residuals2)
# Plot the Q-Q line
qqline(model_residuals2)

#---------------------(Basic Visuals)--------------------------------
#Basic graph un wanted

# now do the plot...you can play with the vjust and hjust
ggplot(spotify, aes(x = trackpopularity, fill = factor(trackpopularity))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.50))
#--------------------------Checking Distnict value-------------------------------------------
#Distinct values from track popularity and track artist.
distinct_counts <- table(spotify$trackpopularity)
print(distinct_counts)

distinct_counts2 <- table(spotify$trackartist)
print(distinct_counts2)

#------------------Extracting Time ----------------------------------
#Duration ms in minute's 

# Assuming your dataframe is named spotify
# Convert milliseconds to minutes
duration_minutes <- spotify$durationms / 60000

average_Duration_minutes <- mean(duration_minutes)
average_Duration_hours <- average_Duration_minutes / 60

cat("Average Duration Time(minutes):",round(average_Duration_minutes,2),"\n")
cat("Average Duration Time (hours):",round(average_Duration_hours,2),"\n")

#-----------------(Tornoado plot)-------------------
#GENRAL LINER MODEL (tornoado plot)

genral=lm(trackpopularity~ 1,
         data = spotify2)
summary(genral)
imp <- importance(model2,genral)
plot(imp)


#-----------------(Bar GraphS)---------------------------
#Plot between playlistgenre and playlistsubgenre

spotify %>% distinct(playlistgenre)
spotify %>% distinct(playlistsubgenre)


# Create the bar plot
ggplot(spotify, aes(x = playlistgenre, fill = playlistgenre)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50)) +
  # Add value labels in the middle of the bars
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    vjust = 5,
    color = "black",
    size = 3
  )


ggplot(spotify, aes(x = playlistsubgenre, fill = playlistsubgenre)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))

ggplot(spotify, aes(x = playlistsubgenre)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))

# use dplyr to generate a new dataframe with grouped totals
spotifyfill <- spotify %>% count(playlistgenre,playlistsubgenre)


# plot that 
ggplot(spotifyfill, aes(fill=playlistgenre, y=n, x=playlistsubgenre)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))


#----------------(CHISQUARE TEST)-------------------------------
#CHISQUARE TEST

# Create a data frame from the main data set.
stu_data = data.frame(spotify$playlistgenre,spotify$playlistsubgenre)
stu_data[is.na(stu_data)] <- 0
print(stu_data)
# Create a contingency table for the chi-square test
stu_data.ct <- table(stu_data)
print(stu_data.ct)
# Turn off scientific notation
options(scipen = 999)
# applying chisq.test() function
print(chisq.test(stu_data.ct))

stu_data_cleaned <- stu_data[!is.na(stu_data$ct), ]


# Create a data frame from the main data set.
stu_data = data.frame(spotify$popularity,spotify$key)
stu_data[is.na(stu_data)] <- 0
print(stu_data)
# Create a contingency table for the chi-square test
stu_data.ct <- table(stu_data)
print(stu_data.ct)
# Turn off scientific notation
options(scipen = 999)
# applying chisq.test() function
print(chisq.test(stu_data.ct))
stu_data_cleaned <- stu_data[!is.na(stu_data$ct), ]

# Create a data frame from the main data set.
stu_data = data.frame(spotify$popularity,spotify$playlistgenre)
stu_data[is.na(stu_data)] <- 0
print(stu_data)
# Create a contingency table for the chi-square test
stu_data.ct <- table(stu_data)
print(stu_data.ct)
# Turn off scientific notation
options(scipen = 999)
# applying chisq.test() function
print(chisq.test(stu_data.ct))
stu_data_cleaned <- stu_data[!is.na(stu_data$ct), ]

# Create a data frame from the main data set.
stu_data = data.frame(spotify$popularity,spotify$playlistsubgenre)
stu_data[is.na(stu_data)] <- 0
print(stu_data)
# Create a contingency table for the chi-square test
stu_data.ct <- table(stu_data)
print(stu_data.ct)
# Turn off scientific notation
options(scipen = 999)
# applying chisq.test() function
print(chisq.test(stu_data.ct))
stu_data_cleaned <- stu_data[!is.na(stu_data$ct), ]

# Create a data frame from the main data set.
stu_data = data.frame(spotify$popularity,spotify$mode)
stu_data[is.na(stu_data)] <- 0
print(stu_data)
# Create a contingency table for the chi-square test
stu_data.ct <- table(stu_data)
print(stu_data.ct)
# Turn off scientific notation
options(scipen = 999)
# applying chisq.test() function
print(chisq.test(stu_data.ct))
stu_data_cleaned <- stu_data[!is.na(stu_data$ct), ]


#---------------------logistics regression------------------------------------

spotify4 <- subset(spotify,select =c(12,15,17,18,19,20,21,22,23) )

spotify4$popularitycategory <- popularity_category
spotify4$rescaled <- -rescaled_popularity



# Run the model
logit <- glm(formula = popularitycategory ~ ., family = binomial(link = "logit"), data = spotify4)
# Do a null model for comparison
logit_reduced <- glm(formula = popularitycategory ~ 1, family = binomial(link = "logit"), data = spotify4)

library(tornado)
library(readr)
library(dplyr)
summary(logit)
summary(logit_reduced)
imp <- importance(logit, logit_reduced)
plot(imp)
#-----------------------------------------ANOVA-------------------------
modela=lm(trackpopularity ~
            key +
            playlistgenre +
            mode +
            playlistsubgenre ,
          data = spotify)

anova(modela)
options(scipen = 999)
print(model)
summary(model)

# Get the model residuals
model_residuals = model$residuals
# Plot the result
hist(model_residuals)
# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals)



model2a=lm(rescaled_popularity~
             danceability +
             loudness +
             mode +
             acousticness +
             instrumentalness +
             liveness +
             valence +
             tempo +
             durationms ,
           data = spotify2)
anova(modela,model2a)
anova(model2a)
summary(model2a)
# Get the model residuals
model_residuals2 = model2$residuals
# Plot the result
hist(model_residuals2)
# Plot the residuals
qqnorm(model_residuals2)
# Plot the Q-Q line
qqline(model_residuals2)



# Perform the two-way ANOVA
two <- aov(trackpopularity ~
             mode +
             key +
             playlistgenre +
             playlistsubgenre ,
           data = spotify)

three = aov(trackpopularity~ playlistsubgenre, data = spotify )
# Print the ANOVA table
print(summary(two))


print(summary(three))


#-----------------------------Extracting Date----------------------------------------

# Convert 'date' column to Date type (if it's not already)
stu_data$date <- as.Date(spotify$trackalbumreleasedate)

# Extract year from the 'date' column
stu_data$year <- format(stu_data$date, "%Y")

spotify<- cbind(spotify,stu_data$year)

#-----------------------------------Group by List--------------------------------------------

library(tidyverse) # metapackage of all tidyverse packages
library(ggridges)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(plotly)
library(tidyverse)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(stringr)
library(hrbrthemes)
library(viridis)
library(janeaustenr)
library(SnowballC)
library(magrittr)
library(glue)
library(stringi)
library(grDevices)
library(wordcloud2)
library(extrafont)
library(grid)
library(gridExtra)
library(radarchart)
library(ggraph)
library(DT)
library(ggrepel)
library(kableExtra)
library(cowplot)
library(tinytex)
library(IRdisplay)



all_average <- spotify2 %>% 
  drop_na(trackpopularity) %>%
  dplyr::group_by(trackpopularity) %>% 
  dplyr::summarise(count = n(),
                   across(everything(), mean))


top_10 <- spotify2 %>% 
    drop_na(trackpopularity) %>%
    dplyr::filter(trackpopularity >= 91 & trackpopularity <= 100) %>%
    dplyr::group_by(trackpopularity) %>% 
    dplyr::summarise(count = n(),
                     across(everything(), mean))

top_100 <- spotify2 %>% 
  drop_na(trackpopularity) %>%
  dplyr::group_by(category) %>% 
  dplyr::summarise(count = n(),
                   across(everything(), mean))

  



#----------------------------------(Scatter Plot)------------------------
library(ggplot2)
library(dplyr)

# Assuming 'spotify2' is your dataset containing the 'trackpopularity' column
filtered_data <- spotify %>%
  filter(trackpopularity >= 90 & trackpopularity <= 100)

# Assuming 'spotify2' is your dataset containing 'popularity' and 'speechiness' columns
ggplot(filtered_data, aes(x = trackpopularity, y = speechiness)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()

# Assuming 'spotify2' is your dataset containing 'popularity' and 'speechiness' columns
ggplot(filtered_data, aes(x = trackpopularity, y = danceability)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()

# Assuming 'spotify2' is your dataset containing 'popularity' and 'speechiness' columns
ggplot(filtered_data, aes(x = trackpopularity, y = energy)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()

# Assuming 'spotify2' is your dataset containing 'popularity' and 'speechiness' columns
ggplot(filtered_data, aes(x = trackpopularity, y = loudness)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()
# Assuming 'spotify2' is your dataset containing 'popularity' and 'speechiness' columns
ggplot(filtered_data, aes(x = trackpopularity, y = loudness)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()
#----------------------------(top 10)---------------------------------
# Assuming 'df' is your dataset containing 'artist_name' column
df <- spotify %>%
  mutate(Count = 1) %>%
  group_by(trackartist) %>%
  summarise(Count = sum(Count)) %>%
  arrange(desc(Count)) %>%
  head(10)
filtered <- spotify %>%
  filter(trackpopularity >= 90 & trackpopularity <= 100) %>%
  group_by(trackartist) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
write.csv(filtered, file = "artist.csv", row.names = FALSE)

filtered1 <- spotify %>%
  filter(trackpopularity >= 90 & trackpopularity <= 100) %>%
  group_by(trackalbumname) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
#-------------------------(Extra Bar plot)-------------------------

filtered2 <- spotify %>%
  group_by(playlistgenre) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)


library(ggplot2)
ggplot(filtered2, aes(x = playlistgenre, y = count, fill = playlistgenre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))


filtered3 <- spotify %>%
  filter(trackpopularity >= 90 & trackpopularity <= 100) %>%
  group_by(playlistgenre) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
write.csv(filtered3, file = "dafil3.csv", row.names = FALSE)




ggplot(filtered2, aes(x = playlistgenre, y = count, fill = playlistgenre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))


fil1 <- spotify %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  
ggplot(fil1, aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))
#-----------------List in every category-------------------
#no of songs every year
fil2 <- spotify %>%
  select(everything()) %>% 
  filter(popularity_category == "High")%>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#no of songs by genre based on high popularity 
fil3 <- spotify %>%
  select(everything()) %>% 
  filter(popularity_category == "High")%>%
  group_by(playlistgenre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#no of songs by genre based on low popularity 
fil4 <- spotify %>%
  select(everything()) %>% 
  filter(popularity_category == "Low")%>%
  group_by(playlistgenre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


modified_table <- fil2 %>%
  mutate(column_name = "s")  # Replace "New Name" with the name you want to assign to all rows

# Specify the row index for the row you want to keep unchanged
row_to_keep_unchanged <- 1  # Replace 3 with the index of the row you want to keep unchanged

# Modify the value in the specified row only
modified_table <- modified_table %>%
  mutate(column_name = ifelse(row_number() == row_to_keep_unchanged, 2019, "All Years"))

# no of songs btween 2018 and remaining years 
dfilo <- modified_table %>%
  group_by(column_name) %>%
  summarise(Count = sum(count)) 

# Plot the bar chart
ggplot(dfilo, aes(x = column_name, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Count of popular songs", x = "Years", y = "Count") +
  theme_minimal()


#-----------------------------------------------------------------------------------
# Assuming both datasets have a column representing the year
# Let's assume the column name representing the year in spotify is 'year' and in fill2 is also 'year'

# Perform the join based on the 'year' column
merged_data <- merge(spotify, fil2, by = "year", all.x = TRUE)

# Print the merged dataset
print(merged_data)
# --------------Bar plot of popularity category--------------------------------
ggplot(merged_data, aes(x = liveness, y = popularity_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))

# Scatter Plot of Popularity vs. Speechiness
ggplot(filtered_data, aes(x = energy, y = energy)) +
  geom_point() +  # Scatter plot
  labs(title = "Scatter Plot of Popularity vs. Speechiness",
       x = "Popularity",
       y = "Speechiness") +
  theme_minimal()

# Perform summarization and assign the result to fil5
fil5 <- spotify2 %>%
  group_by(rescaled_popularity) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


# Assuming popularity_category is a variable in fil5
ggplot(fil5, aes(x = liveness, y = popularity_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), stat = "count", vjust = -0.5, color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 0.50))


#---------------saving the file in csv---------------------------
write.csv(fil5, file = "mydata.csv", row.names = FALSE)
write.csv(spotify2, file = "data2.csv", row.names = FALSE)


