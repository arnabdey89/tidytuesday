################################################################
#Title: Tidy Tuesday - Rap Artists: BBC Music 
#Submitted by: Arnab Dey
#Created: 4/14/2020
################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)

### Read Data
tuesdata <- tidytuesdayR::tt_load('2020-04-14')
polls <- tuesdata$polls

######### Summarize Data by Artist

###### Average Score per Artist
artist_avg <- polls %>%
  group_by(artist) %>%
  summarize(avg_rank = round(mean(rank),2))

###### Number of times Artist was voted
artist_count <- polls %>%
  group_by(artist) %>%
  summarize(num_times = n())

##### Artists and Gender

artist_gender <- gender_artist <- polls %>%
  group_by(artist) %>%
  distinct(gender)

#### Merge Datasets
df1 <- merge(x = artist_avg, y = artist_count)
df2 <- merge(x = df1, y = artist_gender, all.x = T)

## Categorize Average Rank
df2$avg_rank_cat <- 0
df2$avg_rank_cat[df2$avg_rank < 2] <- 1
df2$avg_rank_cat[df2$avg_rank >= 2 & df2$avg_rank < 3] <- 2
df2$avg_rank_cat[df2$avg_rank >= 3 & df2$avg_rank < 4] <- 3
df2$avg_rank_cat[df2$avg_rank >= 4] <- 4

df2$avg_rank_cat <- factor(df2$avg_rank_cat, levels = c(1,2,3,4), 
                           labels = c("1 to 1.99", "2 to 2.99", "3 to 3.99", "4 to 5"))

##### Plot 

scatter <- ggplot(data = df2) + 
  geom_point(mapping = aes(x=avg_rank_cat, y = num_times, color = gender,
    text = paste(
    "Artist: ", artist, "\n",
    "Average Rank: ", avg_rank, "\n",
    "# of time voted: ", num_times))) + 
  theme_light() + 
  labs(x = "Average Rank Received by Artists", y = "Number of times artists were voted",
       title = "Average Rank vs. Number of time Voted")

ggplotly(scatter, tooltip = "text")
