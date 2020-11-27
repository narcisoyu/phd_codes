library(tuber)
library(tidyverse)

## import video list file

France24 <- read_csv("France24_terrorism.csv")

## sort dataframe by time
France24_copy <- France24[order(France24$publishedAt),]

## use 10 most recent videos as a test sample
## replace this command if you want to collect the comments of all the videos
## but DO remember the API limits
x <- tail(France24_copy, 10)

## collect most recent 20 comments, if you want to add more comments, 
## change the parameter of max_results

comments <- data.frame()

for (i in x$video_id) {
  comment <- get_comment_threads(filter = c(video_id = i), max_results = 20)
  comments <- bind_rows(comments, comment)
}

## have a look at the 
head(comments)

