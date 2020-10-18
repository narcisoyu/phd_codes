library(tuber)
library(youtubecaption) #HIGHLY RECOMMENDED TO USE THIS PACKAGE
library(tidyverse)

##IMPUT THE OAUTH, 
## it is important to REMOVE my id and replace with yours
app_id <- "UseYourAppID"
app_secret <- "UseYourAppSecret"
yt_oauth(app_id, app_secret)

##search for the key term from youtube channel
##EXAMPLE: BBCNEWS
bbc_results <- yt_search(term = "terrorism",
                         channel_id = "UC16niRr50-MSBwiO3YDb3RA", ##Dont forget to get the chennel_id, it is different from the user id
                         published_after = "2019-01-01T00:00:00Z", 
                         published_before = "2020-01-01T00:00:00Z")

#extract the video id
video_id <- as.character(bbc_results$video_id)


##get video details
#video_details <- get_video_details(video_id = "Mtskm5J7ztM")

##video details to data frame
#video_details1 <- as.data.frame(video_details)


#create a new (empty) list
dat <- list()

## for loop to select the VIDEOS WHICH CONTAIN SUBTITLES
## IF NO SUBTITLES RETURN ERROR, and the loop stops
## BUT! the tryCatch function will continue to excute the loop (skip error)
## and the 'dat' list contain all the video with subtitle
## at least the "Subtitle" button is available for the video
for (i in video_id) {
  tryCatch({
    caption_track <- list_caption_tracks(part = "snippet", video_id = i)
    dat[i] <- caption_track
  }, error=function(e){cat("ERROR :",conditionMessage(e))})
}

#remove NULL items in the dat list
dat[sapply(dat, is.null)] <- NULL

##convert the "dat" list to a dataframe
caption_tracks <- as.data.frame(do.call("rbind", dat))

##and we get all the video_id with subtitle
caption_tracks <- rownames_to_column(caption_tracks, "video_id")

video_caption <- as.character(caption_tracks$video_id)

##get subtitles

url <- paste0("https://www.youtube.com/watch?v=", video_caption)

###create a new dataframe "caption" to store the subtitles

caption <- data_frame()

for (i in url) {
  tryCatch({
    subtitle <- get_caption(i)
    caption <- rbind(caption, subtitle)
  }, error=function(e){cat("ERROR :",conditionMessage(e))})
}

#now you get the caption data frame, which contains all the subtitles
##make a copy of the caption dataframe as x
x <- as_data_frame(caption)

##want to remove other variables??
x$start <- NULL
x$duration <- NULL
x$segment_id <- NULL

y <- group_by(x, vid) %>% summarise(text=paste(text, collapse = " "))

##and the "y" is all the related subtitle

##write y as file to save?
##use the fuction write_csv or write.csv
## for example....
## write_csv(y...)

