library(igraph)
library(tidyverse)
#library(ggraph)

data <- read.csv("spexit_es.csv", fileEncoding = 'UTF-8')
# which tweets are retweets
rt_patterns = grep("(^RT)((?:\\b\\W*@\\w+)+)", data$text, ignore.case=TRUE)
# show retweets (these are the ones we want to focus on)
data$text[rt_patterns]

# we create a list to store user names
#who_retweet = as.list(1:length(rt_patterns))
#who_post = as.list(1:length(rt_patterns))

#extract the tweet texts
#tweets <- as.data.frame(data$text)
#names(tweets)[1] <- c("text")
#tweets <- as.list(tweets$text)
##extract the tweet username
#users <- as.data.frame(data$from_user_name)
#names(users)[1] <- c("user_name")
#users <- as.list(users$user_name)

#find who retweet and who post(from original text)
who_retweet <- as.data.frame(data$from_user_name[rt_patterns])
who_post <- as.data.frame(data$text[rt_patterns])

##remove everything after RT @username in who_post
names(who_retweet)[1] <- c("retweeter")
names(who_post)[1] <- c("poster")

who_post$poster <- gsub("RT @", "", who_post$poster)


##find the poster
poster <- word(who_post$poster, 1)
poster <- gsub(":", "", poster)

#poster <- as.list(1:length(rt_patterns))
#for (i in 1:length(rt_patterns)) {
#  poster[i] <- strsplit(who_post$poster," ")[[i]][1]
#  poster = gsub(":", "", unlist(poster))
#}

poster <- as.data.frame(poster)

##bind a new dataframe for igraph
retweeter_poster <- cbind(who_retweet, poster)

# generate graph
rt_graph = graph.edgelist(as.matrix(retweeter_poster))

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# choose some layout
glay = layout_with_fr(rt_graph)

# plot
par(bg="white", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="black",
     vertex.size=5,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=.165, s=.28, v=.08, alpha=1),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=1,
     edge.color=hsv(h=0, s=1, v=1, alpha=1))
# edge.color=("#408080")
# add title
#title("nTweets on #MachineLearning : Who retweets whom",
#      cex.main=1, col.main="black")

##export edge list
write.csv(retweeter_poster, file = "pagerank/edgelist_es.csv", row.names = FALSE,fileEncoding = 'UTF-8')

write_graph(rt_graph, "rt_graph.gml", format = c("gml"))
write_graph(rt_graph, "rt_graph.pajek", format = c("pajek"))
