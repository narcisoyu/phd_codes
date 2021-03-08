library(tidyverse)
library(quanteda) # a R package for text analysis
library(stm) # Structural topic model
library(readtext)
library(tm)

## Some resources you may interest
# https://github.com/dondealban/learning-stm
# https://github.com/cbpuschmann/stm_ic2s2 

# import data

data <- readtext("France24_terrorism.csv", text_field = "title")

## CLEAN THE DATA?

# Decode html
data$text <- textutils::HTMLdecode(data$text)

# conver letters to lower case, remove punctuation, remove numbers, remove stopwords
data$text <- data$text %>% tolower() %>% removePunctuation() %>% removeNumbers %>% removeWords(stopwords("english"))

# JUST FOUND THE DATA IS NOT CLEAN ENOUGH
# SOME PUNCTUATIONS STILL REMAIN
# SO...
data$text <- str_replace_all(data$text, "[[:punct:]]", " ")

# sort data by time

data <- data[order(data$publishedAt),]

# convert time variable to yyyy

data$publishedAt <- format(as.Date(data$publishedAt, "%Y-%m-%d"), "%Y")

# convert the data frame to corpus

corpus <- corpus(data)

head(summary(corpus))


# convert corpus to dfm (document feature matrix)
# I use stemming, to better perform the analysis 
# but whether use stemming depends on many things.. 
# give it a try if you dont want to stem the words

dfm <- dfm(corpus, stem = TRUE)

# set dfm margin min 1% / max 99%
dfm.trim <- dfm_trim(dfm, min_docfreq = 0.01, max_docfreq = 0.99, 
                     docfreq_type = "prop") 

# go for stm (structural topic model)
# convert dfm to stm object

dfm2stm <- convert(dfm.trim, to = "stm")

# run stm model
# set how many topics you want to compute
topic.count <- 9

model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, 
                 K = topic.count, data = dfm2stm$meta, 
                 init.type = "Spectral")

# Have a look at the model
data.frame(t(labelTopics(model.stm, n = 10)$prob))

plot(model.stm, type = "summary", text.cex = 0.5)

# compare topic components between two topics
plot(model.stm, type = "perspectives", topics = c(2,6)) 

# Distribution of MAP Estimates of Document-Topic Proportions 
# hist plots a histogram of the MAP estimates of the document-topic loadings across all documents.
# MAP = Maximum a posteriori
plot(model.stm, type = "hist", topics = sample(1:topic.count, size = 9))


# topic prevalence over time
model.stm.labels <- labelTopics(model.stm, 1:topic.count)

dfm2stm$meta$datum <- as.numeric(dfm2stm$meta$publishedAt)

# Estimates a regression where documents are the units, 
# the outcome is the proportion of each document about a topic 
# in an STM model and the covariates are document-meta data. 
# This procedure incorporates measurement uncertainty from the STM model 
# using the method of composition.
model.stm.ee <- estimateEffect(1:topic.count ~ s(datum), model.stm, meta = dfm2stm$meta)

# plot
par(mar=c(1,1,1,1))
par(mfrow=c(3,3))
for (i in seq_along(sample(1:topic.count, size = 9))){
  plot(model.stm.ee, "datum", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
}
