library(tidyverse)
library(stringi)
library(magrittr)
library(flextable)
library(quanteda)
library(topicmodels)
library(LDAvis)
library(tm)
library(textclean)
library(data.table)
library(proustr)
#library(progress) #progress bar

my.data <- read.csv("el_pais.csv", fileEncoding = 'UTF-8')

#remove retweet
my.data <- my.data[!grepl("^RT", my.data$text), ]
my.data$text <- tolower(my.data$text)

##data clean
my.data$text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", my.data$text)
my.data$text <- replace_hash(my.data$text, replacement = "")
my.data$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", my.data$text) #remove emoji


###remove stopwords
my.data$text <- removeWords(my.data$text, stopwords::stopwords(language = "es", source = "stopwords-iso"))
my.data$text <- stripWhitespace(my.data$text)
my.data$text <- removeNumbers(my.data$text)
my.data$text <- removePunctuation(my.data$text)

my.data.2 <- my.data %>% 
  select(id.tweet, text) %>% 
  filter(text != "") %>% 
  filter(text != " ") %>%
  mutate(text = as.character(text))

##stemming
my.data.3 <- as.data.frame(pr_stem_sentences(my.data.2, text, language = "spanish"))


my.corpus <- corpus(my.data.3, docid_field = "id.tweet", text_field = "text")

my.toks <- tokens(
  my.corpus,
  what = "word",
  remove_punct = FALSE,
  remove_symbols = FALSE,
  remove_numbers = FALSE,
  remove_url = FALSE,
  remove_separators = FALSE,
  split_hyphens = FALSE,  ## VERSIÓN JINGYUAN NO QUITÁBAMOS
  include_docvars = FALSE,
  padding = FALSE,
  verbose = quanteda_options("verbose"))

#my.tokens <- tokens_wordstem(my.toks)

#my.tokens <- tokens_remove(my.toks, c("coronavirus", "covid19", "covid"), 
#                           padding = FALSE)
#my.tokens <- tokens_remove(my.tokens, pattern = c("^c"), valuetype = "regex") 
#remove stopword
#my.tokens <- tokens_remove(my.tokens, pattern = stopwords('en'))

my.dfm <- dfm(my.toks)
x <- textstat_frequency(my.dfm)

##### https://tutorials.quanteda.io/machine-learning/topicmodel/
# we keep only the top 5% of the most frequent features (min_termfreq = 0.95)
# that appear in less than 10% of all documents (max_docfreq = 0.1) 
# using dfm_trim() to focus on common but distinguishing features.

my.dfm.trim <- my.dfm %>%  
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
         max_docfreq = 0.1, docfreq_type = "prop")

my.dfm.trim <- my.dfm.trim[ntoken(my.dfm.trim) > 0,]

dtm <- convert(my.dfm.trim, to = "topicmodels")

#my.lda <- LDA(dtm, 4)

#terms(my.lda, 10)

#my.dfm.trim$topic <- topics(my.lda)
#head(topics(my.lda), 20)

#xx <- data.frame("Id" = rownames(my.dfm.trim), "Topic" = my.dfm.trim$topic)



#######script de jingyuan

#set seed and other value to control lda output https://rpubs.com/cosmopolitanvan/topicmodeling
burnin <- 4000
iter <- 2000
thin <- 500
nstart <- 8
#seed <-list(1541, 7573, 3590, 7822, 311, 7492)
seed <-list(1322, 7744, 335, 8191, 5802, 8066, 4972, 1806)
#seed <- replicate(nstart, sample(100:10000,size=1), simplify=FALSE)
best <- TRUE

#
ldaOut <-LDA(dtm, nstart , method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(ldaOut))
################################################################
############ perplexity method to find the best k ???? #########
################################################################
#n_topics <- c(2:100)
#lda_compare <- n_topics %>%
#  map(LDA, x = dtm, control = list(seed = 1109))

#data_frame(k = n_topics,
#           perplex = map_dbl(lda_compare, perplexity)) %>%
#  ggplot(aes(k, perplex)) +
#  geom_point() +
#  geom_line() +
#  labs(title = "Evaluating LDA topic models",
#       subtitle = "Optimal number of topics",
#       x = "Number of topics",
#       y = "Perplexity")
#################################################################
#################################################################

terms(ldaOut, 10)
head(topics(ldaOut), 20)

z <- data.frame("Id" = rownames(my.dfm.trim), "Topic" = topics(ldaOut))
colnames(z)[1] <- c("id.tweet")

#my.data$id.tweet <- as.factor(my.data$id.tweet)

#write.csv(my.data, file = 'elpais_lda.csv', row.names = FALSE, fileEncoding = 'UTF-8') 

######################
#topic1 Livelihood
#topic2 Public Health Professional
#topic3 Pandemic Update
#topic4 Madrid Deescalation
#topic5 Politics
#topic6 StateOfAlarm
#topic7 Economy
#topic8 Covid Information
my.data <- left_join(my.data, z, by = "id.tweet")
my.data$Topic <- gsub("1", "Livelihood", my.data$Topic)
my.data$Topic <- gsub("2", "PublicHealthProfessional", my.data$Topic)
my.data$Topic <- gsub("3", "PandemicUpdate", my.data$Topic)
my.data$Topic <- gsub("4", "MadridDeescalation", my.data$Topic)
my.data$Topic <- gsub("5", "Politics", my.data$Topic)
my.data$Topic <- gsub("6", "StateOfAlarm", my.data$Topic)
my.data$Topic <- gsub("7", "Economy", my.data$Topic)
my.data$Topic <- gsub("8", "CovidInformation", my.data$Topic)

#my.data$Topic <- gsub("FamilyLife", "Livelihood", my.data$Topic)
#my.data$Topic <- gsub("USPolitics", "Politics", my.data$Topic)
#my.data$Topic <- gsub("SpanishGovPolicy", "StateOfAlarm", my.data$Topic)

##split my.data into 3 parts
#Estado de alarma: 14 de marzo,
#fase 1 de descalada 11 de mayo
my.data <- my.data %>% mutate(date = lubridate::ymd_hms(date))
my.data <- my.data[order(my.data$date),]
p1 <- my.data[my.data[["date"]] < "2020-03-14 01:00:00", ]
p2 <- my.data[my.data[["date"]] >= "2020-03-14 01:00:00", ]
p2 <- p2[p2[["date"]] < "2020-05-11 02:00:00", ]
p3 <- my.data[my.data[["date"]] >= "2020-05-11 02:00:00", ]

p1 <- filter(p1, !is.na(p1$Topic))
p2 <- filter(p2, !is.na(p2$Topic))
p3 <- filter(p3, !is.na(p3$Topic))

write.csv(p1, file = 'elpais_p1.csv', row.names = FALSE, fileEncoding = 'UTF-8') 
write.csv(p2, file = 'elpais_p2.csv', row.names = FALSE, fileEncoding = 'UTF-8') 
write.csv(p3, file = 'elpais_p3.csv', row.names = FALSE, fileEncoding = 'UTF-8') 

lda_results<- as.data.frame(terms(ldaOut, 10))
write.csv(lda_results, "statistics/lda_results.csv", fileEncoding = 'UTF-8')

output_p1 <- as.data.frame(table(p1$Topic))
output_p2 <- as.data.frame(table(p2$Topic))
output_p3 <- as.data.frame(table(p3$Topic))
write.csv(output_p1, "statistics/output_p1.csv", fileEncoding = 'UTF-8')
write.csv(output_p2, "statistics/output_p2.csv", fileEncoding = 'UTF-8')
write.csv(output_p3, "statistics/output_p3.csv", fileEncoding = 'UTF-8')





###quizas INUTIL!!!
#export lda results for network analysis
lda_results <- as.data.frame(terms(ldaOut, 200))
#convert rows to columns and cols to rows
lda_results <- as.data.frame(t(lda_results))
#add one column as the "topic" variable
lda_results <- as.data.frame(setDT(lda_results, keep.rownames = TRUE)[])
colnames(lda_results)[1] <- "topic"
#
lda_results$topic <- gsub("Topic 1", "GovernmentPolicy", lda_results$topic)
lda_results$topic <- gsub("Topic 2", "CovidInformation", lda_results$topic)
lda_results$topic <- gsub("Topic 3", "GeneralElection", lda_results$topic)
lda_results$topic <- gsub("Topic 4", "DailyHealthAlert", lda_results$topic)
##merge row values 
# columns to paste together
cols <- c(stri_paste("V", 1:200))
# create a new column `x` with the three columns collapsed together
lda_results$text <- apply( lda_results[ , cols ] , 1 , paste , collapse = " " )
# remove the unnecessary columns
lda_results <- lda_results[ , !( names( lda_results ) %in% cols ) ]
#write csv
write.csv(lda_results, file = 'elpais_python_lda.csv', row.names = FALSE, fileEncoding = 'UTF-8') 
