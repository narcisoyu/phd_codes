library(tidyverse)
library(bibliometrix)
library(SnowballC)
library(tm)
library(textclean)
library(stopwords)
library(proustr)

data <- read.csv("spexit-all-data.csv", fileEncoding = "UTF-8")

es <- filter(data, lang == "es")
en <- filter(data, lang == "en")
ca <- filter(data, lang == "ca")

##remove retweets
es <- es[!grepl("^RT", es$text), ]
en <- en[!grepl("^RT", en$text), ]
ca <- ca[!grepl("^RT", ca$text), ]

##convert all texts to lower case
es$text <- tolower(es$text)
en$text <- tolower(en$text)
ca$text <- tolower(ca$text)

#remove URL, #, @ and emoji
es$text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", es$text)
es$text <- replace_hash(es$text, replacement = "")
es$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", es$text) #remove emoji

en$text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", en$text)
en$text <- replace_hash(en$text, replacement = "")
en$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", en$text) #remove emoji

ca$text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", ca$text)
ca$text <- replace_hash(ca$text, replacement = "")
ca$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", ca$text) #remove emoji

###remove stopwords
es$text <- removeWords(es$text, stopwords::stopwords(language = "es", source = "stopwords-iso"))
es$text <- stripWhitespace(es$text)
es$text <- removeNumbers(es$text)
es$text <- removePunctuation(es$text)

en$text <- removeWords(en$text, stopwords::stopwords(language = "en", source = "stopwords-iso"))
en$text <- stripWhitespace(en$text)
en$text <- removeNumbers(en$text)
en$text <- removePunctuation(en$text)

ca$text <- removeWords(ca$text, stopwords::stopwords(language = "ca", source = "stopwords-iso"))
##additional stopwords for catalan
stopword_ca <- c("l", "lo", "si", "d")
ca$text <- removeWords(ca$text, stopword_ca)
ca$text <- stripWhitespace(ca$text)
ca$text <- removeNumbers(ca$text)
ca$text <- removePunctuation(ca$text)



##############conceptual structure 
#add ; after every word
es$text <- stripWhitespace(es$text) 
es$text <- gsub("[ |\t]{2,}", " ", es$text)  # Remove tabs
es$text <- gsub("^ ", "", es$text)  # Leading blanks
es$text <- gsub(" $", "", es$text)  # Lagging blanks
es$text <- gsub(" +", " ", es$text) # General spaces 
#es$text <- gsub(" ", "; ", es$text)

en$text <- stripWhitespace(en$text) 
en$text <- gsub("[ |\t]{2,}", " ", en$text)  # Remove tabs
en$text <- gsub("^ ", "", en$text)  # Leading blanks
en$text <- gsub(" $", "", en$text)  # Lagging blanks
en$text <- gsub(" +", " ", en$text) # General spaces 
#en$text <- gsub(" ", "; ", en$text)

ca$text <- stripWhitespace(ca$text) 
ca$text <- gsub("[ |\t]{2,}", " ", ca$text)  # Remove tabs
ca$text <- gsub("^ ", "", ca$text)  # Leading blanks
ca$text <- gsub(" $", "", ca$text)  # Lagging blanks
ca$text <- gsub(" +", " ", ca$text) # General spaces 
#ca$text <- gsub(" ", "; ", ca$text)

new_es <- es
new_en <- en
new_ca <- ca

###stemming
#new_es <- as.data.frame(pr_stem_sentences(es, text, language = "spanish"))
#new_en <- as.data.frame(pr_stem_sentences(en, text, language = "english"))
#new_ca <- as.data.frame(pr_stem_sentences(ca, text, language = "french"))


colnames(new_es)[5] <- c("ID")
colnames(new_en)[5] <- c("ID")
colnames(new_ca)[5] <- c("ID")

new_es$ID <- gsub(" ", "; ", new_es$ID)
new_en$ID <- gsub(" ", "; ", new_en$ID)
new_ca$ID <- gsub(" ", "; ", new_ca$ID)

colnames(new_es)[11] <- c("TC")
colnames(new_en)[11] <- c("TC")
colnames(new_ca)[11] <- c("TC")

####MCA
CS_es <- conceptualStructure(new_es, method="MCA", field="ID", minDegree=50, clust= 7, k.max = 7, stemming=FALSE, labelsize=8)
CS_en <- conceptualStructure(new_en, method="MCA", field="ID", minDegree=10, clust= 5, k.max = 5, stemming=FALSE, labelsize=8)
CS_ca <- conceptualStructure(new_ca, method="MCA", field="ID", minDegree=10, clust= 6, k.max = 6, stemming=FALSE, labelsize=8)

CS_es[["graph_terms"]][["labels"]][["title"]] <- c("Spexit content clustering: Spanish")
CS_en[["graph_terms"]][["labels"]][["title"]] <- c("Spexit content clustering: English")
CS_ca[["graph_terms"]][["labels"]][["title"]] <- c("Spexit content clustering: Catalan")

CS_es
CS_en
CS_ca
