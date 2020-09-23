library(dplyr)
library(igraph)
library(ggraph)
library(readxl)

data <- read_excel("Codesheet_VisualNetwork.xlsx", sheet = "VisualSemCovid")
#filter semiotic resources !== 0 or 1
data <- filter(data, semiotic_resources != '0')
data <- filter(data, semiotic_resources != '1')

#filter visual semiotics !== 0 
data <- filter(data, visual_semiotics != '0')

##split the data into three time period
data1 <- filter(data, date < "2020-03-13")

data2 <- filter(data, date > "2020-03-13")
data2 <- filter(data2, date < "2020-05-07")

data3 <- filter(data, date > "2020-05-07")

df <- list(data1, data2, data3)
##select the dataframe for network
for (i in 1:length(df)) {
  df[[i]] <- df[[i]] %>%
    select(f1_update, f2_preventive, f3_medical, f4_social, f5_economic,
           f6_politics_instructions, f7_diplomatic, f8_personal, f9_misinformation, visual_semiotics)
  df[[i]]$visual_semiotics <- as.character(df[[i]]$visual_semiotics)
  df[[i]]$visual_semiotics <- gsub("1", "Iconic", df[[i]]$visual_semiotics)
  df[[i]]$visual_semiotics <- gsub("2", "Indexical", df[[i]]$visual_semiotics)
  df[[i]]$visual_semiotics <- gsub("3", "Symbolic", df[[i]]$visual_semiotics)
  df[[i]]$visual_semiotics <- gsub("4", "Other", df[[i]]$visual_semiotics)
}

#unlist
for (i in seq(df))
  assign(paste0("df", i), df[[i]])

##prepare for the matrix
##summerize the df by visual_semiotics

df1 <- df1 %>% 
          group_by(visual_semiotics) %>% 
          summarise_all(sum)

df2 <- df2 %>% 
  group_by(visual_semiotics) %>% 
  summarise_all(sum)

df3 <- df3 %>% 
  group_by(visual_semiotics) %>% 
  summarise_all(sum)

df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)
df3 <- as.data.frame(df3)

rownames(df1) <- df1$visual_semiotics
df1$visual_semiotics <- NULL

rownames(df2) <- df2$visual_semiotics
df2$visual_semiotics <- NULL

rownames(df3) <- df3$visual_semiotics
df3$visual_semiotics <- NULL

df1 <- as.matrix(df1)
df2 <- as.matrix(df2)
df3 <- as.matrix(df3)


g1 <- graph_from_incidence_matrix(df1, weighted = TRUE)
g2 <- graph_from_incidence_matrix(df2, weighted = TRUE)
g3 <- graph_from_incidence_matrix(df3, weighted = TRUE)

plot(g1)

##set edge width
E(g1)$width <- E(g1)$weight
E(g2)$width <- E(g2)$weight
E(g3)$width <- E(g3)$weight

##change vertex shape  and plot 
###g1
V(g1)$shape <- c("square", "square", "square", "square","circle", "circle", 
                 "circle", "circle", "circle", "circle", "circle", "circle", "circle")

V(g1)$color <- c("red","red","red","red",
                 "blue", "blue","blue","blue","blue","blue",
                 "blue","blue","blue")



plot(g1, vertex.size = strength(g1)/35, edge.width = E(g1)$width/10, edge.color = "grey")
title("Visual semiotic and frame network: Phase 1")



######g2

V(g2)$shape <- c("square", "square", "square", "square","circle", "circle", 
                 "circle", "circle", "circle", "circle", "circle", "circle", "circle")

V(g2)$color <- c("red","red","red","red",
                 "blue", "blue","blue","blue","blue","blue",
                 "blue","blue","blue")



plot(g2, vertex.size = strength(g2)/35, edge.width = E(g2)$width/10, edge.color = "grey")
title("Visual semiotic and frame network: Phase 2")




######g3

V(g3)$shape <- c("square", "square", "square", "square","circle", "circle", 
                 "circle", "circle", "circle", "circle", "circle", "circle", "circle")

V(g3)$color <- c("red","red","red","red",
                 "blue", "blue","blue","blue","blue","blue",
                 "blue","blue","blue")



plot(g3, vertex.size = strength(g3)/35, edge.width = E(g3)$width/10, edge.color = "grey")
title("Visual semiotic and frame network: Phase 3")



