library(readxl)
library(tidyverse)
library(igraph)
library(rgexf)
library(png)
library(ggraph)
#library(patchwork)
#library(jpeg)

data <- read_excel("Dataset_VisualSemCovid.xlsx", sheet = "VisualSemCovid")
#filter crisis phases = 1
data1 <- filter(data, crisis_phases == '1')
#filter faznet data 
data1 <- filter(data1, media == "1")
#filter semiotic resources !== 0 or 1
data1 <- filter(data1, semiotic_resources != '0')
data1 <- filter(data1, semiotic_resources != '1')

##add one more column for pictures
data1 <- mutate(data1, PIC = paste0("pic", row_number()))
#remove data
rm(data)
###build a new dataframe to tidy data
df <- data1 %>%
            select(f1_update, f2_preventive, f3_medical, f4_social, f5_economic,
                   f6_politics_instructions, f7_diplomatic, f8_personal, f9_misinformation, PIC)
##convert column f1 to f9 as numeric
lapply(df[,1:9], as.numeric)
##convert df to matrix
df <- as.data.frame(df)
rownames(df) <- df$PIC
df$PIC <- NULL
df <- as.matrix(df)
##create graph from matrix
g <- graph_from_incidence_matrix(df)
plot(g)

###filter graph by vertex degree
del = which(degree(g) <= 1)
g2 = delete.vertices(g, del)
plot(g2)

###layout?
set.seed(1)
LO <- layout.fruchterman.reingold(g2)
  
plot(g2, layout = LO)
#######read jpg/png
nodes <- V(g2)$name
#define a new vector/list
img <- vector(mode = "list", length= length(nodes))
#change list item name
names(img) <- nodes

for (node in nodes) {
  img[[node]] <- readPNG(paste0("image/converted_image/",node,".png"))
#  assign(node, readPNG(paste0("image/converted_image/",node,".png")))
}

#pic2 = readPNG("image/converted_image/pic2.png")
#pic3 = readPNG("image/converted_image/pic3.png")

##create $raster for image preview
V(g2)$raster = replicate(vcount(g2), img[["pic2"]], simplify=FALSE)

for (i in 2:length(nodes)) {
  V(g2)$raster[[i]] <- img[[i]]
}

#V(g2)$raster[[2]] <- pic3

plot(g2, vertex.shape='raster', vertex.label = NA, layout = LO, edge.width = 5, edge.color = "black")

##save g2 to gephi?
#igraph.to.gexf(g2)





###ANOTHER IDEA
#create frame network from the matrix 'df'
##CREATE A CO-OCCURRENCE (frame) MATRIX FROM THE 'df'
M <- crossprod(df)
diag(M) <- 0
g_frame <- graph_from_adjacency_matrix(M, mode = c("undirected"), weighted = TRUE, diag = FALSE)
plot(g_frame)
###filter graph by vertex degree
del_frame = which(degree(g_frame) < 1)
g_frame_2 = delete.vertices(g_frame, del_frame)
plot(g_frame_2)



##add node strength


set.seed(5200)

g_frame_3 <- g_frame_2 %>%  
    ggraph(layout = 'drl') +
    #scale_label_size(minor_breaks = NULL) +
    geom_edge_link(aes(edge_width = E(g_frame_2)$weight, alpha = E(g_frame_2)$weight)) +
    scale_edge_width(range = c(0.1, 3))+ # control size
    #geom_edge_link(aes(alpha = weight)) +
    geom_node_point(aes(size = strength(g_frame_2), alpha = strength(g_frame_2))) + scale_size(range = c(1,15)) +
    geom_node_label(aes(label = name), repel = TRUE)+
    #geom_node_label(aes(label = id.aff, size = peso))+
    theme_graph() +
    labs(title = c("Frame network")) +
    theme(legend.position = "none") #if hide legend, add this line
    #labs(subtitle = my.period[nperiod])

g_frame_3

