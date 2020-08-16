####################################################################################################
## Analyzing Spanish News Frames on Twitter during COVID-19: A Network Study of El País and El Mundo
## Frames Network
## julio 2020
####################################################################################################

library(tidyverse)
library(magrittr)
library(quanteda)
library(igraph)
library(Matrix)
library(ggraph)
library(tidygraph)
library(ggrepel)
library(patchwork) # Posicionamiento gráficos

# library(devtools)
# install_github("cbail/textnets")
# library(textnets)

load("001-lda.RData")

## Elimina "todo menos los p#
no.remove <- c("p1", "p2", "p3")
rm(list=ls()[! ls() %in% no.remove])

## Renombra el frame "MadridDeescalation"
p1$Topic <- str_replace(p1$Topic, "MadridDeescalation", "Madrid")
p2$Topic <- str_replace(p2$Topic, "MadridDeescalation", "Madrid")
p3$Topic <- str_replace(p3$Topic, "MadridDeescalation", "Madrid")

## Creación de los contenedores de listas
my.p <- vector(mode = "list", length= 3)
my.corpus <- vector(mode = "list", length= 3)
my.dfm <- vector(mode = "list", length= 3)
my.cooc  <- vector(mode = "list", length= 3)
my.igraph  <- vector(mode = "list", length= 3)
my.g  <- vector(mode = "list", length= 3)

## Etiquetas de periodos y diario
my.period <- c("Pre-crisis", "Lockdown", "Recovery")
my.newspaper <- c("El País")

## Selecciona sólo las columnas necesarias para análisis
my.p[[1]] <- p1 %>% 
  select(id.tweet, text, Topic)
  
my.p[[2]] <- p2 %>% 
  select(id.tweet, text, Topic)

my.p[[3]] <- p3 %>% 
  select(id.tweet, text, Topic) 

## Cuenta el número de tweets de cada frame
my.count <- p1 %>% 
  group_by(Topic) %>% 
  summarise(N = n()) 

## Genera corpus, dfm y dfm _tfidf (term frequency-inverse document frequency weighting) para cada período
for (nperiod in 1:3) {
  my.corpus[[nperiod]] <- corpus(my.p[[nperiod]], docid_field = "id.tweet", text_field = "text")
  my.dfm[[nperiod]] <- dfm(my.corpus[[nperiod]], groups = "Topic")
  my.dfm[[nperiod]] <- dfm_tfidf(my.dfm[[nperiod]])

## Matriz de coocurrencias de palabras entre frames y transformación en igraph
my.cooc[[nperiod]] <- my.dfm[[nperiod]] %*% t(my.dfm[[nperiod]])
my.igraph[[nperiod]] <- graph_from_adjacency_matrix(my.cooc[[nperiod]], mode = "undirected", weighted = TRUE, diag = FALSE)

## Añade a igraph: número de tweets por período y strength
V(my.igraph[[nperiod]])$ntweets <- my.count$N
V(my.igraph[[nperiod]])$strength <- strength(my.igraph[[nperiod]], mode = "total")
}

## Genera gráficos
set.seed(5200)
for (nperiod in 1:3) {
  my.g[[nperiod]] <- my.igraph[[nperiod]] %>%  
  ggraph(layout = 'drl') +
  #scale_label_size(minor_breaks = NULL) +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  scale_edge_width(range = c(0.1, 3))+ # control size
  #geom_edge_link(aes(alpha = weight)) +
  geom_node_point(aes(size = strength, alpha = strength)) + scale_size(range = c(1,15)) +
  geom_node_label(aes(label = name), repel = TRUE)+
  #geom_node_label(aes(label = id.aff, size = peso))+
  theme_graph() +
  #labs(title = my.newspaper), 
  labs(subtitle = my.period[nperiod])
  
my.g[nperiod]
}
my.g[[1]]
my.g[[2]]
my.g[[3]]
#ggsave(plot = my.g[[nperiod]], paste("figure_EM/EMb-", my.period[nperiod], ".png", sep = ""), units = "mm", width = 600, height = 600)

## Une gráficos en una única imagen
layout <- "
AAAABBBB
##CCCC##
"

EP.gr <- my.g[[1]] + my.g[[2]] + my.g[[3]] +
  plot_layout(ncol = 3) +
  plot_annotation(title = my.newspaper) &
  theme(plot.tag = element_text(size = 24))
EP.gr
ggsave(plot = EP.gr, "figure_EP/EPb.png", units = "mm", width = 290, height = 210)


save.image("001-network-EP.RData")

