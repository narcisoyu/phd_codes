#################################################x
## Análisis bibliométrico twitter.wos:          ##
##    Mapa temático                             ##
##    Estructura conceptual                     ##
#################################################x
library(tidyverse)
library(bibliometrix)
library(ggrepel)
library(ggthemes)
library("ggsci")
library(patchwork) # Posicionamiento gráficos

#load("twitter-wos-descriptivos-002.RData")

#### Creación datos por periodo ####
## Elimina registros sin año de publicación
wos.fil <- wos %>%
  filter(!is.na(PY))

rm(wos)

## Crea tablas para cada período
wos.fil.p1 <- wos.fil %>%
  filter(period == 1)

wos.fil.p1 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

wos.fil.p2 <- wos.fil %>%
  filter(period == 2) 

wos.fil.p2 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

wos.fil.p3 <- wos.fil %>%
  filter(period == 3) 

wos.fil.p3 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

#### Thematic map 
#### (no ejecutar: se puede hacer ocn Thematic evolution)
#tm.p1 <- thematicMap(wos.fil.p1, field = "DE", n = 400, minfreq = 5,
#            stemming = TRUE, size = 0.5, n.labels = 3, repel = TRUE)

#tm.p2 <- thematicMap(wos.fil.p2, field = "DE", n = 400, minfreq = 5,
#                     stemming = TRUE, size = 0.5, n.labels = 3, repel = TRUE)

#tm.p3 <- thematicMap(wos.fil.p3, field = "DE", n = 400, minfreq = 5,
#                     stemming = TRUE, size = 0.5, n.labels = 3, repel = TRUE)

#tm.p1
#tm.p2
#tm.p3

# Clusters=tm.p1$words[order(tm.p1$words$Cluster,-tm.p1$words$Occurrences),]
# CL <- Clusters %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences)
# CL
# 
# tm.p1.g <- tm.p1$map + ggtitle("Thematic map (2006-2012)")
# tm.p2.g <- tm.p2$map + ggtitle("Thematic map (2013-2016)")
# tm.p3.g <- tm.p3$map + ggtitle("Thematic map (2017-2020)")
# 
# ggsave(plot = tm.p1.g, "outputs/tm_400_5_1.pdf", units = "mm", width = 297, height = 210)
# ggsave(plot = tm.p2.g, "outputs/tm_400_5_2.pdf", units = "mm", width = 297, height = 210)
# ggsave(plot = tm.p3.g, "outputs/tm_400_5_3.pdf", units = "mm", width = 297, height = 210)
# 
# x1 <- tm.p1.g[[1]]
# x2 <- tm.p2.g[[1]]
# x3 <- tm.p3.g[[1]]
# 
# ggplot(x1, aes(x=rcentrality,y=rdensity,colour=label,size=centrality*8,label=name_full))+
#   geom_point(size=log(x1$freq)*6, shape = 20) +
#   #geom_point(size=x$freq/20, shape = 20) +
#   #geom_text(size = 4) +
#   geom_label_repel(point.padding = 1, size = 3, color = "black") +
#   geom_vline(xintercept = mean(x1$rcentrality),linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
#   geom_hline(yintercept = mean(x1$rdensity),linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
#     #scale_x_continuous(breaks=seq(0, max(x$rcentrality)/2, 1)) +
#   labs(x = "Centrality", y= "Density", title = "Thematic map 2006-2012")+ 
#   scale_radius(range=c(5*(1+4), 30*(1+4)))+
#   theme_bw() +
#   theme(legend.position = "none", 
#         axis.text.y =  element_blank(), 
#         axis.text.x =  element_blank(),
#         axis.ticks = element_blank()) +
#   #scale_colour_brewer(palette="Set3")
#   #scale_color_ucscgb()
#   #scale_color_igv()
#   scale_color_gsea()

#### Thematic Evolution ####
years = c(2012, 2016)
te_400_5 <- thematicEvolution(wos.fil, field = "DE", years = years, n = 200, minFreq = 15,
                  size = 0.2, stemming = TRUE, n.labels = 3, repel = TRUE)

#te_400_5 <- te
#te_500_7 <- te

te_400_5
z <- plotThematicEvolution(te_400_5$Nodes,te_400_5$Edges) ## Gráfico de aluvión
z$x$options$colourScale

## Muestra los miembros de los clusters
clusters <- list()
CL <- list()
for (i in 1:3) {
  clusters[[i]] <- te_400_5$TM[[i]]$words[order( te_400_5$TM[[i]]$words$Cluster, - te_400_5$TM[[i]]$words$Occurrences),]
  CL[[i]] <- clusters[[i]] %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences)
}



tit <- c("Thematic map 2006-2012", "Thematic map 2013-2016", "Thematic map 2017-2020")
tm.g <- list()
for (i in 1:3) {
tm  <- te_400_5$TM[[i]]$clusters
tm.g[[i]] <- ggplot(tm, aes(x=rcentrality,y=rdensity,colour=label,label=name_full))+
  geom_point(size=log(tm$freq)*4, shape = 20) +
  #geom_point(size=x$freq/20, shape = 20) +
  #geom_text(size = 4) +
  geom_label_repel(point.padding = 1, size = 9, color = "black") +
  geom_vline(xintercept = mean(tm$rcentrality),linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
  geom_hline(yintercept = mean(tm$rdensity),linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
  #scale_x_continuous(breaks=seq(0, max(x$rcentrality)/2, 1)) +
  labs(x = "Centrality", y= "Density", title = tit[[i]], size = 8)+ 
  #scale_radius(range=c(5*(1+4), 30*(1+4)))+
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.y =  element_blank(), 
        axis.text.x =  element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_text(size=20),
        plot.title=element_text(size=24),
        axis.title = element_text(size=22))+
  
  #scale_colour_brewer(palette="Set3")
  #scale_color_ucscgb()
  #scale_color_igv()
  scale_color_gsea()
}

layout <- "
AAAABBBB
##CCCC##
"
tm.gr <- tm.g[[1]] + tm.g[[2]] + tm.g[[3]] +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24))
plot_layout(widths = 2)
tm.gr

ggsave(plot = tm.gr, "outputs/twitter-wos-TM-3.pdf", units = "mm", width = 600, height = 600)

# te_500_7
# plotThematicEvolution(te_500_7$Nodes,te_500_7$Edges)

###conceptual structure
CS.p1 <- conceptualStructure(wos.fil.p1, field = "DE", method = "MCA",
                             quali.supp = NULL, quanti.supp = NULL, minDegree = 100,
                             clust = "auto", k.max = 8, stemming = TRUE, labelsize = 10,
                             documents = 1001, graph = TRUE)

CS.p2 <- conceptualStructure(wos.fil.p2, field = "DE", method = "MCA",
                    quali.supp = NULL, quanti.supp = NULL, minDegree = 200,
                    clust = "auto", k.max = 8, stemming = TRUE, labelsize = 10,
                    documents = 8143, graph = TRUE)

CS.p3 <- conceptualStructure(wos.fil.p3, field = "DE", method = "MCA",
                    quali.supp = NULL, quanti.supp = NULL, minDegree = 200,
                    clust = "auto", k.max = 8, stemming = TRUE, labelsize = 10,
                    documents = 9214, graph = TRUE)

save(list = ls(all = TRUE), file= "twitter-wos-thematic-evolution-002.RData")
