#####################
## Cálculo RGR
#####################

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork) # Posicionamiento gráficos

## Cargar los datos generados hasta el momento ##
attach('twitter-wos-004.RData'); wos <- wos; detach('file:twitter-wos-004.RData')

## Cálculo RGR & DT
pubs <- wos %>% 
  group_by(PY) %>% 
  filter(!is.na(PY) ) %>% 
  summarise(Publications = n()) %>% 
  mutate(cum = cumsum(Publications)) %>% 
  mutate(RGR = log(cum / lag(cum))) %>% 
  mutate(DT = log(2) / RGR) %>% 
  rename(Year = PY) %>% 
  mutate(Year = as.character(Year))

###round RGR and DT
# pubs$RGR <- round(pubs$RGR, 2)
# pubs$DT <- round(pubs$DT, 2)

apatheme = theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(family = 'sans'),
    legend.title = element_blank(),
    legend.position = c(.7, .8),
    axis.line.x = element_line(color = 'black'),
    axis.line.y = element_line(color = 'black')
  )

## Pubs by year (with 2020)
pubs %>% 
  ggplot(aes(x=Year, y =Publications, group = 1)) +
      geom_line() +
    geom_point() +
    geom_label_repel(aes(label = Publications)) +
    labs(title = "Publications by year") +
     theme_bw() #+
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) 

#ggsave("outputs/pubs-year-01.pdf")

## Pubs by year (without 2020)
pubs.year <- pubs %>% 
  filter(Year != 2020) %>% 
  ggplot(aes(x=Year, y =Publications, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = Publications), hjust = 1.3, vjust = 1.3) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
  labs(title = "Publications by year") +
   theme_bw()
#ggsave("outputs/pubs-year-02.pdf")

## RGR
pubs.rgr <- pubs %>% 
    filter(Year != 2020) %>% 
    ggplot(aes(x=Year, y =RGR, group = 1)) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(label = round(RGR,2)), hjust = 1.3, vjust = 1.3) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    labs(title = "Relative Growth Rate") +
     theme_bw()
#ggsave("outputs/pubs-RGR.pdf")

## DT
pubs.dt <-pubs %>% 
  filter(Year != 2020) %>% 
  ggplot(aes(x=Year, y =DT, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = round(DT,2)), hjust = 1.3, vjust = 1.3) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
  labs(y = "Years", title = "Doubling time") +
  theme_bw()
#ggsave("outputs/pubs-DT.pdf")

#(pubs.year + pubs.rgr) / (pubs.dt + plot_spacer())

layout <- "
AAAABBBB
CCCCDDDD
"
rgr <- pubs.year + pubs.rgr + pubs.dt + nauthors.g +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') 
  plot_layout(widths = 2)
rgr
#ggsave("outputs/twitter-wos-RGR.pdf")

ggsave(plot = rgr, "outputs/twitter-wos-RGR-2.pdf", units = "mm", width = 300, height = 300)

save(list = ls(all = TRUE), file= "twitter-wos-RGR-001.RData")


