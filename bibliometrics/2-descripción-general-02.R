#####################################x
## Análisis bibliométrico twitter:  #x
##     Descriptivos referencias     #x
#####################################x

####Bibliotecas ####

library(bibliometrix)
library(tidyverse)
library(magrittr)
library(ggrepel)
library(patchwork) # Posicionamiento gráficos

## Para grágicos
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

#### Cargar datos ####
#####################x
attach('twitter-wos-004.RData'); wos <- wos; detach('file:twitter-wos-004.RData')

#### Creación variable "period" ####
wos <- wos %>% 
  mutate (period = case_when(
    PY < 2013 ~ 1,
    PY >2012 &  PY <2017 ~ 2,
    PY > 2016 ~ 3,
  ))

#####################x
#### Bibliometrix ####
#####################x

bibliometrics.wos <- wos %>%
  ##filter(PY == 2020) %>%
  ## Crea elemento bibliometrics a partir de tabla wos
  biblioAnalysis(sep = ";")

#### Crea datos adicionales ####
wos <- metaTagExtraction(wos, Field = "SR", sep = ";") ## Primer autor + año + revista
wos <- metaTagExtraction(wos, Field = "AU_UN", sep = ";") ## Afiliaciones
wos <- metaTagExtraction(wos, Field = "AU_CO", sep = ";") ## Países autores

save(list = ls(all = TRUE), file= "twitter-wos-descriptivos-001.RData")

#### Cuenta Autores ####
cuenta.AU <- as.data.frame(tableTag(wos, Tag = "AU", sep = ";"))

## Autores por periodo
x <- list()
for (i in 1:3) {
  x[[i]] <- wos %>% 
    filter(period == i) %>% 
    tableTag(Tag = "AU", sep = ";") %>% 
    as.data.frame() %>% 
    mutate(period = i)
}

autores <- data.frame()
for (i in 1:3) {
  autores <- bind_rows(autores, x[[i]])
}

cuenta.AU.period <- autores %>% 
  rename(AU = Tab) %>% 
  pivot_wider(names_from = period, values_from =  Freq) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(AU, "Period 1", "Period 2","Period 3", Total) %>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-AU-period.csv")

#### Most cited first authors ####
#citations_AU <- citations(wos, field = "author", sep = ";")

#### Cuenta Times Cited ####
cuenta.TC <- bibliometrics.wos$MostCitedPapers
cuenta.TC %<>% 
  filter(TC >10) %>% 
  mutate(year = as.numeric(str_extract(`Paper         `, "\\d{4}"))) 

cuenta.TC %<>% 
  mutate(NewTCperYear = cuenta.TC$TC /(2020-cuenta.TC$year))

write_csv(cuenta.TC, "outputs/cuenta-TC.csv")  

## Cuenta TC period
cuenta.TC.period <- wos %>% 
  select(SR, TC, period) %>% 
  mutate(TC = as.integer(TC)) %>% 
  pivot_wider(names_from = period, values_from =  TC, ) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  select(c(SR, p1, p2, p3))%>% 
  replace(., is.na(.), 0)%>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
# %>% 
#   mutate(rank_p1 = min_rank(-p1)) %>%   
#   mutate(rank_p2 = min_rank(-p2)) %>%   
#   mutate(rank_p3 = min_rank(-p3)) %>% 
#   mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
#   mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
#   mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
#   select(SR, "Period 1", "Period 2","Period 3", Total) %>% 
#   slice(1:20) %>% 
  write_csv("outputs/cuenta-TC-period.csv")  

## Average citation rate normalized by time
cuenta.TC.au <- wos %>% 
  filter(PY > 2000 & PY <2020) %>% 
  group_by(PY) %>% 
  summarise(
    TCtot = sum(as.numeric(TC)),
    Docs = n())

cuenta.TC.au %<>% 
  mutate(CitesByDocs = TCtot / Docs,
         CitesRel = TCtot / (2020-PY),
         CitesByDocsRel = CitesByDocs / (2020-PY)) 
write_csv(cuenta.TC.au, "outputs/cuenta_TC_au.csv")


cuenta.tc.au.g <- cuenta.TC.au %>% 
  ggplot(aes(x=as.factor(PY), y =CitesByDocsRel, group = 1)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = round(CitesByDocsRel, digits = 2))) +
  labs(title = "Average citation rate", x = "Year", y = "Cites by document") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size =12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

cuenta.tc.au.g

ggsave(plot = cuenta.tc.au.g, "outputs/cuenta_tc_au.pdf", units = "mm", width = 300, height = 300)


#### Cuenta Most cited references ####
cuenta.CR <-wos %>% 
  citations(field = "article", sep = ";") %$% 
  as.data.frame(Cited)

# Most cited references por periodo
x <- list()
for (i in 1:3) {
  x[[i]] <- wos %>% 
    filter(period == i) %>% 
    citations(field = "article", sep = ";") %$% 
    as.data.frame(Cited) %>% 
    mutate(period = i)
}

cited.ref <- data_frame()
for (i in 1:3) {
  cited.ref <- bind_rows(cited.ref, x[[i]])
}

cuenta.CR.period <- cited.ref %>% 
  pivot_wider(names_from = period, values_from =  Freq) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(CR, "Period 1", "Period 2","Period 3", Total)%>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-CR-period.csv")

#### Cuenta afiliaciones autores ####
cuenta.AFF <- as.data.frame(tableTag(wos, Tag = "AU_UN"))

# x <- strsplit(wos$AU_UN, ";")
# lista.AU_UN <- as.data.frame(rapply(x, function(x) list(x)))
# cuenta.AU_UN <- lista.AU_UN %>% 
#   rename(AU_UN = starts_with("r")) %>% 
#   group_by(AU_UN) %>%
#   summarise(n = n()) %>% 
#   arrange(desc(n))
# cuenta.AU_UN

## Afiliaciones autores por periodo
x <- list()
for (i in 1:3) {
  x[[i]] <- wos %>% 
    filter(period == i) %>% 
    metaTagExtraction(Field = "AU_UN", sep = ";") %>% 
    tableTag(Tag = "AU_UN") %>% 
    as.data.frame() %>% 
    mutate(period = i)
}

institutions <- data.frame()
for (i in 1:3) {
  institutions <- bind_rows(institutions, x[[i]])
}

institutions <- institutions %>% 
  rename(AFF = Tab) %>% 
  pivot_wider(names_from = period, values_from =  Freq) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(AFF, "Period 1", "Period 2","Period 3", Total) %>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-AFF-period.csv")

#### Cuenta países autores !!!  ####
## No coincide con cálculos bibliometrix

x <- strsplit(wos$AU_CO, ";")
lista.AU_CO <- as.data.frame(rapply(x, function(x) list(x)))
cuenta.AU_CO <- lista.AU_CO %>% 
  rename(AU_CO = starts_with("r")) %>% 
  group_by(AU_CO) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))
cuenta.AU_CO

#### Cuenta Sources ####
### HAY QUE DEPURAR: TODAS LAS CONFERENCIAS ANUALES UNIRLAS EN UNA 
### P.EJ.: "2012 IEEE/ACM" Y "2013 IEEE/ACM" PASAR A "IEEE/ACM"

cuenta.SO <-wos %>% 
  group_by(SO, period) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))
cuenta.SO

## Sources period
cuenta.SO.period <-wos %>% 
  group_by(SO, period) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = period, values_from =  n) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(SO, "Period 1", "Period 2","Period 3", Total) %>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-SO-period.csv")

# cuenta.SO.period %>% 
#   summarize(max_p3 = max(p3, na.rm = TRUE),
#             max_p2 = max(p2, na.rm = TRUE),
#             max_p1=max(p1, na.rm = TRUE))



#### Cuenta palabra clave DE ####
cuenta.DE <- as.data.frame(tableTag(wos, Tag = "DE"))

## DE por periodo
x <- list()
for (i in 1:3) {
  x[[i]] <- wos %>% 
    filter(period == i) %>% 
    tableTag(Tag = "DE", sep = ";") %>% 
    as.data.frame() %>% 
    mutate(period = i)
}

de <- data.frame()
for (i in 1:3) {
  de <- bind_rows(de, x[[i]])
}

cuenta.DE.period <- de %>% 
  rename(DE = Tab) %>% 
  pivot_wider(names_from = period, values_from =  Freq) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(DE, "Period 1", "Period 2","Period 3", Total) %>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-DE-period.csv")

### recuento adicional DE e ID

cuenta.DE %>% 
  summarise(total_de = sum(Freq))

cuenta.ID %>% 
  summarise(total_id = sum(Freq))

de %>% 
  group_by(period) %>% 
  summarise(total = sum(Freq), n = n())

id %>% 
  group_by(period) %>% 
  summarise(total = sum(Freq), n = n())


#### Cuenta keywords plus ID ####
cuenta.ID <- as.data.frame(tableTag(wos, Tag = "ID"))

# x <- strsplit(wos$ID, "[;]")
# lista.ID <- as.data.frame(rapply(x, function(x) list(x)))
# cuenta.ID <-lista.ID %>% 
#   rename(ID = starts_with("r")) %>% 
#   group_by(ID) %>%
#   summarise(n = n()) %>% 
#   arrange(desc(n))
# cuenta.ID

## ID por periodo
x <- list()
for (i in 1:3) {
  x[[i]] <- wos %>% 
    filter(period == i) %>% 
    tableTag(Tag = "ID", sep = ";") %>% 
    as.data.frame() %>% 
    mutate(period = i)
}

id <- data.frame()
for (i in 1:3) {
  id <- bind_rows(id, x[[i]])
}

cuenta.ID.period <- id %>% 
  rename(ID = Tab) %>% 
  pivot_wider(names_from = period, values_from =  Freq) %>% 
  rename(p1 = `1`) %>% 
  rename(p2 = `2`) %>% 
  rename(p3 = `3`) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total =rowSums(.[2:4], na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  mutate(rank_p1 = min_rank(-p1)) %>%   
  mutate(rank_p2 = min_rank(-p2)) %>%   
  mutate(rank_p3 = min_rank(-p3)) %>% 
  mutate("Period 1" = paste(p1, " (", rank_p1, ")", sep = "")) %>% 
  mutate("Period 2" = paste(p2, " (", rank_p2, ")", sep = "")) %>% 
  mutate("Period 3" = paste(p3, " (", rank_p3, ")", sep = "")) %>% 
  select(ID, "Period 1", "Period 2","Period 3", Total) %>% 
  slice(1:20) %>% 
  write_csv("outputs/cuenta-ID-period.csv")

#### Cuenta Research areas ####
cuenta.SC <- as.data.frame(tableTag(wos, Tag = "SC"))
write_csv(cuenta.SC, "outputs/cuenta-SC.csv")

#### Cuenta WoS Cathegory ####
cuenta.WC <- as.data.frame(tableTag(wos, Tag = "WC"))
write_csv(cuenta.WC, "outputs/cuenta-WC.csv")

### Authors by year
nauthors <- as_tibble(bibliometrics.wos$nAUperPaper)
nauthors %<>%
  mutate(Year = wos$PY) %>% 
  filter(!is.na(Year) & Year < 2020) %>% 
  group_by(Year) %>% 
  summarise(TotYear = sum(value), ndocs = n()) %>% 
  mutate(AuthorsMean = TotYear / ndocs)


nauthors.g <- nauthors %>% 
  ggplot(aes(x=as.factor(Year), y =AuthorsMean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = round(AuthorsMean, digits = 2)), size = 8) +
  labs(title = "Mean authors by doc & year", x = "Year", y = "Authors") +
  apatheme +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) 

nauthors.g

ggsave(plot = nauthors.g, "outputs/nauthorsYear-02.pdf", units = "mm", width = 300, height = 300)

#### Gráficos ####
wos  %>% 
  group_by(PY) %>% summarise(n = n()) %>%
  ggplot(aes(x = PY, y = n)) +
  geom_line() +
  geom_point() +
  labs(title = "Publications by year", x = "Year",
       y = "Publications") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 9),
        legend.position = "bottom") #+
  scale_x_discrete(labels = waiver())


  
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
    geom_label_repel(aes(label = Publications), hjust = 1.3, vjust = 1.3) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    labs(title = "Publications by year") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  #ggsave("outputs/pubs-year-02.pdf")
  
  ## RGR
  pubs.rgr <- pubs %>% 
    filter(Year != 2020) %>% 
    ggplot(aes(x=Year, y =RGR, group = 1)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = round(RGR,2)), hjust = 1.3, vjust = 1.3) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    labs(title = "Relative Growth Rate") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  #ggsave("outputs/pubs-RGR.pdf")
  
  ## DT
  pubs.dt <-pubs %>% 
    filter(Year != 2020) %>% 
    ggplot(aes(x=Year, y =DT, group = 1)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = round(DT,2)), hjust = 1.3, vjust = 1.3) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    labs(y = "Years", title = "Doubling time") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1, size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  #ggsave("outputs/pubs-DT.pdf")
  
  layout <- "
AAAABBBB
CCCCDDDD
"
  rgr <- pubs.year + pubs.rgr + pubs.dt + cuenta.tc.au.g +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = 'A') 
  plot_layout(widths = 2)
  rgr
  #ggsave("outputs/twitter-wos-RGR.pdf")
  
ggsave(plot = rgr, "outputs/twitter-wos-RGR-4.pdf", units = "mm", width = 300, height = 300)
  

###### Guardar RData ########
save(list = ls(all = TRUE), file= "twitter-wos-descriptivos-003.RData")

