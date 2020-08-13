######################################
## Análisis bibliométrico twitter:  ##
##     Descriptivos referencias     ##
######################################

####Bibliotecas ####

library(bibliometrix)
library(tidyverse)
library(magrittr)

## Cargar los datos generados hasta el momento ##
#################################################
attach('twitter-wos-004.RData'); wos <- wos; detach('file:twitter-wos-004.RData')

# wos.tidy <- as_tibble(wos)
# ## Publicaciones por año
# wos.tidy %>%
#   group_by(PY) %>%
#   summarise(n = n()) %>%
#   plot()
# 
# wos.tidy %>%
#   group_by(DT) %>%
#   summarise(n = n()) 
# 
# sinany <- wos.tidy %>%
#   filter(is.na(PY))
######################################
### BIBLIOMETRICS con Bibliometrix ###
######################################

## Creación variable "period"
wos <- wos %>% 
  mutate (period = case_when(
    PY < 2013 ~ 1,
    PY >2012 &  PY <2017 ~ 2,
    PY > 2016 ~ 3,
  ))


bibliometrics.wos <- wos %>%
  ##filter(PY == 2020) %>%
  ## Crea elemento bibliometrics a partir de tabla wos
  biblioAnalysis(sep = ";")

save(list = ls(all = TRUE), file= "twitter-wos-descriptivos-001.RData")


# b <- wos %>%
#   #filter(PY == 2020) %>%
#   #as_tibble() %>%
#   citations(field = "author", sep = ";") %$%
#   as_tibble(Cited) 
#   
# x <- wos.tidy %>%
#   filter(PY == 2020) %$%
#   mutate(citations_AU = citations(., field = "author", sep = ";"))
# 
# # Most cited references
# citations_ART <-citations(wos, field = "article", sep = ";")
# 
# # Local citations
# # How many times an author included in this collection have been cited by other authors also in the collection.
# localCitations <- localCitations(wos, sep = ";")
  

## Crea datos adicionales

# Most cited first authors
#citations_AU <- citations(wos, field = "author", sep = ";")

# Most cited references
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

# Local citations
# How many times an author included in this collection have been cited by other authors also in the collection.
#localCitations <- localCitations(wos, sep = ";")

### Descriptivos

## Autores
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

## Afiliaciones autores
wos <- metaTagExtraction(wos, Field = "AU_UN", sep = ";")
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

## Países autores !!! No coincide con cálculos bibliometrix 
wos <- metaTagExtraction(wos, Field = "AU_CO", sep = ";")
x <- strsplit(wos$AU_CO, ";")
lista.AU_CO <- as.data.frame(rapply(x, function(x) list(x)))
cuenta.AU_CO <- lista.AU_CO %>% 
  rename(AU_CO = starts_with("r")) %>% 
  group_by(AU_CO) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))
cuenta.AU_CO

### HAY QUE DEPURAR: TODAS LAS CONFERENCIAS ANUALES UNIRLAS EN UNA 
### P.EJ.: "2012 IEEE/ACM" Y "2013 IEEE/ACM" PASAR A "IEEE/ACM"
## Sources
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






## Extrae y cuenta todos los DE diferentes
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

## Extrae y cuenta  todos los ID diferentes
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

## Research areas
cuenta.SC <- as.data.frame(tableTag(wos, Tag = "SC"))

## WoS Cathegory
cuenta.WC <- as.data.frame(tableTag(wos, Tag = "WC"))

# Gráficos
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

save(list = ls(all = TRUE), file= "twitter-wos-descriptivos-002.RData")

