######
### Conceptual structure
#######################3

library(tidyverse)
library(bibliometrix)

load("twitter-wos-003.RData")

wos.fil <- wos %>%
  filter(!is.na(PY))

rm(wos)

wos.fil.p1 <- wos.fil %>%
  filter(PY>=2006 & PY< 2012)

wos.fil.p1 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

wos.fil.p2 <- wos.fil %>%
  filter(PY>=2012 & PY< 2015) 

wos.fil.p2 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

wos.fil.p3 <- wos.fil %>%
  filter(PY>=2015 & PY< 2018) 

wos.fil.p3 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

wos.fil.p4 <- wos.fil %>%
  filter(PY>=2018 & PY<= 2020)

wos.fil.p4 %>% 
  group_by(PY) %>% 
  summarise(n = n()) 

CS <- conceptualStructure(wos.fil.p1,field="DE", method="CA", minDegree=10, clust=5, stemming=FALSE, labelsize=10, documents=10)

CS
save(list = ls(all = TRUE), file= "twitter-wos-conceptual-structure-001.RData")
