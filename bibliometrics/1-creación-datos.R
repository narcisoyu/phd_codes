##################################################
## Análisis bibliométrico:                      ##
##    Carga datos brutos                        ##
##    y modificaciones generales datos          ##
##################################################

#Bibliotecas
#library(stringr)
#library(igraph)
#library(dplyr) # Para manipulación tablas
#library(knitr) #opciones markdown, por ejemplo tablas
##Opcional, sólo si se va a generar PDFs con markdwon
#library(extrafont) #Para cargar fuentes
library(bibliometrix)
library(readODS) # Para leer ODS "reemplazar" con la depuración keywords
library(DataCombine) # Para hacer la depuración
library(stringr) # Para hacer pasos previos a la depuración
library(tidyverse)
library(textstem)
# library(SnowballC)

#carga datos texto plano (sin formato)
#largechar <- readLines("../twitter-wos-articulos.txt")
largechar <- readFiles("datos/twitter-bib-tot.txt")
## Crea tabla wos a partir de los datos cargados
wos <- isi2df(largechar) #Convierte en Data con bibliometrix

## Elimina "largechar"
remove(largechar)
save(list = ls(all = TRUE), file= "twitter-wos-001.RData")
## Podemos cargar los datos generados hasta el momento sin necesidad de "carga datos texto plano"
## load('twitter-wos-articulos-01.RData')

###################################################
## Depurar palabras clave autores(DE) y kw+ (ID) ##
###################################################

## Primero copia los campos originales para conservar copia

wos$DE_orig <- wos$DE
wos$ID_orig <- wos$ID

# Sustituye espacions en blanco por guiones
wos$DE  <- str_replace_all(wos$DE, "\\s", "-")
wos$DE  <- str_replace_all(wos$DE, ";-", "; ")
wos$DE  <- str_replace_all(wos$DE, "; ", ";")
wos$ID  <- str_replace_all(wos$ID, "\\s", "-")
wos$ID  <- str_replace_all(wos$ID, ";-", "; ")
wos$ID  <- str_replace_all(wos$ID, "; ", ";")

# wos$DE <- str_replace_all(wos$DE, ";;", ";")
# wos$DE <- str_replace_all(wos$DE, ";\\s*", "#####")
# wos$DE <- str_replace_all(wos$DE, "\\s+", " ")
# wos$DE <- str_replace_all(wos$DE, " ", "-")
# wos$DE <- str_replace_all(wos$DE, "#####", ";")

# Carga el ods con las palabras a reemplazar
reemplazar <- as.data.frame(read_ods("reemplazar.ods"))

# Para que el reemplazo funciones, hay que encontrar la forma de que la búsqueda sólo de resultados con palabra completa
reemplazar$desde <- (paste(";", reemplazar$desde, ";", sep =""))
reemplazar$hacia <- (paste(";", reemplazar$hacia, ";", sep =""))

#también hay que añadir el ";" al inicio y final de los kw
wos$DE <- (paste(";", wos$DE, ";", sep =""))
wos$ID <- (paste(";", wos$ID, ";", sep =""))

# Ejecuta la depuración
wos$DE <- FindReplace(data= wos, Var= "DE", replaceData = reemplazar, from = "desde", to = "hacia", exact = FALSE, vector = TRUE)
wos$ID <- FindReplace(data= wos, Var= "ID", replaceData = reemplazar, from = "desde", to = "hacia", exact = FALSE, vector = TRUE)

save(list = ls(all = TRUE), file= "twitter-wos-002.RData")

# Quitar "twitter"
wos$DE <- str_replace_all(wos$DE, ";", "; ")
wos <- termExtraction(wos, Field = "DE", remove.terms = "TWITTER")
wos$DE <- wos$DE_TM

#wos$DE <- str_replace_all(wos$DE, ";TWITTER;", ";") # Alternativa que no funciona

wos$ID <- str_replace_all(wos$ID, ";", "; ")
wos <- termExtraction(wos, Field = "ID", remove.terms = "TWITTER")
wos$ID <- wos$ID_TM
# 
# wos$ID <- str_replace_all(wos$ID, ";TWITTER;", ";")

save(list = ls(all = TRUE), file= "twitter-wos-003.RData")

# Lemmatization
wos$DE_l <- lemmatize_strings(wos$DE)
wos$DE_l <- str_replace_all(wos$DE_l, " - ", "-")

wos$ID_l <- lemmatize_strings(wos$ID)
wos$ID_l <- str_replace_all(wos$ID_l, " - ", "-")

# Sustituye "NA" por NA (missing) (comprobar número columna)
#wos_2[,38 ][wos_2[,38]=="NA"] <- NA
#wos_2[,39 ][wos_2[,39]=="NA"] <- NA

## Elimina guiones inicio y final de kw
wos$DE <- str_replace_all(wos$DE, "-;", ";") 
wos$DE <- str_replace_all(wos$DE, ";-", ";")

wos$ID <- str_replace_all(wos$ID, "-;", ";") 
wos$ID <- str_replace_all(wos$ID, ";-", ";")

## Con la función anterior no se elimina los que están al inicio o al final del registro
## Elimina los guiones iniciales y finales

wos$DE <- str_replace_all(wos$DE, "^-", "") 
wos$DE <- str_replace_all(wos$DE, "-$", "") 
wos$ID <- str_replace_all(wos$ID, "^-", "") 
wos$ID <- str_replace_all(wos$ID, "-$", "") 

save(list = ls(all = TRUE), file= "twitter-wos-004.RData")



##