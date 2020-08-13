#######################################
#### Utilidades para manipulación datos
#######################################



## Elimina guiones inicio y final de kw
wos$DE <- str_replace_all(wos$DE, "-;", ";") 
wos$DE <- str_replace_all(wos$DE, ";-", ";")

wos$ID <- str_replace_all(wos$ID, "-(?=;)", "") 
wos$ID <- str_replace_all(wos$ID, ";(?=-)", "")

## Con la función anterior no se elimina los que están al inicio o al final del registro
## Elimina los guiones iniciales y finales

wos$DE <- str_replace_all(wos$DE, "^-", "") 
wos$DE <- str_replace_all(wos$DE, "-$", "") 
wos$ID <- str_replace_all(wos$ID, "^-", "") 
wos$ID <- str_replace_all(wos$ID, "-$", "") 


## Extrae y cuenta todos los DE diferentes
x <- strsplit(wos$DE, ";")
## lista.DE <- as.data.frame(unique(rapply(x, function(x) head(x, 1))))
lista.DE <- as.data.frame(rapply(x, function(x) list(x)))
cuenta.DE <-lista.DE %>% 
  rename(DE = starts_with("r")) %>% 
  group_by(DE) %>%
  summarise(n = n())
cuenta.DE

## Extrae y cuenta  todos los ID diferentes
x <- strsplit(wos$ID, "[;]")
lista.ID <- as.data.frame(rapply(x, function(x) list(x)))
cuenta.ID <-lista.ID %>% 
  rename(ID = starts_with("r")) %>% 
  group_by(ID) %>%
  summarise(n = n())
cuenta.ID

