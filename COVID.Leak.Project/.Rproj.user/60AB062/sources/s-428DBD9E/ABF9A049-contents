### ISTAT Popolazione di precisa origine per provincia

library(wesanderson)
library(RColorBrewer)
library(tidyverse)
library(xlsx)
library(forcats) ## deal with factor values

display.brewer.pal(n = 8, name = 'RdBu') # the economist pallette
display.brewer.pal(n = 8, name = 'Spectral')
display.brewer.all()

istat <- read_csv("non-Covid-data/ISTAT/2020_Popolazione_DCIS_POPRES1_06122020051110210.csv")
regioni <- read_csv("non-Covid-data/Lista_Provincie.csv")
regioni$REGIONE <- str_replace(string = regioni$REGIONE, pattern = "Emilia Romagna", replacement = "Emilia-Romagna")
regioni$REGIONE <- str_replace(string = regioni$REGIONE, pattern = "Valle d'Aosta", replacement = "Valle d'Aosta / Vallée d'Aoste")
regioni$REGIONE <- str_replace(string = regioni$REGIONE, pattern = "Friuli Venezia Giulia", replacement = "Friuli-Venezia Giulia")
regioni$REGIONE <- str_replace(string = regioni$REGIONE, pattern = "Trentino Alto Adige", replacement = "Trentino Alto Adige / Südtirol")
regioni$REGIONE <- str_replace(string = regioni$REGIONE, pattern = "Trentino Alto Adige", replacement = "Trentino Alto Adige / Südtirol")

istat$Territorio <- str_replace(string = istat$Territorio, pattern = "Monza e della Brianza", replacement = "Monza-Brianza")

covid_lombardia <- read_csv("Release_1/LOMBARDIA/SEGNALAZIONE 1/#10 - Covidelaks - Lombardia upd. giugno 2020 - Foglio1.csv")

# function that converts a SIGLA to FULL.NAME # DONE
lista.provincie <- read_csv("non-Covid-data/Lista_Provincie.csv")

full.name.provincia <- function(sigla){
  x <- lista.provincie %>%
    filter(SIGLA == sigla)
  return (x[[2]][1])
}

covid_lombardia <- covid_lombardia %>%
  mutate(ProvName = sapply(X = Provincia, FUN = full.name.provincia))

### Crelate list of relevant provinces - SIGLA # DONE
rel.prov.sigla <- covid_lombardia%>% ## define provinces that are to be considered
  group_by(Provincia) %>%
  summarise(count = n()) %>%
  filter(count > 10) %>%
  filter(!is.na(.)) %>%
  .[[1]]

### Crelate list of relevant provinces - FULLNAME # DONE
rel.prov.name <- covid_lombardia%>% ## define provinces that are to be considered
  group_by(ProvName) %>%
  summarise(count = n()) %>%
  filter(count > 10) %>%
  filter(!is.na(.)) %>%
  .[[1]]

n_rel.prov <- length(rel.prov.sigla)

##create tibble with simplified age count based on ISTAT data

v <- istat %>%
  mutate_if(sapply(X=., is.character), as.factor) %>% # convert all chr to factors
  mutate(AgeValue= as.numeric(str_extract(string = ETA1, pattern = '[0123456789]+'))) %>% # Extract ETA1 into the number, convert to dbl
  mutate(ETA1 = fct_reorder(ETA1, AgeValue)) %>%  #use numerical value of AgeValue to list the factor of ETA1
  group_by(AgeValue,Territorio) %>%
  summarise(people = sum(Value)) %>%
  filter(!is.na(AgeValue)) %>%
  filter(Territorio %in% rel.prov.name) %>%
  mutate(people = floor(people/1000)) %>%
  ungroup()

##create new df that include age distribution for all relevant provinces
age_distribution <- data.frame("AgeValue"= NA,"ProvName"=NA)
for (i in 1:nrow(v)) {
  temp <- data.frame("AgeValue"= rep(v$AgeValue[i],v$people[i]),"ProvName"= rep(v$Territorio[i],v$people[i]))
  age_distribution <- rbind(temp, age_distribution)
}
age_distribution <- age_distribution[complete.cases(age_distribution), ]

### overlapping distribution charts

covid_lombardia %>%
  filter(Provincia %in% rel.prov.sigla) %>%

  ggplot(aes(x = as.numeric(Età))) +
  geom_histogram(data = as_tibble(age_distribution), aes(x = AgeValue, y=..density.. ,fill = brewer.pal(n = 8, name = 'RdBu')[3]),bins = 50, color = "black") +
  geom_density(data = as_tibble(age_distribution), aes(x = AgeValue, y=..density..),alpha=0.2, fill=brewer.pal(n = 8, name = 'RdBu')[4]) +
  
  geom_histogram(aes(y=..density.., fill = brewer.pal(n = 8, name = 'RdBu')[1]),bins = 50, color = "black",alpha = 0.5) +
  geom_density(alpha=0.2, fill=brewer.pal(n = 8, name = 'RdBu')[2]) + 
  #geom_vline(aes(xintercept = median(as.numeric(Età))),color="red", linetype="dashed", size=1) +
  facet_wrap(~ProvName) +
  
  scale_fill_identity(name = '', guide = 'legend',labels = c('Morti da COVID19','Popolazione')) +
  
  #scale

  #regression
  
  ## labels
  labs(title = "Distribuzione decessi da COVID19 a Giugno 2020",
       subtitle ="Regione Lombardia",
       caption = "\n\nFonte: COVIDLeaks: https://segnalazioni.soccorso-civile.it - Ass. Luca Coscioni e ISTAT
       Chart: @riccardoguidi87
       Dati pervenuti su CovidLeaks non sono ufficiali ne verificabili, e potrebbero essere incompleti",
       tag = "") +
  ylab("Densità \n (conteggio relativo)") +
  xlab("Età Anagrafica \n (anni)") +
  
  #theme
  theme_classic() +
  theme(panel.background = element_rect(fill = brewer.pal(n = 8, name = 'RdBu')[5]),
        panel.border = element_rect(color = "black",fill = NA, size = 1),
        axis.text.x = element_text(size=10,angle = 45,hjust = 1, colour = "black"),
        axis.text.y = element_text(size=10,angle = 0,hjust = 1, colour = "black"),
        title = element_text(colour = brewer.pal(n = 8, name = 'RdBu')[2],face = "bold"),
        axis.title = element_text(color = "black"), 
        legend.position = "top", 
        legend.text = element_text(face = "bold",size = 10)
        #legend.background = element_rect(size = 10)
        
  )

