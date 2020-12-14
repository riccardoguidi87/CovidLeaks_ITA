## eta decessi vs provincia - Toscana
## import and cleaning data
# Install

library(wesanderson)
library(RColorBrewer)
library(tidyverse)
library(xlsx)

display.brewer.pal(n = 8, name = 'RdBu') # the economist pallette
display.brewer.pal(n = 8, name = 'Spectral')
display.brewer.all()

df <- read_csv("Release_1/TOSCANA/SEGNALAZIONE 1/#14 covidleaks - 10 nov 2020 - no prov.csv")
str(df)

# function that converts a SIGLA to FULL.NAME # DONE
lista.provincie <- read_csv("non-Covid-data/Lista_Provincie.csv")

full.name.provincia <- function(sigla){
  x <- lista.provincie %>%
    filter(SIGLA == sigla)
  return (x[[2]][1])
}

df <- df %>%
  mutate(ProvName = sapply(X = Provincia, FUN = full.name.provincia))

# Convert chr into dates
df$Data_Evento <- as.Date(df$Data_Evento,"%d/%m/%y")

min(df$Data_Evento)
max(df$Data_Evento)

# Filter for provinces that fell into a category # DONE

x <- df%>% ## define provinces that are to be considered
  group_by(Provincia) %>%
  summarise(count = n()) %>%
  filter(count > 10) %>%
  filter(!is.na(.)) %>%
  .[[1]]

df %>%
  filter(Provincia %in% x) %>%
  filter(!is.na(.)) %>%
  
  ggplot(aes(x=ProvName, y= Eta, fill = Sesso)) +
  geom_boxplot(outlier.shape = NA) + 
  #geom_jitter(alpha=0.1) +
  
  ## labels
  labs(title = "Distribuzione Casi Positivi SARS-CoV-19",
       subtitle ="Tra 23 Ottobre e 10 Novembre - Regione Toscana",
       caption = "Fonte: COVIDLeaks: https://segnalazioni.soccorso-civile.it - Ass. Luca Coscioni\nChart: Riccardo Guidi
       Dati pervenuti su CovidLeaks non sono ufficiali ne verificabili, e potrebbero essere incompleti",
       tag = "") +
  ylab("Età") +
  xlab("Provincia") +
  
  theme_bw(base_line_size = 0,base_rect_size = 0)+
  theme(panel.background = element_rect(fill = brewer.pal(n = 8, name = 'RdBu')[5]),
        panel.border = element_rect(color = "black",fill = NA, size = 1),
        axis.text.x = element_text(size=15,angle = 45,hjust = 1, colour = "black"),
        axis.text.y = element_text(size=15,angle = 0,hjust = 1, colour = "black"),
        title = element_text(colour = brewer.pal(n = 8, name = 'RdBu')[1],face = "bold"),
        axis.title = element_text(color = "black"),
        
        legend.position = ,
        legend.background = element_blank()
  )
