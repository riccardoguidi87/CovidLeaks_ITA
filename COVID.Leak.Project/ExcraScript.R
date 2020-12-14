### extra script
## import and cleaning data
# Install

library(wesanderson)
library(RColorBrewer)
library(tidyverse)
library(xlsx)

display.brewer.pal(n = 8, name = 'RdBu') # the economist pallette
display.brewer.pal(n = 8, name = 'Spectral')
display.brewer.all()

df <- read_csv("Release_1/LOMBARDIA/SEGNALAZIONE 1/#10 - Covidelaks - Lombardia upd. giugno 2020 - Foglio1.csv")
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

# Filter for provinces that fell into a category # DONE

x <- df%>% ## define provinces that are to be considered
  group_by(Provincia) %>%
  summarise(count = n()) %>%
  filter(count > 10) %>%
  filter(!is.na(.)) %>%
  .[[1]]

df %>%
  filter(Provincia %in% x) %>%
  
  ggplot(aes(x=ProvName, y= Età, fill = Sesso)) +
  geom_boxplot(outlier.shape = NA) + 
  #geom_jitter(alpha=0.1) +
  
  ## labels
  labs(title = "Decessi da COVID19 fino a Giugno 2020",
       subtitle ="Regione Lombardia",
       caption = "Fonte: COVIDLeaks: https://segnalazioni.soccorso-civile.it - Ass. Luca Coscioni\nChart: Riccardo Guidi",
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
        
        legend.position = c(0.95,0.15),
        legend.background = element_blank()
  )

df %>%
  filter(Provincia %in% x) %>%
  group_by(Provincia) %>%
  summarise(count=n(),
            oncologico=sum(`Altre Patologie Oncologiche`,na.rm = TRUE),
            cardio = sum(`Altre patologie Cardiovascolari`, na.rm = TRUE),
            diabete = sum(`Diabete o Metaboliche`, na.rm = TRUE),
            respiratorie = sum(`Altre Patologie Respiratorie`, na.rm = TRUE)) %>%
  gather(malattia,conteggio,oncologico, cardio, diabete, respiratorie) %>%
  
  ggplot(aes(x=Provincia, y=conteggio, fill=malattia)) +
  geom_col(position = "fill") +
  scale_fill_brewer(brewer.pal(n = 4, name = "Spectral")) +
  #geom_jitter(alpha=0.1) +
  
  labs(title = "Lombardia - Decessi COVID19 fino a Giugno 2020",
       subtitle = "Fonte: COVIDLeaks - Ass. Luca Coscioni | Chart: Riccardo Guidi") +
  theme_bw(base_line_size = 0,base_rect_size = 0)+
  theme(axis.text = element_text(size = 20, face = "bold",family = "sans"),
        axis.title = element_text(size = 15, face = "bold",family = "sans"))


###
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
