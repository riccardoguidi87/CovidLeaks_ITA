## eta decessi vs provincia - Toscana
## import and cleaning data
# Install

library(wesanderson)
library(RColorBrewer)
library(tidyverse)
library(xlsx)
library(modelr)

display.brewer.pal(n = 8, name = 'RdBu') # the economist pallette
display.brewer.pal(n = 8, name = 'Spectral')
display.brewer.all()

df <- read_csv("Release_1/VENETO/SEGNALAZIONE 1/table1.csv")

# Convert chr into dates
df$data <- str_replace(df$data,pattern = "3-Nov", replacement = "3-Nov-20")
df$data <- as.Date(df$data,"%d-%b-%y")

# Convert chr into numbers, except some columns
df <- df %>%
  mutate(across(-contains(c("Comune","data")), as.numeric)) %>%
  mutate(across(contains(c("successivo_a_caso","pos_Caso")), as.numeric))

# define provinces that are to be considered
x <- df%>% 
  group_by(Comune) %>%
  summarise(sumCasi = sum(`totale casi pos_Caso_data`)) %>%
  filter(sumCasi > 10) %>%
  filter(!is.na(.)) %>%
  .[[1]]

# filter based on above + plotting
df %>%
  filter(Comune %in% x) %>%
  mutate(isVerona = ifelse(str_detect(string = Comune, pattern = "^Verona$"),yes = "Verona",no =  "Fuori Verona")) %>%
  
  ggplot(aes(x=data, y= caso_positivo, color = Comune)) +
  #geom_smooth(method = "lm", se = FALSE,) +
  geom_line() +
  facet_wrap(~isVerona, scales = "free") +
  
  ## labels
  labs(title = "Nuovi Casi Positivi SARS-CoV-19, per comune",
       subtitle ="Novembre - Regione Veneto",
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
        
        legend.position = "NULL"
  )

