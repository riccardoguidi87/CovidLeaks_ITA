library(wesanderson)
library(RColorBrewer)
library(tidyverse)
library(xlsx)
library(forcats) ## deal with factor values

display.brewer.pal(n = 8, name = 'RdBu') # the economist pallette
display.brewer.pal(n = 8, name = 'Spectral')
display.brewer.all()

istat <- read_csv("non-Covid-data/ISTAT/2020_Popolazione_DCIS_POPRES1_06122020051110210.csv")

istat %>%
  mutate_if(sapply(X=., is.character), as.factor) %>% # convert all chr to factors
  mutate(AgeValue= as.numeric(str_extract(string = ETA1, pattern = '[0123456789]+'))) %>% # Extract ETA1 into the number, convert to dbl
  mutate(ETA1 = fct_reorder(ETA1, AgeValue)) %>%  #use numerical value of AgeValue to list the factor of ETA1
  group_by(AgeValue) %>%
  summarise(people = sum(Value)) %>%
  filter(!is.na(AgeValue)) %>%

  ggplot(aes(x = as.numeric(AgeValue), y=(people/10^6))) +
  geom_point(color = brewer.pal(n = 8, name = 'RdBu')[1]) +
  
  #scale
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  
  #regression
  #stat_smooth(method = "lm", formula = y ~ poly(x, 20), se = FALSE, color = brewer.pal(n = 8, name = 'RdBu')[3], alpha = 1) +
  #stat_smooth(geom="line", alpha = 0.5) +
  geom_line(stat = "smooth",
              method = "lm", formula = y ~ poly(x, 20), se = FALSE, 
              color = brewer.pal(n = 8, name = 'RdBu')[3], 
              alpha = 0.5,
              linetype = "solid",
              size = 3
              ) +

  ## labels
  labs(title = "Età dei Residenti in Italia - per Gennaio 2020",
       subtitle ="",
       caption = "Fonte: ISTAT - Chart: Riccardo Guidi",
       tag = "") +
  ylab("Popolazione \n (milioni)") +
  xlab("Età Anagrafica \n (anni)") +
  
  #theme
  theme_classic() +
  theme(panel.background = element_rect(fill = brewer.pal(n = 8, name = 'RdBu')[5]),
        axis.text.x = element_text(size=15,angle = 45,hjust = 1, colour = "black"),
        axis.text.y = element_text(size=15,angle = 0,hjust = 1, colour = "black"),
        title = element_text(colour = "red",face = "bold"),
        axis.title = element_text(color = "black")
        )



