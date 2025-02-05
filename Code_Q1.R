#packages
library(jsonlite)
library(lubridate)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(psych)
library(geomtextpath)
library(treemapify)
library(dplyr)
#install.packages("dplyr")


#zet hier het pad naar de map waarin je de Spotify data hebt opgeslagen.
setwd("C:/Users/floor/Documents/Studio Data/Jobs/2024_Mediawijs/Dig-ItUp/Spotify Extended Streaming History")

##Ontsluiten van data in json naar data waarmee je in R aan de slag kant. 
streamHistory23_24 <- fromJSON("Streaming_History_Audio_2023-2024_2.json", flatten = TRUE)


#VRAAG: VARIEERT DE INTENSITEIT VAN LUISTEREN NAAR MIJN FAVORIETE ARTIESTEN UIT SPOTIFY WRAPPED DOORHEEN DE MAANDEN? 
#Wat kan je hieruit halen
#- enkel muziek, dus als master_metadata_track_name leeg is in de dataset (NA), dan is het een podcast. Dit willen we eruit filteren. 
#- enkel bij master_metadata_album_artist_name behouden we de namen van de artiesten uit de Spotify Wrapped: The National, Bears Den, Bon Iver, Daughter en Brihang
#- aggreren per artiest per maand (nl. intensiteit per maand, in de zin van luisterminuten)


#VOORBEREIDEN VAN DE DATASET HIERVOOR
##enkel muziek
streamHistory23_24_music <- streamHistory23_24 %>%
  filter(!is.na(master_metadata_track_name))

  
##data aggregeren per maand en artiest (zicht krijgen per artiest, per maand, intensiteit in luisterminuten) = uiteindelijke dataset
streamHistory_per_month <- streamHistory23_24_music %>%
  mutate(month = floor_date(as_datetime(ts), "month")) %>%  # Zet 'ts' om naar maand
  group_by(month, master_metadata_album_artist_name) %>%  # Groepeer per maand en artiest
  summarise(total_minutes = sum(ms_played, na.rm = TRUE) / 60000, .groups = "drop") %>%  # Zet ms om naar minuten
  arrange(month, desc(total_minutes))  # Sorteer per maand en meest beluisterde artiest bovenaan

##enkel Wrapped-artiesten
streamHistory_per_month_selected <- streamHistory_per_month %>%
  filter(master_metadata_album_artist_name %in% c("The National", "Bear's Den", "Bon Iver", "Daughter", "Brihang"))

##makkelijker dan vergelijken in tabel = visualiseren (op twee manieren hieronder)
#lijngrafiek
streamHistory_per_month_selected %>% 
ggplot(aes(x = month, y = total_minutes, group = master_metadata_album_artist_name, color = master_metadata_album_artist_name)) + 
  geom_line()

#heatmap
streamHistory_per_month_selected %>%
  ggplot(aes(x = month, 
             y = master_metadata_album_artist_name, 
             fill = total_minutes)) + 
  geom_tile() + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Kleurintensiteit afhankelijk van luistertijd
  labs(title = "Luistertijd per maand per artiest",
       x = "Maand",
       y = "Artiest",
       fill = "Totale Luistertijd (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Draai maandlabels voor leesbaarheid


#binnen alle artiesten, maandniveau: top 5 per maand? 
streamHistory_per_month


