# Dieses Skript kombiniert die Informationen über die Video-Untertitel mit den
# Video-Metadaten.

library(tidyverse)

# Lade die abgefragten Meta Daten und Untertitel:
M <- readRDS("Video_Meta_All.rds")
C <- readRDS("Captions_All.rds") 


C <- C[,c("video_id", "text")]

df_joined <- left_join(M, C, by = c("video_id")) 

# Zusammengeführten Datensatz speichern
saveRDS(df_joined, file = "Video_Meta_Captions_All.rds")
