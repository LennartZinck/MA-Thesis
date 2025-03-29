###############################################################################
# Dieses Skript fragt die Video-ID's der vordefinierten Liste von 
# YouTube Kanälen aus dem Skript 1_Get_Channel_IDs.R über die YouTube API ab.
###############################################################################

library(tidyverse)
library(tuber)

# Authentifizierung des YouTube API Accounts
# Entfernen der cached Authentifizierung, falls notwendig:
# system("rm .httr-oauth")
# system("del .httr-oauth")
yt_oauth(client_id, client_secret, token = "")

# Laden der csv, die in 1_Get_Channel_IDs.R erstellt wurde
C <- read_csv("Channel_IDs.csv")


# Mit dieser for-loop wirde die Liste der (aktuellen) Video_IDs eines Kanals abgefragt
for(i in 1:nrow(C)) {
  
  # Channel Ressourcen (contentDetails) enhalten die Video IDs in Playlist IDs
  channel_resources <- list_channel_resources(filter = c(channel_id = C$channel_id[i]),
                                              part = "contentDetails")
  
  # Playlist ID für die "Gesamt"-Playlist, die alle Videos enthält
  playlist_id <- channel_resources$items[[1]]$contentDetails$relatedPlaylists$uploads
  
  # Die Video IDs von allen Videos in der Playlist
  # Wenn "max_results" höher als 50 gesetzt wird, werden automatisch alle IDs abgefragt.
  
  Video_ids <- get_playlist_items(filter = c(playlist_id = playlist_id), 
                                  max_results = 999) 
  
  # Da ich Kommentare, Meta Daten und Untertitel abfrage, kombiniere ich hier mit
  # Channel, Channel Name und Channel ID
  Video_ids <- Video_ids %>%
    mutate(channel_name = C$channel_name[i],
           channel = C$channel[i],
           channel_id = C$channel_id[i]) %>%
    relocate(channel_name, channel, channel_id)
  
  # Wir werden die Daten in einer Datei namens [Kanal].csv.gz speichern.
  # also erstellen wir diesen Namen, indem wir den Kanalnamen und „.csv.gz“ einfügen.
  # Ich schließe as.factor(Sys.Date()) ein, um das Datum des Datenabrufs in der Datei zu speichern.
  file_name <- str_c(C$channel[i], "-", as.factor(Sys.Date()), ".csv.gz")
  
  # Korrekter Pfadname innerhalb des Working Directories
  path_file_name <- str_c("Data/Video_IDs/", file_name)
  
  # Jede csv wird nach der konkreten video id benannt
  write_csv(Video_ids, path_file_name)
  
  # Einfügen, falls die YouTube API nach vielen Anfragen hintereinander blockiert.
  Sys.sleep(2) 
  
}