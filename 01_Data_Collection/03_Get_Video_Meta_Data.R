###############################################################################
# Dieses Skript fragt die Video Statistiken und Meta Daten der Videos aus der
# Liste von Videos aus dem Skript 2_Get_Video_Ids.R über die YouTube API ab.
###############################################################################

library(tidyverse)
library(tuber)


# Authentifizierung des YouTube API Accounts
# Entfernen der cached Authentifizierung, falls notwendig:
# system("rm .httr-oauth")
# system("del .httr-oauth")
yt_oauth(client_id, client_secret, token = "")


files <- list.files("Data/Video_IDs", full.names = TRUE)

F <- list()

for(i in 1:length(files)) {
  
  F[[i]] <- read_csv(files[i])
  
}

# D ist ein Dataframe mit Daten eines spezifischen Videos in jeder Reihe
D <- bind_rows(F)


# Untersuchungszeitraum
D$contentDetails.videoPublishedAt <- lubridate::as_date(D$contentDetails.videoPublishedAt)
D <- D %>% filter(contentDetails.videoPublishedAt >= '2024-8-06' & contentDetails.videoPublishedAt <= '2025-2-23')

saveRDS(D, "Video_ID_Zuschnitt.rds")


# Hiermit wird kontrolliert, dass eine Video ID nur einmal abgefragt wurde
S <- c(unique(D$channel))

files <- c()

for (i in 1:length(S)) {
  channel <- S[i]
  files2 <- list.files(str_c("Data/Video_Meta/",channel))
  files <- c(files, files2)
}


# Video IDs sind 11 Zeichen lang
# Dieser Code fischt die Video ID aus den Dateinamen.
files <- str_sub(files, start = 1, end = 11)

for(i in 1:length(files)){
  D <- D %>% filter(contentDetails.videoId != files[i])
}



# Die For-Loop fragt die Video Metadaten wie Aufrufe, Gefällt mir Angaben, Titel, Beschreibung
for (i in 1:nrow(D)) {
  
  
  video_id <- D$contentDetails.videoId[i]
  channel_id <- D$channel_id[i]
  channel_name <- D$channel_name[i]
  channel <- D$channel[i]
  
  # Video Beschreibung and Titel
  Video_Meta <- tryCatch(get_video_details(video_id = video_id, part = "snippet"),
                         error = function(error_message) {
                           message("Error in collecting Video Meta Data: ", video_id) 
                           message(error_message)
                           return(NULL)
                         } 
  )
  
  Video_Meta <- bind_rows(Video_Meta$items[[1]]$snippet[c("publishedAt", "title", "description")])
  
  
  # Die Statistiken der Videos (Aufrufe, Gefällt mir Angaben, Anzahl der Kommentare, etc.)
  Video_Statistics <- tryCatch(get_video_details(video_id = video_id, part = "statistics"),
                               error = function(error_message) {
                                 message("Error in collecting Video Meta Data", video_id)
                                 message(error_message)
                                 return(NULL)
                               }
  )
  
  Video_Statistics <- bind_rows(Video_Statistics$items[[1]]$statistics)
  
  
  
  # Video Länge
  Video_Length <- tryCatch(get_video_details(video_id = video_id, part = "contentDetails"),
                           error = function(error_message) {
                             message("Error in collecting Video Meta Data: ", video_id) 
                             message(error_message)
                             return(NULL)
                           } 
  )
  
  Video_Length <- Video_Length$items[[1]]$contentDetails$duration
  
  Video_Meta <- cbind(Video_Meta, Video_Statistics, Video_Length)
  
  if(!is.null(Video_Meta)) {
    
    Video_Meta <- Video_Meta %>%
      mutate(channel_name = channel_name,
             channel = channel,
             channel_id = channel_id,
             video_id = video_id) %>%
      relocate(channel_name, channel, channel_id, video_id)
    
    if(!channel %in% list.files("Data/Video_Meta/")) {
      system(str_c("mkdir Data/Video_Meta/", channel))
    }
    
    
    # Auch hier füge ich zur Nachvollziehbarkeit das Datum hinzu 
    file_path_name <- str_c("Data/Video_Meta/", channel, "/", video_id, "-", as.factor(Sys.Date()), ".csv.gz")
    
    write_csv(Video_Meta, file_path_name)
    
    Sys.sleep(2) 
    
    
  }
  
}