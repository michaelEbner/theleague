#packages
library(ggplot2)
library(tidyverse)


setwd("~/Documents/R/201801_the_league_whatsapp_chat")


#adjust date value
chat <- read.delim("chat.txt", header=FALSE)
chat$date <- str_extract(chat$V1,"[0-9]{1,2}.{1}[0-9]{1,2}.{1}[0-9]{2}")
split_date <- str_split_fixed(chat$date,"/",3)
chat <- cbind(chat,split_date)
names(chat) <- c("V1","date","day","month","year")
chat$day <- as.character(chat$day)
chat$month <- as.character(chat$month)
chat$year <- as.character(chat$year)
chat$day <-   as.character(ifelse(chat$day %in% c("1","2","3","4","5","6","7","8","9"),paste0("0",chat$day),chat$day))
chat$month <- as.character(ifelse(chat$month %in% c("1","2","3","4","5","6","7","8","9"),paste0("0",chat$month),chat$month))
chat$date <- paste0(chat$day,".",chat$month,".20",chat$year)
chat$V1 <- str_replace(chat$V1,"[0-9]{1,2}.{1}[0-9]{1,2}.{1}[0-9]{2}",chat$date)
chat <- chat[,1]
write.table(chat, "chat_adjusted.txt",quote=F,col.names = F, row.names = F)

InputFile <-"chat_adjusted.txt"
Connection <- file(InputFile,open="r")
Messages <- NULL
Iterator <- 0

while (length(Line <- readLines(Connection, n = 1, warn = FALSE)) > 0) {
  
  Timestamp.Raw = strsplit(Line, " - ")[[1]][1]
  
  if (Iterator == 0){
    Timestamp.Format <- "%d/%m/%Y, %H:%M"
    Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
    if (is.na(Timestamp)) {
      Timestamp.Format <- "%Y-%m-%d, %I:%M %p"
      Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
      if (is.na(Timestamp)) {
        Timestamp.Format <- "%d.%m.%Y, %H:%M"
        Timestamp = as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
        assertthat::noNA(Timestamp)
      }
    }
  }
  
  Timestamp <- as.POSIXct(strptime(Timestamp.Raw, Timestamp.Format, tz = "GMT"))
  
  if (is.na(Timestamp)) {
    Messages$Text[Iterator] <- paste(Messages$Text[Iterator], Line)
  } else {
    Person = strsplit(strsplit(Line, ": ")[[1]][1], " - ")[[1]][2]
    
    Text <- strsplit(Line, ": ")[[1]]
    Text <- strsplit(Line, ": ")[[1]][2:length(Text)]
    
    Message <- data.frame(Timestamp = Timestamp, Person = Person, Text = Text)
    
    Messages <- rbind(Messages, Message)
    Iterator <- Iterator + 1
    
  }
}
close(Connection)

Messages$Text <- as.character(Messages$Text)

Messages$MediaOmitted <- FALSE

Messages$Text.Cleaned <- Messages$Text

# Flag messages that contain media
Messages$media <- ifelse(grepl("Media omitted",Messages$Text) == "TRUE",1,0)
Messages$Text <- sub("<Media omitted>","",Messages$Text)


library(lubridate)
Messages$Time <- floor_date(Messages$Timestamp,"time")
Messages$Times <- format(ymd_hms(Messages$Timestamp), "%H:%M:%S")
# Remove Smileys
Messages$Smiley <- ifelse() 
Messages$Text.Cleaned <- sapply(Messages$Text.Cleaned, 
                                function(row) iconv(row, "latin1", "ASCII", sub = ""))

# Tidy Person in case we are looking at a group chat
Filter.Person <- rbind(grep(" changed ", Messages$Person), 
                       grep(" geändert", Messages$Person),
                       grep(" added", Messages$Person),
                       grep(" hinzugefügt", Messages$Person),
                       grep(" removed", Messages$Person),
                       grep(" entfernt", Messages$Person),
                       grep(" left", Messages$Person), 
                       grep(" verlassen", Messages$Person),
                       grep(" created", Messages$Person),
                       grep(" erstellt", Messages$Person))

if (length(Filter.Person) > 0) Messages <- Messages[-Filter.Person, ]



Messages$media <- ifelse(grepl("Media omitted",Messages$Text) == "TRUE",1,0)

Messages$Text <- sub("<Media omitted>","",Messages$Text)

realNames = c(realNames = c("Tobi","Luke","George","Pauli","*+43 676 5248183","*+43 676 9013305,","Tschigg","Max","Christoph","Benji","Klemens","Stefan","Mike","Nape","Phil","Peta","System","Peter","Lukas L.","Mike S.","Marco","Schurli","Jan")
)
names = unique(Messages$Person)


Messages <- merge(Messages,names,by = "Person")
