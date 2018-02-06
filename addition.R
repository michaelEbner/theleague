library(lubridate)
Messages$cet <- Messages$Timestamp-3600*8
Messages$time <- format(ymd_hms(Messages$cet), "%H:%M:%S")
Messages$year <- lubridate::year(Messages$cet)
Messages$cet <- Messages$Timestamp-3600*8
Messages$day <- lubridate::day(Messages$cet)
Messages$month <- lubridate::month(Messages$cet,label = T)
Messages$yday <- lubridate::yday(Messages$cet)


agg <- Messages[,16:17]
agg <- agg %>% group_by(date) %>% summarise(message_count = sum(count))
