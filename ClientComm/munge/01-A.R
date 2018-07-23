Sys.setenv(TZ='GMT')

####################
### TEXT LEVEL  ###
####################

messages <- read.csv(file = file.path("data", "messages.csv"))
surveys <- read.csv(file = file.path("data", "surveys.csv"))

messages <- data.frame(messages)
messages <- unique(messages)

messages$send_at <- as.POSIXct(as.character(messages$send_at), tz = "America/New_York")
messages$created_at <- as.POSIXct(as.character(messages$created_at), tz = "America/New_York")
messages$client_created_at <- as.POSIXct(as.character(messages$client_created_at), tz = "America/New_York")

temp <- messages

# Merge messages & message_level_sentiments

# message.level.sentiments <- message.level.sentiments[,c(4,17,18,19,20)]

# message.level.sentiments[duplicated(message.level.sentiments$id),]
# message.level.sentiments <- unique(message.level.sentiments)

# temp <- merge(message.level.sentiments, messages, by = "id")

# Message scheduling -- time diff in hours
temp$scheduled_diff <- difftime(temp$send_at, temp$created_at, units = "hours")

temp$scheduled_diff[temp$scheduled_diff < 0] <- 0
temp$scheduled_diff[temp$scheduled_diff > 336] <- 336

# Day of week
temp$send_at_DoW <- wday(as.Date(temp$send_at), label=TRUE)

# Work around terrible time zone problem
temp$created_at <- as.POSIXct(temp$created_at_backup, tz = "America/New_York")
temp$send_at <- as.POSIXct(temp$send_at_backup, tz = "America/New_York")
temp$client_created_at <- as.POSIXct(temp$client_created_at_backup, tz = "America/New_York")


# Time of day (bins)
temp$send_at_time <- hour(temp$send_at) + 
  minute(temp$send_at)/60 + 
  second(temp$send_at)/3600

bins <- cut(temp$send_at_time, 
            breaks = c(0,7,12,17,21,24),
            labels = c("Swing", "Morning", "Afternoon", "Evening", "Night"))

temp$send_at_ToD_bins <- bins

temp$send_at_ToD_bins <- temp$send_at_ToD_bins %>% fct_collapse(Night = c("Swing", "Night"))

# Message contains future appointment date
temp$has_future_date <- as.numeric(temp$dates_ext != "[]") # Indicator of date in text
temp$office_closure <- as.numeric(grepl("closed", temp$body) | grepl("closure", temp$body)) # Office closures
temp$future_appointment_date <- as.numeric(temp$has_future_date == 1 & temp$office_closure == 0) # Dates without office closures

# Delete "inbound" messages that are not from clients 
temp$system_messages <- as.numeric(grepl("Updated the phone number to", temp$body) | grepl("was transferred to you", temp$body))
temp <- temp[temp$system_messages == 0,]

# Delete unneeded variables
# temp <- temp[,c(9,10,1,17,18,26,19,14,32,27,28,29,6,7,11)]

# Create dataframes
all_messages <- temp
user_messages <- temp[temp$inbound == "False",]

cache('all_messages')
cache('user_messages')

##########################
### RELATIONSHIP LEVEL ###
##########################

# Dedupe
find_multiple_outcomes <- aggregate(surveys$client_id, by=list(surveys$client_id, surveys$user_id), FUN = length)

single_outcomes <- subset(find_multiple_outcomes, x == 1)
colnames(single_outcomes)[1:2] <- c("client_id", "user_id")

surveys_deduped <- merge(surveys, single_outcomes, by = c("client_id", "user_id"), all.x = FALSE, all.y = TRUE)

# Create outcome variable
outcomes <- surveys_deduped[surveys_deduped$text != "Case still open / not applicable",] # drop these rows

outcomes <- outcomes %>% mutate(supervision_failure = 
                                  case_when(text == "FTA" ~ TRUE,
                                            text == "Supervision rescinded" ~ TRUE,
                                            text == "Successful closeout" ~ FALSE))

## Create aggregate tables for variables of interest

# Maximum scheduled time difference
max_scheduled_diff <- aggregate(user_messages$scheduled_diff,
                                by=list(user_messages$client_id, 
                                        user_messages$user_id), 
                                max)
colnames(max_scheduled_diff) <- c("client_id","user_id","max_scheduled_diff")

# Number of messages by day of week
user_messages$message_i <- 1

DoW_message <- spread(user_messages, send_at_DoW, message_i, fill = 0)

DoW_message_count <- aggregate(cbind(DoW_message[,c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")]), 
                               by=list(DoW_message$client_id, 
                               DoW_message$user_id), FUN = sum)
colnames(DoW_message_count) <- c("client_id","user_id", "Sun_messages", "Mon_messages", "Tue_messages",
                                 "Wed_messages", "Thu_messages", "Fri_messages", "Sat_messages")

DoW_message_count$Wkd_messages <- DoW_message_count$Sun_messages + DoW_message_count$Sat_messages

# Number of messages by time of day
ToD_message <- spread(user_messages, send_at_ToD_bins, message_i, fill = 0)

names(ToD_message)[32] <- "Missing"

ToD_message_count <- aggregate(cbind(ToD_message[,c("Night", "Morning", "Afternoon", "Evening", "Missing")]), by = list(ToD_message$client_id,
                                                                     ToD_message$user_id), FUN = sum)
colnames(ToD_message_count) <- c("client_id","user_id", "Night_messages", "Morning_messages", "Afternoon_messages", 
                                 "Evening_messages", "Missing_messages")

# Number of future appointment dates
number_appt_reminders <- aggregate(user_messages$future_appointment_date, 
                                   by=list(user_messages$client_id,
                                           user_messages$user_id), FUN = sum) 
colnames(number_appt_reminders) <- c("client_id", "user_id", "future_appointment_date")

# Number of client messages
client_msg_count <- aggregate(as.numeric(all_messages$inbound == "True"), 
                              by = list(all_messages$client_id, all_messages$user_id),
                              FUN = sum)
colnames(client_msg_count) <- c("client_id", "user_id", "client_msg_count")

# Number of user messages
user_msg_count <- aggregate(as.numeric(all_messages$inbound == "False"), 
                            by = list(all_messages$client_id, all_messages$user_id),
                            FUN = sum)
colnames(user_msg_count) <- c("client_id", "user_id", "user_msg_count")

# Median user message length
user_messages$user_msg_length <- nchar(as.character(user_messages$body))

med_user_msg_length <- aggregate(user_messages$user_msg_length,
                                 by = list(user_messages$client_id,
                                           user_messages$user_id),
                                 FUN = median)
colnames(med_user_msg_length) <- c("client_id", "user_id", "med_user_msg_length")

# # Maximum polarity
# max_polarity <- aggregate(user_messages$polarity,
#                           by = list(user_messages$client_id,
#                                     user_messages$user_id),
#                           FUN = max)
# colnames(max_polarity) <- c("client_id", "user_id", "max_polarity")
# 
# # Minimum polarity
# min_polarity <- aggregate(user_messages$polarity,
#                           by = list(user_messages$client_id,
#                                     user_messages$user_id),
#                           FUN = min)
# colnames(min_polarity) <- c("client_id", "user_id", "min_polarity")
# 
# # Maximum subjectivity
# max_subjectivity <- aggregate(user_messages$subjectivity,
#                               by = list(user_messages$client_id,
#                                         user_messages$user_id),
#                               FUN = max)
# colnames(max_subjectivity) <- c("client_id", "user_id", "max_subjectivity")
# 
# # Minimum subjectivity
# min_subjectivity <- aggregate(user_messages$subjectivity,
#                               by = list(user_messages$client_id,
#                                         user_messages$user_id),
#                               FUN = min)
# colnames(min_subjectivity) <- c("client_id", "user_id", "min_subjectivity")

# Merge all variables into one dataframe

temp1 <- merge(outcomes, max_scheduled_diff, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, DoW_message_count, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, ToD_message_count, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, number_appt_reminders, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, client_msg_count, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, user_msg_count, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

temp1 <- merge(temp1, med_user_msg_length, by=c("user_id","client_id"), 
              all.x = FALSE, all.y = FALSE)

# temp1 <- merge(temp1, max_polarity, by=c("user_id","client_id"), 
#               all.x = FALSE, all.y = FALSE)
# 
# temp1 <- merge(temp1, min_polarity, by=c("user_id","client_id"), 
#               all.x = FALSE, all.y = FALSE)
# 
# temp1 <- merge(temp1, max_subjectivity, by=c("user_id","client_id"), 
#               all.x = FALSE, all.y = FALSE)
# 
# temp1 <- merge(temp1, min_subjectivity, by=c("user_id","client_id"), 
#               all.x = FALSE, all.y = FALSE)

# Delete unneeded variables
# temp1 <- temp1[,c(2,1,7:28)]

# Final tidy up
temp1$PO <- as.factor(temp1$user_id)
temp1$PO <- relevel(temp1$PO, ref="31")
temp1$user_id <- as.factor(temp1$user_id)
temp1$client_id <- as.factor(temp1$client_id)


# Create dataframe
cc.dtf <- temp1

cache('cc.dtf')
