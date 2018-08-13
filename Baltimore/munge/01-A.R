Sys.setenv(TZ='America/New_York')

####################
### TEXT LEVEL  ###
####################

messages <- read.csv(file = file.path("data", "messages.csv"))
surveys <- read.csv(file = file.path("data", "surveys.csv"))
users <- read.csv(file = file.path("data", "users.csv"))
clients <- read.csv(file = file.path("data", "clients.csv"))

messages <- data.frame(messages)
messages <- unique(messages)

users <- users[,c("id", "full_name")]
clients <- clients[,c(1:3)]

users$user_first_name <- sapply(strsplit(as.character(users$full_name), ' '), function(x) x[1])
users$user_last_name <- sapply(strsplit(as.character(users$full_name), ' '), function(x) x[length(x)])
users <- subset(users, user_first_name != "OLD")
users <- subset(users, user_first_name != "ClientComm")

names(clients)[2] <- "client_first_name"
names(clients)[3] <- "client_last_name"

messages <- merge(messages, users, by.x = "user_id", by.y = "id", all.x = TRUE, all.y = FALSE)
messages <- merge(messages, clients, by.x = "client_id", by.y = "id", all.x = TRUE, all.y = FALSE)

temp <- messages

temp$created_at <- as.POSIXct(temp$created_at_num, origin = '1970-01-01')
temp$send_at <- as.POSIXct(temp$send_at_num, origin = '1970-01-01')
temp$client_created_at <- as.POSIXct(temp$client_created_at_num, origin = '1970-01-01')

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
temp$send_at_DoW <- wday(temp$send_at, label = TRUE)

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

# Create dataframes
all_messages <- temp
user_messages <- temp[temp$inbound == "False",]
client_messages <- temp[temp$inbound == "True",]

cache('all_messages')
cache('user_messages')
cache('client_messages')

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

names(ToD_message)[40] <- "ToD_Missing"

ToD_message_count <- aggregate(cbind(ToD_message[,c("Night", "Morning", "Afternoon", "Evening", "ToD_Missing")]), by = list(ToD_message$client_id,
                                                                     ToD_message$user_id), FUN = sum)
colnames(ToD_message_count) <- c("client_id","user_id", "Night_messages", "Morning_messages", "Afternoon_messages", 
                                 "Evening_messages", "ToD_Missing_messages")

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

################################################
### TEXT LEVEL: WHICH USER MSGS GET REPLIES? ###
################################################

client_messages$client_id <- as.factor(as.character(client_messages$client_id))
user_messages$client_id <- as.factor(as.character(user_messages$client_id))

first_client_reply <- client_messages %>% group_by(client_id) %>% summarize(min_time = min(send_at_num)) # time of first client msg

temp2 <- user_messages

temp2 <- merge(temp2, first_client_reply, by = c("client_id"))

setdiff(first_client_reply$client_id, user_messages$client_id)
setdiff(first_client_reply$client_id, temp2$client_id)

temp2 <- temp2 %>% filter(send_at_num < min_time) # PO messages leading to a client reply

setdiff(first_client_reply$client_id, temp2$client_id) # there are still about 100 clients that don't come through into temp2
# ground check looks ok -- 1113, 1230, 1339, 1383 all have first msg in relationship inbound

msgs_leading_to_reply <- temp2

first_msg_replied <- msgs_leading_to_reply %>% group_by(client_id) %>% slice(which.max(send_at_num)) # the first user msg in a relationship that received a client reply (might be initial msg, might not)

# number_msgs_to_reply <- aggregate(msgs_leading_to_reply$client_id, by = list(msgs_leading_to_reply$client_id), FUN = length) # how many user msgs before the first client msg
# colnames(number_msgs_to_reply) <- c("client_id", "number_to_reply")

# number_msgs_to_reply$reply_to_initial <- as.numeric(number_msgs_to_reply$number_to_reply == 1) # client replied to first user msg (T/F)

# ultimately_replied <- merge(ultimately_replied, number_msgs_to_reply, by = c("client_id")) # all user msgs that eventually received a reply

# initial_msg_replied <- filter(first_msg_replied, reply_to_initial == 1) # initial user msgs that received a client reply

## NEW APPROACH

# indicator: is this user msg an initial msg? 

initial_msg <- user_messages %>% group_by(client_id) %>% summarize(initial_msg = min(send_at_num))
temp4 <- merge(user_messages, initial_msg, by = c("client_id"))
temp4 <- temp4 %>% mutate(initial_msg_indicator = case_when(initial_msg == send_at_num ~ 1,
                                                            initial_msg != send_at_num ~ 0))

# indicator: did the client respond to this user msg?

msg_replied <- first_msg_replied[c("client_id","send_at_num")]
msg_replied$msg_replied_i <- 1

temp4 <- merge(temp4, msg_replied, by = c("client_id", "send_at_num"), all.x = TRUE, all.y = TRUE)
temp4$msg_replied_i[is.na(temp4$msg_replied_i)] <- 0

temp4 <- temp4[c("client_id", "user_id", "id", "body", "inbound", 
                 "user_first_name", "user_last_name", "client_first_name", "client_last_name",
                 "created_at", "created_at_backup", "created_at_num", 
                 "send_at", "send_at_backup", "send_at_num",
                 "send_at_DoW", "send_at_ToD_bins", "initial_msg_indicator", "msg_replied_i", 
                 "future_appointment_date", "scheduled_diff", "user_msg_length")]

# add variables of interest

temp4$greeting <- as.numeric(grepl("hello", temp4$body, ignore.case = TRUE) | 
                               grepl("good morning", temp4$body, ignore.case = TRUE) |
                               grepl("good afternoon", temp4$body, ignore.case = TRUE) |
                               grepl("good evening", temp4$body, ignore.case = TRUE)) 

temp4$closing <- as.numeric(grepl("have a great", temp4$body, ignore.case = TRUE) | 
                              grepl("have a good", temp4$body, ignore.case = TRUE) | 
                              grepl("have a bless", temp4$body, ignore.case = TRUE) | 
                              grepl("enjoy", temp4$body, ignore.case = TRUE) | 
                              grepl("stay safe", temp4$body, ignore.case = TRUE))

temp4$polite <- as.numeric(grepl("please", temp4$body, ignore.case = TRUE) |
                             grepl("thank you", temp4$body, ignore.case = TRUE) |
                             grepl("courtesy", temp4$body, ignore.case = TRUE) |
                             grepl("friendly", temp4$body, ignore.case = TRUE))

temp4$business <- as.numeric((grepl("verif", temp4$body, ignore.case = TRUE) & !grepl("re-verif", temp4$body, ignore.case = TRUE)) |
                               grepl("stub", temp4$body, ignore.case = TRUE) |
                               grepl("letter", temp4$body, ignore.case = TRUE) |
                               grepl("document", temp4$body, ignore.case = TRUE))

temp4$problem <- as.numeric(grepl("warrant", temp4$body, ignore.case = TRUE) |
                              grepl("fail", temp4$body, ignore.case = TRUE) |
                              grepl("missed", temp4$body, ignore.case = TRUE) |
                              grepl("violation", temp4$body, ignore.case = TRUE) |
                              grepl("compliance", temp4$body, ignore.case = TRUE))

temp4$urgency <- as.numeric(grepl("asap", temp4$body, ignore.case = TRUE) |
                              grepl("a.s.a.p.", temp4$body, ignore.case = TRUE) |
                              grepl("immediately", temp4$body, ignore.case = TRUE) |
                              grepl("right away", temp4$body, ignore.case = TRUE) |
                              grepl("imperative", temp4$body, ignore.case = TRUE))

temp4$info <- as.numeric(grepl("will be closed", temp4$body, ignore.case = TRUE) |
                           grepl("office is closed", temp4$body, ignore.case = TRUE) |
                           grepl("closure", temp4$body, ignore.case = TRUE) |
                           grepl("shutdown", temp4$body, ignore.case = TRUE) |
                           grepl("phone lines", temp4$body, ignore.case = TRUE))

temp4$yelling <- as.numeric(grepl("^[^a-z]*$", temp4$body))

temp4$pls_respond <- as.numeric(grepl("respond to this text", temp4$body, ignore.case = TRUE) |
                                  grepl("respond to this message", temp4$body, ignore.case = TRUE) |
                                  grepl("acknowledge", temp4$body, ignore.case = TRUE) |
                                  grepl("reply to this text", temp4$body, ignore.case = TRUE) |
                                  grepl("reply to this message", temp4$body, ignore.case = TRUE))

# court date reminder/appointment date reminder
msgs_with_dates <- temp4[temp4$future_appointment_date == 1,c("client_id", "user_id", "send_at_num", "body")]

msgs_with_dates$court_date_reminder <- as.numeric((grepl("court date", msgs_with_dates$body, ignore.case = TRUE) &
                                                     !grepl("date specified is your scheduled court date please report", msgs_with_dates$body, ignore.case = TRUE)) |
                                                    grepl("your trial", msgs_with_dates$body, ignore.case = TRUE) | 
                                                    grepl("jury trial", msgs_with_dates$body, ignore.case = TRUE) |
                                                    grepl("court today", msgs_with_dates$body, ignore.case = TRUE) |
                                                    (grepl("trial date", msgs_with_dates$body, ignore.case = TRUE) &
                                                       !grepl("after the trial on the trial date", msgs_with_dates$body, ignore.case = TRUE)) |
                                                    (grepl("court tomorrow", msgs_with_dates$body, ignore.case = TRUE) &
                                                       !grepl("if you have court tomorrow", msgs_with_dates$body, ignore.case = TRUE)) |
                                                    grepl("scheduled for trial", msgs_with_dates$body, ignore.case = TRUE) |
                                                    grepl("reminder of court", msgs_with_dates$body, ignore.case = TRUE) |
                                                    grepl("court on", msgs_with_dates$body, ignore.case = TRUE) |
                                                    grepl("court this morning", msgs_with_dates$body, ignore.case = TRUE)
                                                    )

msgs_with_dates$appointment_date_reminder <- as.numeric(grepl("check in", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report day", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report date", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report to", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report by", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report in", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report on", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("next appointment is", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report tomorrow", msgs_with_dates$body, ignore.case = TRUE) |
                                                          grepl("report every", msgs_with_dates$body, ignore.case = TRUE))

msgs_with_dates <- msgs_with_dates[,c("client_id", "send_at_num", "court_date_reminder", "appointment_date_reminder")]


temp5 <- merge(temp4, msgs_with_dates, by = c("client_id", "send_at_num"), all.x = TRUE)

# client name
temp5$has_client_last_name <- stri_detect(temp5$body, fixed = temp5$client_last_name)
temp5$has_client_first_name <- stri_detect(temp5$body, fixed = temp5$client_first_name)

temp5$has_client_name <- as.numeric(temp5$has_client_last_name | temp5$has_client_first_name)

# user name
temp5$has_user_last_name <- stri_detect(temp5$body, fixed = temp5$user_last_name)
temp5$has_user_first_name <- stri_detect(temp5$body, fixed = temp5$user_first_name)

temp5$has_user_name <- as.numeric(temp5$has_user_last_name | temp5$has_user_first_name)

# replace NAs in msgs_with_dates with 0
temp5$appointment_date_reminder[is.na(temp5$appointment_date_reminder)] <- 0
temp5$court_date_reminder[is.na(temp5$court_date_reminder)] <- 0

## message similarity

temp6 <- user_messages %>% full_join(user_messages, by = "user_id") 
temp7 <- temp6 %>% filter(client_id.x != client_id.y)

temp7$reuse_score <- stringsim(as.character(temp7$body.x), as.character(temp7$body.y)) # this takes a very long time to run

max_reuse_score <- temp7 %>% group_by(client_id.x, send_at_num.x) %>% summarize(max_reuse_score = max(reuse_score))

cache('max_reuse_score')

temp8 <- merge(temp5, max_reuse_score, by.x = c("client_id", "send_at_num"), by.y = c("client_id.x", "send_at_num.x"),
               all.x = FALSE, all.y = FALSE)

# Final tidy up
temp8$PO <- as.factor(temp8$user_id)
temp8$PO <- relevel(temp8$PO, ref="31")
temp8$user_id <- as.factor(temp8$user_id)
temp8$client_id <- as.factor(temp8$client_id)

user_msgs_qualities <- temp8

cache('user_msgs_qualities')







