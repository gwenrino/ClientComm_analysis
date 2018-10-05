Sys.setenv(TZ='America/New_York')
library(tidyverse)
library(stringi)
library(ProjectTemplate)
library(lubridate)

slc.data <- read.csv(file = file.path("data", "slc.data.csv"))
surveys <- read.csv(file = file.path("data", "surveys.csv"))
messages <- read.csv(file = file.path("data", "messages.csv"))
clients <- read.csv(file = file.path("data", "clients.csv"))
users <- read.csv(file = file.path("data", "users.csv"))
salt.lake.discharges <- read.csv(file = file.path("data", "salt.lake.discharges.csv"))

#################################################
### Linking slc.data and salt.lake.discharges ###
#################################################

glimpse(slc.data)
glimpse(surveys)
glimpse(messages)
glimpse(clients)
glimpse(salt.lake.discharges)

length(unique(salt.lake.discharges$ofndr_num))
# doubled offender numbers sometimes associated with second outcome, sometimes not.
# keep dupes for now

summary(clients$ofndr_num) # about 1700 clients have ofndr_num

# Get ofndr_num for slc.data
temp.1 <- left_join(slc.data, clients, by = "client_id")
temp.1$user_id.y <- NULL

# Join on ofndr_num
length(unique(salt.lake.discharges$ofndr_num)) # there are 6323 unique ofndr_nums
ofndr_num_join <- inner_join(salt.lake.discharges, temp.1, by = "ofndr_num") # 866 match numbers in CC data

summary(clients$ofndr_num)
setdiff(temp.1$ofndr_num, salt.lake.discharges$ofndr_num)

ofndr_num_join$ofndr_num_match_i <- 1
ofndr_num_match <- ofndr_num_join[,c("client_id", "ofndr_num_match_i")]
temp.1 <- left_join(temp.1, ofndr_num_match, by = "client_id")  
temp.1$ofndr_num_match_i[is.na(temp.1$ofndr_num_match_i)] <- 0
table(temp.1$ofndr_num_match_i) # shouldn't there be 866 matches? why are there 1028?

temp.1 <- unique(temp.1)
client_count <- temp.1 %>% group_by(client_id) %>% add_count(client_id) %>% filter(n != 1)

# Join on first and last name
first_last_join <- inner_join(salt.lake.discharges, temp.1, by = c("last_name", "first_name")) # 363 matches

nrow(first_last_join[first_last_join$ofndr_num_match_i == 1,]) # 93 ofndr_num matches in first_last_join
nrow(first_last_join[first_last_join$ofndr_num_match_i == 0,]) # 270 first_last matches that ofndr_num match didn't catch

# Join on last name and search for first
names_join <- inner_join(salt.lake.discharges, temp.1, by = "last_name")
names_join$first_name_match <- stri_detect(names_join$first_name.x, fixed = names_join$client_first_name)
names_join_match <- names_join %>% filter(first_name_match == TRUE) # 2591 matches 

nrow(names_join_match[names_join_match$ofndr_num_match_i == 1,]) # 918 ofndr_num matches in names_join_match
nrow(names_join_match[names_join_match$ofndr_num_match_i == 0,]) # 1673 names matches that ofndr_num match didn't catch

names_potential_false_positives <- subset(names_join_match, !is.na(ofndr_num.x) & !is.na(ofndr_num.y))

false_positives <- names_potential_false_positives[names_potential_false_positives$ofndr_num.x != names_potential_false_positives$ofndr_num.y,]

names_join_match <- subset(names_join_match, !(ofndr_num.x %in% false_positives$ofndr_num.x))
names_join_match <- subset(names_join_match, !(ofndr_num.x %in% false_positives$ofndr_num.y))

# example of confusion about agcy_desc vs. department_id
View(unique(names_join_match[(names_join_match$user_id.x == 7 & names_join_match$ofndr_num_match_i == 1), c(1,7,8,12,16,19)]))

names(names_join_match)
slc.matched <- names_join_match[,c("client_id","user_id.x","agcy_desc","department_id","end_dt",
                                   "discharge_desc","discharge_cat","client_created_at","client_created_at_backup",
                                   "client_created_at_num","active","ofndr_num_match_i")]

# deduping 
slc.matched <- unique(slc.matched)


#####
# Data sets for comparison of failure rates for CC and non-CC clients

unmatched_outcomes <- subset(salt.lake.discharges, !(ofndr_num %in% names_join_match$ofndr_num.x))
unmatched_outcomes <- subset(unmatched_outcomes, !(ofndr_num %in% names_join_match$ofndr_num.y))
unmatched_outcomes <- unique(unmatched_outcomes)
unmatched_outcomes <- unmatched_outcomes[,c("ofndr_num","agcy_desc","end_dt","discharge_cat")]
unmatched_outcomes <- unmatched_outcomes %>% filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")
levels(unmatched_outcomes$discharge_cat)
unmatched_outcomes$discharge_cat <- factor(unmatched_outcomes$discharge_cat)
levels(unmatched_outcomes$discharge_cat) <- c(FALSE, TRUE)
names(unmatched_outcomes)[4] <- "supervision_failure"

matched_outcomes <- subset(salt.lake.discharges, ofndr_num %in% names_join_match$ofndr_num.x)
matched_outcomes <- subset(matched_outcomes, ofndr_num %in% names_join_match$ofndr_num.y)
matched_outcomes <- unique(matched_outcomes)
matched_outcomes <- matched_outcomes[,c("ofndr_num","agcy_desc","end_dt","discharge_cat")]
matched_outcomes <- matched_outcomes %>% filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")
levels(matched_outcomes$discharge_cat)
matched_outcomes$discharge_cat <- factor(matched_outcomes$discharge_cat)
levels(matched_outcomes$discharge_cat) <- c(FALSE, TRUE)
names(matched_outcomes)[4] <- "supervision_failure"


CC_pretrial_outcomes <- matched_outcomes %>% filter(agcy_desc == "PRETRIAL SERVICES")

non_CC_pretrial_outcomes <- unmatched_outcomes %>% filter(agcy_desc == "PRETRIAL SERVICES") 

CC_probation_outcomes <- matched_outcomes %>% filter(agcy_desc == "PROBATION SERVICES") 

non_CC_probation_outcomes <- unmatched_outcomes %>% filter(agcy_desc == "PROBATION SERVICES") 


cache('CC_pretrial_outcomes')
cache('non_CC_pretrial_outcomes')
cache('CC_probation_outcomes')
cache('non_CC_probation_outcomes')


##### Working out the department_id/agcy confusion in the matched data

slc.matched[slc.matched$client_id==277,]
filter(slc.matched, slc.matched$user_id.x == 7)
filter(ofndr_num_join, ofndr_num_join$user_id.x == 13)
head(slc.matched,20)

# are any POs assigned to more than 1 department? NO
slc.matched[,c(2,4)] %>% group_by(user_id.x) %>% summarize(n_distinct(department_id))
aggregate(department_id ~ user_id.x, slc.matched, function(x) length(unique(x)))

unique(slc.matched$user_id.x[slc.matched$department_id == 1])

client_end_date_nums <- unique(mult_client_msgs[,c(1,37)])

practice_dt <- client_end_date_nums[client_end_date_nums$client_id %in% c(585,277),]
practice_dt <- practice_dt %>% arrange(client_id, end_dt_num) %>% mutate(id = row_number())

n <- nrow(practice_dt)

for (i in 1:n) {
  client_id <- practice_dt$client_id[i]
  if (practice_dt$id[i] == 1) {print ("first row of data")}
  else if (practice_dt$id[i] == n) {print ("last row of data")}
  else if (practice_dt$client_id[i] != practice_dt$client_id[i-1]) {print ("first row of group")}
  else if (practice_dt$client_id[i] != practice_dt$client_id[i+1]) {print ("last row of group")}
  else {print ("next row has same client_id")}
}


n <- nrow(practice_dt)
outcome_range <- cbind(data.frame(matrix(nrow=0,ncol=3)))

for (i in 1:n) {
  client_id <- practice_dt$client_id[i]
  if (practice_dt$id[i] == 1) {
    # first row of data
    low_edge <- 0
    high_edge <- practice_dt$end_dt_num[i]
    row_range <- cbind(client_id, low_edge, high_edge)
    outcome_range <- rbind(outcome_range, row_range)
  }
  else if (practice_dt$id[i] == n) {
    # last row of data
    low_edge <- practice_dt$end_dt_num[i-1]
    high_edge <- practice_dt$end_dt_num[i]
    row_range <- cbind(client_id, low_edge, high_edge)
    outcome_range <- rbind(outcome_range, row_range)
    }
  else if (practice_dt$client_id[i] != practice_dt$client_id[i-1]) {
    # first row of group
    low_edge <- 0
    high_edge <- practice_dt$end_dt_num[i]
    row_range <- cbind(client_id, low_edge, high_edge)
    outcome_range <- rbind(outcome_range, row_range)
    }
  else if (practice_dt$client_id[i] != practice_dt$client_id[i+1]) {
    # last row of group
    low_edge <- practice_dt$end_dt_num[i-1]
    high_edge <- practice_dt$end_dt_num[i]
    row_range <- cbind(client_id, low_edge, high_edge)
    outcome_range <- rbind(outcome_range, row_range)
    }
  else {
    # next row has same client_id
    low_edge <- practice_dt$end_dt_num[i-1]
    high_edge <- practice_dt$end_dt_num[i]
    row_range <- cbind(client_id, low_edge, high_edge)
    outcome_range <- rbind(outcome_range, row_range)
    }
}

### Strategy: attach matched outcomes to messages, separate by ranges, then filter to dates within 2 months of outcome date

names(slc.matched)[2] <- "user_id"

matched.msgs <- merge(slc.matched, messages, by = c("client_id", "user_id"), all.x = FALSE, all.y = FALSE)

matched.msgs$add_hrs_min_sec <- paste(as.character(matched.msgs$end_dt), "12:00:00 EDT", sep=" ")

matched.msgs$end_dt_converted <- as.POSIXct(matched.msgs$add_hrs_min_sec, format = "%m/%d/%y %H:%M:%S", tz="EDT")

matched.msgs$end_dt_num <- as.numeric(matched.msgs$end_dt_converted)

client_id.end_dt_num.matched <- unique(matched.msgs[,c(1,36)])

end_dates_ids <- client_id.end_dt_num.matched %>% arrange(client_id, end_dt_num) %>% mutate(id = row_number())

n <- nrow(end_dates_ids)
outcome_range <- cbind(data.frame(matrix(nrow=0,ncol=3)))

for (i in 1:n) {
  client_id <- end_dates_ids$client_id[i]
  end_dt_num <- end_dates_ids$end_dt_num[i]
  if (end_dates_ids$id[i] == 1) {
    low_edge <- 0
    row_range <- cbind(client_id, low_edge, end_dt_num)
    outcome_range <- rbind(outcome_range, row_range)
  }
  else if (end_dates_ids$id[i] == n) {
    low_edge <- end_dates_ids$end_dt_num[i-1]
    row_range <- cbind(client_id, low_edge, end_dt_num)
    outcome_range <- rbind(outcome_range, row_range)
  }
  else if (end_dates_ids$client_id[i] != end_dates_ids$client_id[i-1]) {
    low_edge <- 0
    row_range <- cbind(client_id, low_edge, end_dt_num)
    outcome_range <- rbind(outcome_range, row_range)
  }
  else if (end_dates_ids$client_id[i] != end_dates_ids$client_id[i+1]) {
    low_edge <- end_dates_ids$end_dt_num[i-1]
    row_range <- cbind(client_id, low_edge, end_dt_num)
    outcome_range <- rbind(outcome_range, row_range)
  }
  else {
    low_edge <- end_dates_ids$end_dt_num[i-1]
    row_range <- cbind(client_id, low_edge, end_dt_num)
    outcome_range <- rbind(outcome_range, row_range)
  }
}

temp.3 <- merge(matched.msgs, outcome_range, by = c("client_id", "end_dt_num"), all = TRUE)

matched.msgs.filtered <- temp.3 %>% filter(send_at_num > low_edge) %>% filter(send_at_num < end_dt_num)
# msgs now uniquely associated with outcomes

temp.4 <- matched.msgs.filtered %>% group_by(client_id, end_dt_num) %>% summarize(last_msg_num = max(send_at_num)) %>%
  filter((end_dt_num - last_msg_num) < 5184000) # max msg is less than 2 months before end_dt

temp.4$legit_relationship_i <- 1

matched.msgs.filtered <- merge(matched.msgs.filtered, temp.4, by = c("client_id", "end_dt_num"), all = TRUE)

matched.msgs.filtered <- matched.msgs.filtered %>% filter(legit_relationship_i == 1)

View(matched.msgs.filtered[matched.msgs.filtered$client_id == 277,c(1,4,3,5,32,38,2)]) # still wrong!
View(matched.msgs[matched.msgs$client_id == 277,c(1,3,2,4,31,36)])
# what's weird about this one is that most of the messages are AFTER the first end_dt but still way BEFORE the second end_dt
# maybe reassigned to a non-CC agent

table(matched.msgs.filtered$agcy_desc, matched.msgs.filtered$department_id) # a higher % align than before...
table(slc.matched$agcy_desc, slc.matched$department_id)

# Which are the mismatches?
matched.msgs.filtered[matched.msgs.filtered$agcy_desc == "PROBATION SERVICES" & matched.msgs.filtered$department_id == 2,c("client_id", "user_id")]
matched.msgs.filtered[matched.msgs.filtered$agcy_desc == "PRETRIAL SERVICES" & matched.msgs.filtered$department_id == 1,c("client_id", "user_id")]

# mismatches in dept 2
messages$body[messages$client_id == 4388 & messages$user_id == 52] # text says probation (agrees with agcy_desc)
messages$body[messages$client_id == 3793 & messages$user_id == 52] # appears to be probation (agrees with agcy_desc)
messages$body[messages$client_id == 1181 & messages$user_id == 46] # text says pretrial (agrees with dept_id)


# mismatches in dept 1
messages$body[messages$client_id == 295 & messages$user_id == 95] # text says probation (agrees with dept_id)
messages$body[messages$client_id == 4300 & messages$user_id == 48] # text says probation (agrees with dept_id)
messages$body[messages$client_id == 4329 & messages$user_id == 18] # text says probation but alerts look like pretrial (agrees with dept_id)
messages$body[messages$client_id == 3626 & messages$user_id == 19] # text says probation (agrees with dept_id)
messages$body[messages$client_id == 5470 & messages$user_id == 117] # appears to be probation (agrees with dept_id)
messages$body[messages$client_id == 1756 & messages$user_id == 100] # not clear, looks like pretrial? (agrees with agcy_desc)
messages$body[messages$client_id == 1705 & messages$user_id == 116] # looks like probation, but "you are getting released on pre-trial"? (agrees with agcy_desc)

unique_outcomes_and_relationships <- unique(matched.msgs.filtered[,c(1:5)])
table(unique_outcomes_and_relationships$agcy_desc, unique_outcomes_and_relationships$department_id)

# more than one relationship per outcome 
unique_outcomes_and_relationships %>% group_by(client_id, end_dt_num) %>% count() %>% filter(n != 1)
# drop them!
matched.msgs.filtered <- matched.msgs.filtered[!(matched.msgs.filtered$client_id %in% c(317,536,1108,1622,2180,2340,2368)),]

# Drop mismatches and court services
matched.msgs.filtered <- matched.msgs.filtered %>% 
  filter((agcy_desc == "PROBATION SERVICES" & department_id == 1) | (agcy_desc == "PRETRIAL SERVICES" & department_id == 2))

# Drop unnecessary variables
slc.messages <- matched.msgs.filtered[,c("client_id","user_id","department_id","agcy_desc","end_dt","end_dt_num",
                                                  "discharge_desc","discharge_cat","client_created_at.x","client_created_at_backup.x",
                                                  "client_created_at_num.x","active","body","inbound","send_at","send_at_backup",
                                                  "send_at_num","dates_ext","created_at","created_at_backup","created_at_num")]
names(slc.messages)[9] <- "client_created_at"
names(slc.messages)[10] <- "client_created_at_backup"
names(slc.messages)[11] <- "client_created_at_num"


########################
### MUNGING MESSAGES ###
########################

users <- users[,c("id", "full_name")]
clients <- clients[,c("client_id", "first_name", "last_name")]

users$user_first_name <- sapply(strsplit(as.character(users$full_name), ' '), function(x) x[1])
users$user_last_name <- sapply(strsplit(as.character(users$full_name), ' '), function(x) x[length(x)])
users <- users[ !(users$id %in% c(1,2,53,106,120,131,136,139,147,154,157,158)), ]

names(clients)[2] <- "client_first_name"
names(clients)[3] <- "client_last_name"

slc.messages <- merge(slc.messages, users, by.x = "user_id", by.y = "id", all.x = TRUE, all.y = FALSE)
slc.messages <- merge(slc.messages, clients, by = "client_id", all.x = TRUE, all.y = FALSE)

temp <- slc.messages

temp$created_at <- as.POSIXct(temp$created_at_num, origin = '1970-01-01')
temp$send_at <- as.POSIXct(temp$send_at_num, origin = '1970-01-01')
temp$client_created_at <- as.POSIXct(temp$client_created_at_num, origin = '1970-01-01')

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
table(temp$send_at_ToD_bins)

# Message contains future appointment date
temp$has_future_date <- as.numeric(temp$dates_ext != "[]") # Indicator of date in text

# Delete "inbound" messages that are not from clients 
temp$system_messages <- as.numeric(grepl("was transferred to you", temp$body))
temp <- temp[temp$system_messages == 0,]
temp$system_messages <- NULL

# Create dataframes
all_messages <- temp
user_messages <- temp[temp$inbound == "False",]
client_messages <- temp[temp$inbound == "True",]

cache('all_messages')
cache('user_messages')
cache('client_messages')


####################################
### RELATIONSHIP LEVEL: PRETRIAL ###
####################################

# outcome data

pretrial_outcomes <- all_messages %>% filter(department_id == 2) %>% 
  filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")

pretrial_outcomes <- unique(pretrial_outcomes[,c("client_id","user_id","discharge_cat","end_dt_num")])

levels(pretrial_outcomes$discharge_cat)
pretrial_outcomes$discharge_cat <- factor(pretrial_outcomes$discharge_cat)
levels(pretrial_outcomes$discharge_cat) <- c(FALSE, TRUE)

names(pretrial_outcomes)[3] <- "supervision_failure"

table(pretrial_outcomes$supervision_failure) # is this right? 41%, much higher failure rate than Baltimore

# quick check
s <- salt.lake.discharges %>% filter(agcy_desc == "PRETRIAL SERVICES") %>% filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")
table(s$discharge_cat) # 37% failure rate
# Manya says failure rate is much higher in SLC, started above 50% and went down with use of CC
# but she would have expected not quite this high...


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

names(ToD_message)[27] <- "ToD_Missing"

ToD_message_count <- aggregate(cbind(ToD_message[,c("Night", "Morning", "Afternoon", "Evening", "ToD_Missing")]), by = list(ToD_message$client_id,
                                                                                                                            ToD_message$user_id), FUN = sum)
colnames(ToD_message_count) <- c("client_id","user_id", "Night_messages", "Morning_messages", "Afternoon_messages", 
                                 "Evening_messages", "ToD_Missing_messages")

# Number of future appointment dates
number_appt_reminders <- aggregate(user_messages$has_future_date, 
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

# merge all variables into one dataframe

temp1 <- merge(pretrial_outcomes, max_scheduled_diff, by=c("user_id","client_id"), 
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


temp1$PO <- as.factor(temp1$user_id)
temp1$user_id <- as.factor(temp1$user_id) 
temp1$client_id <- as.factor(temp1$client_id)

# which PO should be reference level?
table(temp1$supervision_failure,temp1$PO)
temp1$PO <- relevel(temp1$PO, ref="45")

# Create dataframe
slc.pretrial.dtf <- temp1

cache('slc.pretrial.dtf')


#####################
##### PROBATION #####
#####################

# outcome data

probation_outcomes <- all_messages %>% filter(department_id == 1) %>% 
  filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")

probation_outcomes$discharge_cat <- factor(probation_outcomes$discharge_cat)
levels(probation_outcomes$discharge_cat) <- c(FALSE, TRUE)

names(probation_outcomes)[8] <- "supervision_failure"

table(probation_outcomes$supervision_failure) # 8% -- but this is the percentage of msgs, not percentage of relationships...

probation_relationships <- unique(probation_outcomes[,c("client_id","user_id","supervision_failure","end_dt_num")])

table(probation_relationships$supervision_failure) # 17% ???

# quick check
t <- salt.lake.discharges %>% filter(agcy_desc == "PROBATION SERVICES") %>% filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")
table(t$discharge_cat) # 26% failure rate
# why is probation failure rate so much lower in matched data than in outcome data?
# why is matched failure rate so much lower in probation than in pretrial?

all_probation_msgs <- all_messages %>% filter(department_id == 1)

# Number of client messages
client_msg_count <- aggregate(as.numeric(all_probation_msgs$inbound == "True"), 
                                by = list(all_probation_msgs$client_id, all_probation_msgs$user_id, all_probation_msgs$end_dt_num),
                                FUN = sum)
colnames(client_msg_count) <- c("client_id", "user_id","end_dt_num", "client_msg_count")
  
# Number of user messages
user_msg_count <- aggregate(as.numeric(all_probation_msgs$inbound == "False"), 
                              by = list(all_probation_msgs$client_id, all_probation_msgs$user_id, all_probation_msgs$end_dt_num),
                              FUN = sum)
colnames(user_msg_count) <- c("client_id", "user_id","end_dt_num", "user_msg_count")

# Length of client time on CC (for normalization)
time_on_cc <- all_messages %>% mutate(secs_on_cc = end_dt_num - client_created_at_num)

summary(time_on_cc$secs_on_cc) # why are so many of these values negative?
View(probation_outcomes[,c("client_id","user_id","end_dt","client_created_at")])
# right, because CC was restarted on 4/16/2018...

all_messages %>% summarize(min(send_at))
all_messages %>% summarize(min(client_created_at))
# there are messages that are much older than the cliented_created_at date
# therefore use first msg send_at as beginning of relationship instead

time_on_cc <- all_messages %>% mutate(secs_on_cc = end_dt_num - send_at_num)

summary(time_on_cc$secs_on_cc) # much better

time_on_cc$hrs_on_cc <- time_on_cc$secs_on_cc/3600

time_on_cc$months_on_cc <- time_on_cc$hrs_on_cc/725

time_on_cc <- time_on_cc %>% filter(department_id == 1)
  
time_on_cc <- time_on_cc[,c("client_id","user_id","months_on_cc","end_dt_num")]

time_on_cc <- time_on_cc %>% group_by(client_id,user_id,end_dt_num) %>%
  summarize(time_on_cc = max(months_on_cc))

slc.probation.dtf <- inner_join(time_on_cc, user_msg_count, by = c("client_id","user_id","end_dt_num"))

slc.probation.dtf <- inner_join(slc.probation.dtf,client_msg_count, by = c("client_id","user_id","end_dt_num"))

slc.probation.dtf <- slc.probation.dtf %>% mutate(user_msgs_per_month = user_msg_count/time_on_cc,
                                                  client_msgs_per_month = client_msg_count/time_on_cc)

probation_outcomes <- unique(probation_outcomes[,c(1,2,6,8)])

slc.probation.dtf <- inner_join(slc.probation.dtf,probation_outcomes, by = c("client_id","user_id","end_dt_num"))

cache('slc.probation.dtf')


#######################################################################################################
###### COME BACK TO THIS IF DECIDE TO EXAMINE QUALITIES OF PRETRIAL MSGS THAT RECEIVE RESPONSES #######
#######################################################################################################

pretrial_client_msgs <- client_messages %>% filter(department_id == 2) %>% 
  filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")

pretrial_client_msgs$client_id <- as.factor(as.character(pretrial_client_msgs$client_id))

pretrial_user_msgs <- user_messages %>% filter(department_id == 2) %>% 
  filter(discharge_cat == "SUCCESSFUL" | discharge_cat == "UNSUCCESSFUL")

pretrial_user_msgs$client_id <- as.factor(as.character(pretrial_user_msgs$client_id))

first_client_reply <- pretrial_client_msgs %>% group_by(client_id, end_dt_num) %>% summarize(min_time = min(send_at_num)) # time of first client msg

temp2 <- pretrial_user_msgs

temp2 <- merge(temp2, first_client_reply, by = c("client_id","end_dt_num"))

#setdiff(first_client_reply$client_id, pretrial_user_msgs$client_id)
#setdiff(first_client_reply$client_id, temp2$client_id)

temp2 <- temp2 %>% filter(send_at_num < min_time) # PO messages leading to a client reply

setdiff(first_client_reply$client_id, temp2$client_id) # there are still about 100 clients that don't come through into temp2
# ground check looks ok -- 1113, 1230, 1339, 1383 all have first msg in relationship inbound

msgs_leading_to_reply <- temp2

first_msg_replied <- msgs_leading_to_reply %>% group_by(client_id, end_dt_num) %>% slice(which.max(send_at_num)) # the first user msg in a relationship that received a client reply (might be initial msg, might not)

# indicator: is this user msg an initial msg? 

pretrial_initial_msg <- pretrial_user_msgs %>% group_by(client_id, end_dt_num) %>% summarize(initial_msg = min(send_at_num))
temp4 <- merge(pretrial_user_msgs, pretrial_initial_msg, by = c("client_id","end_dt_num"))
temp4 <- temp4 %>% mutate(initial_msg_indicator = case_when(initial_msg == send_at_num ~ 1,
                                                            initial_msg != send_at_num ~ 0))

# indicator: did the client respond to this user msg?

msg_replied <- first_msg_replied[c("client_id","send_at_num")]
msg_replied$msg_replied_i <- 1

temp4 <- merge(temp4, msg_replied, by = c("client_id", "send_at_num"), all.x = TRUE, all.y = TRUE)
temp4$msg_replied_i[is.na(temp4$msg_replied_i)] <- 0

temp4 <- temp4[c("client_id", "user_id", "body", "inbound", 
                 "user_first_name", "user_last_name", "client_first_name", "client_last_name",
                 "created_at", "created_at_backup", "created_at_num", 
                 "send_at", "send_at_backup", "send_at_num",
                 "send_at_DoW", "send_at_ToD_bins", "initial_msg_indicator", "msg_replied_i", 
                 "has_future_date", "scheduled_diff", "user_msg_length")]

################
## ok to here ##
################

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

temp6 <- pretrial_user_msgs %>% full_join(pretrial_user_msgs, by = "user_id") 
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

################
### PO LEVEL ###
################

temp.a <- cc.dtf %>% group_by(PO) %>% tally() %>% mutate(number_of_clients = n) # of clients per PO

cc.dtf$supervision_failure <- as.numeric(cc.dtf$supervision_failure)

# number of failures, summary stats on msg_count by PO
temp.b <- cc.dtf %>% group_by(PO) %>% summarize(number_of_failures = sum(supervision_failure),
                                                med_msg_count = median(user_msg_count),
                                                mean_msg_count = mean(user_msg_count),
                                                min_msg_count = min(user_msg_count),
                                                max_msg_count = max(user_msg_count))

temp.c <- merge(temp.a, temp.b, by = "PO")
temp.c$n <- NULL

temp.c$percent_failures <- temp.c$number_of_failures/temp.c$number_of_clients # % failure by PO

temp.d <- user_msgs_qualities %>% group_by(PO) %>% summarize(mean_reuse_score = mean(max_reuse_score))

temp.e <- merge(temp.c, temp.d, by = "PO", all.x = TRUE, all.y = FALSE)

PO_data <- temp.e

cache('PO_data')



