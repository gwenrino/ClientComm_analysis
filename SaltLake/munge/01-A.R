Sys.setenv(TZ='America/New_York')
library(tidyverse)
library(stringi)
library(ProjectTemplate)

slc.probation.data <- read.csv(file = file.path("data", "slc.probation.data.csv"))
slc.pretrial.data <- read.csv(file = file.path("data", "slc.pretrial.data.csv"))
slc.dept3.data <- read.csv(file = file.path("data", "slc.dept3.data.csv"))
slc.data <- read.csv(file = file.path("data", "slc.data.csv"))
surveys <- read.csv(file = file.path("data", "surveys.csv"))
messages <- read.csv(file = file.path("data", "messages.csv"))
clients <- read.csv(file = file.path("data", "clients.csv"))
salt.lake.discharges <- read.csv(file = file.path("data", "salt.lake.discharges.csv"))

#######################################################
### Linking salt.lake.data and salt.lake.discharges ###
#######################################################

glimpse(salt.lake.data)
glimpse(clients)
glimpse(surveys)

colnames(clients)[10] <- "ofndr_num"
colnames(clients)[1] <- "client_id"

length(unique(salt.lake.discharges$ofndr_num))
# doubled offender numbers sometimes associated with second outcome, sometimes not.
# keep dupes for now

summary(clients$ofndr_num)
temp.1 <- merge(salt.lake.discharges, clients, by = "ofndr_num", all.x = FALSE, all.y = FALSE)

salt.lake.active <- salt.lake.data[salt.lake.data$active == TRUE,]
salt.lake.active$active <- NULL

temp.2 <- merge(salt.lake.active, temp.1, by = "client_id", all.x = FALSE, all.y = FALSE)


## Compare temp.2 with merge of surveys and salt.lake.data
temp.3 <- merge(salt.lake.active, surveys, by.x = c("client_id", "user_id"), 
               by.y = c("surveys_client_id", "surveys_user_id"), 
               all.x = TRUE, all.y = FALSE)

temp.4 <- merge(temp.2, temp.3, by = "client_id")
# looks like a pretty fair amount of overlap...


















# Rate of tech violations

pima.violations <- unique(pima.outcomes[,c(1,2,3,16)])
table(pima.violations$violation)

pima.matched.violations <- merge(pima.violations, pima.matched.active, by = c("client_RSN"))
table(pima.matched.violations$violation)

pima.unmatched.violations <- anti_join(pima.violations, pima.matched.active, by = "client_RSN")
table(pima.unmatched.violations$violation)

prop.test(c(423, 1185), c(789,2095))





###########################
### Munging survey data ###
###########################

# Dedupe surveys
find_multiple_outcomes <- aggregate(surveys$surveys_client_id, by=list(surveys$surveys_client_id, surveys$surveys_user_id), FUN = length)

single_outcomes <- subset(find_multiple_outcomes, x == 1)
colnames(single_outcomes)[1:2] <- c("surveys_client_id", "surveys_user_id")

surveys_deduped <- merge(surveys, single_outcomes, by = c("surveys_client_id", "surveys_user_id"), all.x = FALSE, all.y = TRUE)

# Violation indicator variable
surveys_deduped$violation_i <- as.numeric(grepl("probation violation", surveys_deduped$surveys_text))

# Merge messages and deduped surveys
temp.a <- merge(messages, surveys_deduped, by.x = c("client_id", "user_id"), by.y = c("surveys_client_id", "surveys_user_id"), all.x = FALSE, all.y = FALSE)

relationships <- unique(temp.a[,c("client_id", "user_id")])

relationships <- aggregate(relationships$user_id, by = list(relationships$client_id), FUN = length)
colnames(relationships) <- c("client_id","users_n")

kept.clients <- subset(relationships, users_n==1)

temp.a <- subset(temp.a, client_id %in% kept.clients$client_id)

# Restrict data to exclude last month of relationship (see line 214 for only last month)

difftime('2018-07-01', '2018-08-01', units = 'secs') # how many seconds in a month?

temp.a$cutoff <- temp.a$surveys_created_at_num - 2678400

temp.a <- subset(temp.a, cutoff > send_at_num)

# Find the next PO message after a client message

count_client_msgs <- temp.a %>% group_by(client_id,user_id) %>% summarize(client_msgs_n = sum(inbound == TRUE))

count_PO_msgs <- temp.a %>% group_by(client_id,user_id) %>% summarize(user_msgs_n = sum(inbound == FALSE))

PO_msgs <- subset(temp.a, inbound==FALSE)
names(PO_msgs) <- paste("PO_msgs", names(PO_msgs), sep="_")

client_msgs <- subset(temp.a, inbound==TRUE)
names(client_msgs) <- paste("client_msgs", names(client_msgs), sep="_")

potential_replies <- merge(PO_msgs, client_msgs, by.x=c("PO_msgs_client_id", "PO_msgs_user_id"),
                           by.y=c("client_msgs_client_id", "client_msgs_user_id"), all.x = TRUE, all.y = TRUE)

potential_replies$response_time <- potential_replies$PO_msgs_send_at_num - potential_replies$client_msgs_send_at_num

potential_replies_PO_to_client <- subset(potential_replies, response_time>1)

PO_replies <- potential_replies_PO_to_client %>% group_by(PO_msgs_client_id, PO_msgs_user_id, PO_msgs_id) %>% 
  summarise(PO_reply_time = min(response_time))

temp.b <- PO_replies %>% group_by(PO_msgs_client_id, PO_msgs_user_id) %>% summarize(med_response_time = median(PO_reply_time))

temp.c <- merge(count_client_msgs, count_PO_msgs, by = c("client_id", "user_id"))

temp.c <- merge(temp.c, temp.b, by.x = c("client_id", "user_id"), by.y = c("PO_msgs_client_id", "PO_msgs_user_id"), 
                all.x = TRUE, all.y = TRUE)

temp.d <- merge(temp.c, surveys_deduped, by.x = c("client_id", "user_id"), by.y = c("surveys_client_id", "surveys_user_id"))

# Length of client time on CC (for normalization)

client_time <- clients[,c(1,5)]
names(client_time)[2] <- "client_created_at"
client_time$client_created_at_num <- as.numeric(as.POSIXlt(client_time$client_created_at, tz = "America/New_York"))

temp.e <- temp.d %>% group_by(client_id) %>% summarize(last_survey_num = max(as.numeric(as.POSIXlt(surveys_updated_at))))

temp.e$last_survey <- as.POSIXlt(temp.e$last_survey_num, origin = '1970-01-01')

calc_client_duration <- merge(client_time, temp.e, by=c("client_id"))
calc_client_duration$days_on_cc <- difftime(calc_client_duration$last_survey, calc_client_duration$client_created_at, units="days")
calc_client_duration$months_on_cc <- as.numeric(calc_client_duration$days_on_cc) / 30

temp.f <- merge(temp.d, calc_client_duration, by="client_id")

temp.f$client_msgs_per_month <- temp.f$client_msgs_n/temp.f$months_on_cc
temp.f$user_msgs_per_month <- temp.f$user_msgs_n/temp.f$months_on_cc

temp.f$last_survey <- NULL

temp.f <- subset(temp.f, months_on_cc>1)

temp.f$crime_or_abscond_i <- as.numeric(grepl("Absconded", temp.f$surveys_text) | grepl("criminal", temp.f$surveys_text))

# Number of failures, summary stats on msg_count by PO

PO_messaging <- temp.f %>% group_by(user_id) %>% summarize(number_of_violations = sum(violation_i),
                                                           number_of_crimes = sum(crime_or_abscond_i),
                                                           med_msg_count = median(user_msgs_per_month),
                                                           mean_msg_count = mean(user_msgs_per_month),
                                                           min_msg_count = min(user_msgs_per_month),
                                                           max_msg_count = max(user_msgs_per_month))

number_of_clients <- temp.f %>% group_by(user_id) %>% tally() %>% mutate(number_of_clients = n) # of clients per PO

PO_messaging <- merge(PO_messaging, number_of_clients, by = "user_id")

PO_messaging <- subset(PO_messaging, user_id != 36) # crazy outlier

PO_messaging$percent_violations <- PO_messaging$number_of_violations/PO_messaging$number_of_clients # % failure by PO

PO_messaging$percent_crimes <- PO_messaging$number_of_crimes/PO_messaging$number_of_clients # % failure by PO

min_outcomes <- subset(PO_messaging, number_of_clients>9)
min_outcomes <- subset(min_outcomes, percent_crimes!=0)

min_outcomes[min_outcomes$user_id==36,]
max(min_outcomes$max_msg_count)

pima.relationships <- temp.f

cache('pima.relationships')

# Subset: relationships that ended in tech violations
users_with_failures <- unique(pima.relationships$user_id[pima.relationships$violation_i==1])
pima.relationships_with_failures <- subset(pima.relationships, user_id %in% users_with_failures)

cache('pima.relationships_with_failures')

# The month prior to terminating (survey) -- see line 115

temp.a$cutoff <- temp.a$surveys_created_at_num - 2678400

temp.a$msg_is_in_last_month <- as.numeric(temp.a$send_at_num > temp.a$cutoff)

#temp.a$client_id <- as.factor(temp.a$client_id)
#temp.a$user_id <- as.factor(temp.a$user_id)

PO_outgoing_msgs <- subset(temp.a, inbound==FALSE)

user_msgs_to_clients_in_last_month <- aggregate(PO_outgoing_msgs$msg_is_in_last_month, by=list(PO_outgoing_msgs$user_id, PO_outgoing_msgs$client_id), FUN=sum)
colnames(user_msgs_to_clients_in_last_month) <- c("user_id","client_id","msgs_in_last_month")
user_msgs_to_clients_in_last_month$any_msgs_to_client_i <- as.numeric(user_msgs_to_clients_in_last_month$msgs_in_last_month>0)

final_month_proportion_clients_no_messages_by_user <- aggregate(user_msgs_to_clients_in_last_month$any_msgs_to_client_i, by=list(user_msgs_to_clients_in_last_month$user_id), FUN=mean)
colnames(final_month_proportion_clients_no_messages_by_user) <- c("user_id","proportion_clients_no_final_month_msg")
final_month_proportion_clients_no_messages_by_user$proportion_clients_no_final_month_msg <- 1 - final_month_proportion_clients_no_messages_by_user$proportion_clients_no_final_month_msg

final_month_avg_messages_by_user <- aggregate(user_msgs_to_clients_in_last_month$msgs_in_last_month, by=list(user_msgs_to_clients_in_last_month$user_id), FUN=mean)
colnames(final_month_avg_messages_by_user) <- c("user_id","avg_msgs_to_clients_final_month")

final_month_number_clients_n_messages_by_user <- aggregate(user_msgs_to_clients_in_last_month$any_msgs_to_client_i, by=list(user_msgs_to_clients_in_last_month$user_id), FUN=length)
colnames(final_month_number_clients_n_messages_by_user) <- c("user_id","n_clients_w_survey_outcome")

final_month_stats_by_user <- merge(final_month_proportion_clients_no_messages_by_user, final_month_avg_messages_by_user, 
                                   by="user_id")

final_month_stats_by_user <- merge(final_month_stats_by_user, final_month_number_clients_n_messages_by_user, by="user_id")

PO_client_violations <- unique(temp.a[,c(1,2,31)])

client_violation_rate <- aggregate(PO_client_violations$violation_i, by=list(PO_client_violations$user_id), FUN=mean)
colnames(client_violation_rate) <- c("user_id","violation_rate")

final_month_stats_by_user <- merge(final_month_stats_by_user, client_violation_rate, by="user_id")

write.csv(final_month_stats_by_user, "final_month_stats_by_user.csv", row.names=FALSE)

unreliable_reporting <- c(4,7,13,18,20,21,22,24,28,29,32,37,42,45,56,61,62,66,75)

reliable_reporting <- subset(temp.a, !(user_id %in% unreliable_reporting))

####################################
### Link clients and risk levels ###
####################################

temp5a <- merge(clients, risk_levels, by.x = "phone_number", by.y = "tel_num", all.x = FALSE, all.y = FALSE)

temp5a <- merge(temp5a, temp.f, by = c("client_id", "user_id"), all.x = FALSE, all.y = FALSE)
# 157 matches

# Manya says column a in officers_client_roster is the client_id, try matching on this
temp5b <- merge(clients, risk_levels, by.x = "client_id", by.y = "X", all.x = FALSE, all.y = FALSE)

temp5b <- merge(temp5b, temp.f, by = c("client_id", "user_id"), all.x = FALSE, all.y = FALSE)
# 256 matches

temp5c <- merge(clients, risk_levels, by.x = c("phone_number", "client_id"), by.y = c("tel_num", "X"), all.x = FALSE, all.y = FALSE)
# no matches at all, so maybe the client_id match was a red herring?


