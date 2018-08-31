Sys.setenv(TZ='America/New_York')
library(tidyverse)

pima.data <- read.csv(file = file.path("data", "pima.data.csv"))
messages <- read.csv(file = file.path("data", "messages.csv"))
surveys <- read.csv(file = file.path("data", "surveys.csv"))

# Dedupe
find_multiple_outcomes <- aggregate(surveys$surveys_client_id, by=list(surveys$surveys_client_id, surveys$surveys_user_id), FUN = length)

single_outcomes <- subset(find_multiple_outcomes, x == 1)
colnames(single_outcomes)[1:2] <- c("surveys_client_id", "surveys_user_id")

surveys_deduped <- merge(surveys, single_outcomes, by = c("surveys_client_id", "surveys_user_id"), all.x = FALSE, all.y = TRUE)

surveys_deduped$violation_i <- as.numeric(grepl("probation violation", surveys_deduped$surveys_text))

# merge

temp.a <- merge(messages, surveys_deduped, by.x = c("client_id", "user_id"), by.y = c("surveys_client_id", "surveys_user_id"), all.x = FALSE, all.y = FALSE)

length(unique(temp.a$client_id))
relationships <- unique(temp.a[,c("client_id", "user_id")])

relationships <- aggregate(relationships$user_id, by = list(relationships$client_id), FUN = length)
colnames(relationships) <- c("client_id","users_n")
kept.clients <- subset(relationships, users_n==1)

temp.a <- subset(temp.a, client_id %in% kept.clients$client_id)

count_client_msgs <- temp.a %>% group_by(client_id,user_id) %>% summarize(client_msgs_n = sum(inbound == TRUE))

count_PO_msgs <- temp.a %>% group_by(client_id,user_id) %>% summarize(user_msgs_n = sum(inbound == FALSE))

PO_msgs <- subset(temp.a, inbound==FALSE)
names(PO_msgs) <- paste("PO_msgs", names(PO_msgs), sep="_")

client_msgs <- subset(temp.a, inbound==TRUE)
names(client_msgs) <- paste("client_msgs", names(client_msgs), sep="_")

potential_replies <- merge(PO_msgs, client_msgs, by.x=c("PO_msgs_client_id", "PO_msgs_user_id"),
                           by.y=c("client_msgs_client_id", "client_msgs_user_id"), all.x = TRUE, all.y = TRUE)

potential_replies$response_time <- potential_replies$PO_msgs_send_at_num - potential_replies$client_msgs_send_at_num
summary(potential_replies_PO_to_client$response_time)

potential_replies_PO_to_client <- subset(potential_replies, response_time>1)

length(unique(potential_replies_PO_to_client$PO_msgs_client_id))
head(potential_replies_PO_to_client)

PO_replies <- potential_replies_PO_to_client %>% group_by(PO_msgs_client_id, PO_msgs_user_id, PO_msgs_id) %>% summarise(PO_reply_time = min(response_time))

quantile(PO_replies$PO_reply_time,seq(0,1,.01))

temp.b <- PO_replies %>% group_by(PO_msgs_client_id, PO_msgs_user_id) %>% summarize(med_response_time = median(PO_reply_time))

temp.c <- merge(count_client_msgs, count_PO_msgs, by = c("client_id", "user_id"))

temp.c <- merge(temp.c, temp.b, by.x = c("client_id", "user_id"), by.y = c("PO_msgs_client_id", "PO_msgs_user_id"), 
                all.x = TRUE, all.y = TRUE)

temp.d <- merge(temp.c, surveys_deduped, by.x = c("client_id", "user_id"), by.y = c("surveys_client_id", "surveys_user_id"))


##

names(temp.d)
names(clients)
client_time <- clients[,c(1,5)]
names(client_time)[2] <- "client_created_at"
head(client_time)
client_time$client_created_at_num <- as.numeric(as.POSIXlt(client_time$client_created_at, tz = "America/New_York"))
names(temp.d)
summary(temp.d)

temp.e <- temp.d %>% group_by(client_id) %>% summarize(last_survey_num = max(as.numeric(as.POSIXlt(surveys_updated_at))))

temp.e$last_survey <- as.POSIXlt(temp.e$last_survey_num, origin = '1970-01-01')

calc_client_duration <- merge(client_time, temp.e, by=c("client_id"))
head(calc_client_duration)

length(unique(calc_client_duration$client_id))

calc_client_duration$days_on_cc <- difftime(calc_client_duration$last_survey, calc_client_duration$client_created_at, units="days")
calc_client_duration$months_on_cc <- as.numeric(calc_client_duration$days_on_cc) / 30

temp.f <- merge(temp.d, calc_client_duration, by="client_id")

temp.f$client_msgs_per_month <- temp.f$client_msgs_n/temp.f$months_on_cc
temp.f$user_msgs_per_month <- temp.f$user_msgs_n/temp.f$months_on_cc

### PO analysis

# number of failures, summary stats on msg_count by PO
temp.f$last_survey <- NULL

temp.f <- subset(temp.f, months_on_cc>1)

temp.f$crime_or_abscond_i <- as.numeric(grepl("Absconded", temp.f$surveys_text) | grepl("criminal", temp.f$surveys_text))
table(temp.f$crime_or_abscond_i)
names(temp.f)
PO_messaging <- temp.f %>% group_by(user_id) %>% summarize(number_of_violations = sum(violation_i),
                                                           number_of_crimes = sum(crime_or_abscond_i),
                                                           med_msg_count = median(user_msgs_per_month),
                                                           mean_msg_count = mean(user_msgs_per_month),
                                                           min_msg_count = min(user_msgs_per_month),
                                                           max_msg_count = max(user_msgs_per_month))

head(PO_messaging)
number_of_clients <- temp.f %>% group_by(user_id) %>% tally() %>% mutate(number_of_clients = n) # of clients per PO

PO_messaging <- merge(PO_messaging, number_of_clients, by = "user_id")

PO_messaging <- subset(PO_messaging, user_id != 36)

PO_messaging$percent_violations <- PO_messaging$number_of_violations/PO_messaging$number_of_clients # % failure by PO

PO_messaging$percent_crimes <- PO_messaging$number_of_crimes/PO_messaging$number_of_clients # % failure by PO
table(PO_messaging$number_of_violations)
table(PO_messaging$number_of_clients, PO_messaging$number_of_violations)

min_outcomes <- subset(PO_messaging, number_of_clients>9)
min_outcomes <- subset(min_outcomes, percent_crimes!=0)

min_outcomes[min_outcomes$user_id==36,]
max(min_outcomes$max_msg_count)

p0 <- ggplot(min_outcomes, aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x))
p0

p1 <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "lm", se = FALSE)# Yes
p1 <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x))# Yes
p1 <- ggplot(min_outcomes[c(min_outcomes$mean_msg_count<10),], aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ splines::ns(x,df=2))# Yes
p1 <- ggplot(min_outcomes, aes(x = mean_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "gam", se = TRUE, formula=y~ poly(x,2))# Yes
p2 <- ggplot(PO_messaging, aes(x = max_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "lm", se = FALSE) # Yes
p5 <- ggplot(PO_messaging, aes(x = med_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
p1
p5
?geom_smooth



ggsave("Median_msg_count.png", width = 6, height = 4)

ggsave("Median_msg_count_crimes.png", width = 6, height = 4)


##

pima.relationships <- temp.f

# mean_msgs_per_month v probability of violation
mean(pima.relationships_with_failures$user_msgs_per_month[pima.relationships_with_failures$violation_i==1])
mean(pima.relationships_with_failures$user_msgs_per_month[pima.relationships_with_failures$violation_i==0])

pima.tv1 <- glm(violation_i ~ client_msgs_per_month, data=pima.relationships, family=binomial)
summary(pima.tv1)

#[c(pima.relationships$user_msgs_per_month<10),]

users_with_failures <- unique(pima.relationships$user_id[pima.relationships$violation_i==1])
pima.relationships_with_failures <- subset(pima.relationships, user_id %in% users_with_failures)

pima.tv2 <- glm(violation_i ~ (client_msgs_n>0) + user_msgs_per_month + as.factor(user_id) + months_on_cc, 
                data=pima.relationships_with_failures[c(pima.relationships_with_failures$user_msgs_per_month<10),], 
                family=binomial)
summary(pima.tv2)
table(pima.relationships$violation_i, round(pima.relationships$user_msgs_per_month,0))
length(unique(temp.d$user_id))
summary(pima.relationships_with_failures)

plot(density(pima.relationships$user_msgs_per_month))

pima.tv3 <- glm(violation_i ~ client_msgs_n + user_msgs_n, data=temp.d, family=binomial)
summary(pima.tv3)

pima.tv4 <- glm(violation_i ~ user_msgs_n, data=temp.d, family=binomial)
summary(pima.tv4)

table(temp.d$user_msgs_n)

# limited time (3 months?)
# poisson by month?
# msgs per 3 months
# other outcomes?
# PO's success rates by number of months on CC
# number of months of PO experience at end of client supervision



