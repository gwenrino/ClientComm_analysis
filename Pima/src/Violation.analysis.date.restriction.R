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

## diverge here from Violation.analysis script

difftime('2018-07-01', '2018-08-01', units = 'secs')

temp.a$cutoff <- temp.a$surveys_created_at_num - 2678400

temp.a <- subset(temp.a, cutoff > send_at_num)

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

# number of failures, summary stats on msg_count by PO
temp.f$last_survey <- NULL

temp.f <- subset(temp.f, months_on_cc>1)

temp.f$crime_or_abscond_i <- as.numeric(grepl("Absconded", temp.f$surveys_text) | grepl("criminal", temp.f$surveys_text))
table(temp.f$crime_or_abscond_i)
names(temp.f)

### Link clients and risk levels

# Match on phone number
risk_levels <- read.csv(file = file.path("data", "officers_client_roster_cleaned.csv"))

temp5a <- merge(clients, risk_levels, by.x = "phone_number", by.y = "tel_num", all.x = FALSE, all.y = FALSE)

temp5a <- merge(temp5a, temp.f, by = c("client_id", "user_id"), all.x = FALSE, all.y = FALSE)
# 157 matches

# Manya says column a in officers_client_roster is the client_id, try matching on this
temp5b <- merge(clients, risk_levels, by.x = "client_id", by.y = "X", all.x = FALSE, all.y = FALSE)

temp5b <- merge(temp5b, temp.f, by = c("client_id", "user_id"), all.x = FALSE, all.y = FALSE)
# 256 matches

temp5c <- merge(clients, risk_levels, by.x = c("phone_number", "client_id"), by.y = c("tel_num", "X"), all.x = FALSE, all.y = FALSE)
# no matches at all, so maybe the client_id match was a red herring?



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

p1.crimes <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "lm", se = FALSE)# Yes
p1 <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x))# Yes
p1 <- ggplot(min_outcomes[c(min_outcomes$mean_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ splines::ns(x,df=2))# Yes
p1.violations <- ggplot(min_outcomes, aes(x = mean_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x))# Yes
p2 <- ggplot(PO_messaging, aes(x = max_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "lm", se = FALSE) # Yes
p5 <- ggplot(PO_messaging, aes(x = med_msg_count, y = percent_violations)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
p1
p5
?geom_smooth



ggsave("Median_msg_count.png", width = 6, height = 4)

ggsave("Median_msg_count_crimes.png", width = 6, height = 4)


p1.violations <- ggplot(min_outcomes, aes(x = med_msg_count, y = percent_violations)) + 
  geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x)) # changed to median
p1.crimes <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE)# Yes


p1.failures <- ggplot(min_outcomes, aes(x = med_msg_count)) + 
  geom_point(aes(y = percent_violations, color = "percent_violations")) + 
  geom_smooth(aes(y = percent_violations, color = "percent_violations"), method = "gam", se = FALSE, formula=y~ log(x)) + 
  geom_point(aes(y = percent_crimes, color = "percent_crimes")) +
  geom_smooth(aes(y = percent_crimes, color = "percent_crimes"), method = "lm", se = FALSE) + 
  scale_colour_manual("", 
                      breaks = c("percent_violations", "percent_crimes"), 
                      values = c("blue", "red")) +
  labs(title="Pima Probation Officers' ClientComm Engagement",
       subtitle="vs. Rates of Negative Outcomes", 
       x="Median Messages Sent Per Month", 
       y="Rates of Negative Outcomes")

p1.failures
ggsave("Rates_of_negative_outcomes.png", width = 6, height = 4)

p1.violations <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<6 & min_outcomes$percent_violations>0),], 
                        aes(x = med_msg_count, y = percent_violations)) + 
  geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x)) +
  labs(title="Pima Probation Officers' ClientComm Engagement",
       subtitle="vs. Rate of Technical Violations",
       x="Median Messages Sent Per Month",
       y="Rate of Technical Violations")

p1.violations
ggsave("Rate_of_techinal_violations.png", width = 6, height = 4)

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

names(pima.relationships_with_failures)
pima.tv2 <- glm(violation_i ~ (client_msgs_n>0) + log(user_msgs_per_month + 1) + months_on_cc, 
                data=pima.relationships_with_failures[c(pima.relationships_with_failures$user_msgs_per_month<10),], 
                family=binomial)
summary(pima.tv2)
table(pima.relationships$violation_i, round(pima.relationships$user_msgs_per_month,0))
length(unique(temp.d$user_id))
summary(pima.relationships_with_failures)

pima.tv3 <- glm(violation_i ~ (client_msgs_per_month) * (user_msgs_per_month>1) + months_on_cc + as.factor(user_id), 
                data=reduced_pima, 
                family=binomial)
summary(pima.tv3)

median(pima.relationships_with_failures$user_msgs_per_month)
table(round(pima.relationships_with_failures$user_msgs_per_month, 0))
table(round(pima.relationships_with_failures$client_msgs_per_month, 0))


pima.tv3.h <- glmer(violation_i ~ (client_msgs_per_month) * (user_msgs_per_month>1) + 
                      months_on_cc + (1 | user_id), 
                data=reduced_pima, control = glmerControl(optimizer = "bobyqa"), 
                family=binomial)
summary(pima.tv3.h)

pima.tv3.h.reduced <- glmer(violation_i ~ (client_msgs_per_month) * log(user_msgs_per_month+1) + 
                      months_on_cc + (1 | user_id), 
                    data=reduced_pima, control = glmerControl(optimizer = "bobyqa"), 
                    family=binomial)
summary(pima.tv3.h.reduced)


pred_out <- predict(pima.tv3.h.reduced, reduced_pima, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(round(pred_out,0)==reduced_pima$violation_i, na.rm=TRUE)/(nrow(reduced_pima))

pima.tv3.h2 <- gam(violation_i ~ (client_msgs_per_month) * (user_msgs_per_month) + 
                      months_on_cc + as.factor(user_id), 
                    data=reduced_pima, 
                    family=binomial)
summary(pima.tv3.h2)

pima.tv3.h3 <- glmer(violation_i ~ (client_msgs_per_month>1) +
                       (user_msgs_per_month>.5 & user_msgs_per_month<3) +
                      months_on_cc + (1 | user_id), 
                    data=reduced_pima, control = glmerControl(optimizer = "bobyqa"), 
                    family=binomial)
summary(pima.tv3.h3)

library(mgcv)
pima.tv4.h <- gam(violation_i ~ as.factor(client_msgs_per_month>.75) + poly((user_msgs_per_month),2) + 
                    as.factor(user_id) + months_on_cc, 
                    data=pima.relationships_with_failures, 
                    family=binomial)
summary(pima.tv4.h)

pima.tv4.h_interact <- gam(violation_i ~ client_msgs_per_month * poly((user_msgs_per_month),2) + 
                    as.factor(user_id) + months_on_cc, 
                  data=pima.relationships_with_failures, 
                  family=binomial)
summary(pima.tv4.h_interact)


pima.tv5.h <- glmer(violation_i ~ (user_msgs_per_month>7)  + (client_msgs_per_month>.75) + 
                      (user_msgs_per_month>.75 & user_msgs_per_month<4) + months_on_cc + (1 | user_id), 
                    data=reduced_pima, 
                    family=binomial,  control = glmerControl(optimizer = "bobyqa"))
summary(pima.tv5.h)

pima.tv6.h <- glmer(violation_i ~ (user_msgs_per_month>7)  + client_msgs_per_month * (user_msgs_per_month<.75) + months_on_cc + (1 | user_id), 
                    data=reduced_pima, 
                    family=binomial,  control = glmerControl(optimizer = "bobyqa"))
summary(pima.tv6.h)

#[c(pima.relationships_with_failures$user_msgs_per_month<10),]


## Effect size pima.tv3.h ## model makes sense, but predictions are confusing
pima.client_msg_count_0 <- pima.relationships_with_failures
pima.client_msg_count_0$client_msgs_per_month <- 0
pima.client_msg_count_0$user_msgs_per_month <- 2

pima.client_msg_count_25 <- pima.relationships_with_failures
pima.client_msg_count_25$client_msgs_per_month <- .25
pima.client_msg_count_25$user_msgs_per_month <- .5

pima.client_msg_count_1 <- pima.relationships_with_failures
pima.client_msg_count_1$client_msgs_per_month <- 1
pima.client_msg_count_1$user_msgs_per_month <- 2

pima.client_msg_count_2 <- pima.relationships_with_failures
pima.client_msg_count_2$client_msgs_per_month <- 2
pima.client_msg_count_2$user_msgs_per_month <- .5

pima.client_msg_count_3 <- pima.relationships_with_failures
pima.client_msg_count_3$client_msgs_per_month <- 3
pima.client_msg_count_3$user_msgs_per_month <- 2

predictions_client_msg_count_0 <- predict(pima.tv3.h, newdata = pima.client_msg_count_0, type = "response")
predictions_client_msg_count_25 <- predict(pima.tv3.h, newdata = pima.client_msg_count_25, type = "response")
predictions_client_msg_count_1 <- predict(pima.tv3.h, newdata = pima.client_msg_count_1, type = "response")
predictions_client_msg_count_2 <- predict(pima.tv3.h, newdata = pima.client_msg_count_2, type = "response")
predictions_client_msg_count_3 <- predict(pima.tv3.h, newdata = pima.client_msg_count_3, type = "response")

mean(predictions_client_msg_count_0) # client(0), user(0) 16.5%
mean(predictions_client_msg_count_0) # client(0), user(2) 13.4%

mean(predictions_client_msg_count_1) # client(1), user(0) 5.8%
mean(predictions_client_msg_count_1) # client(1), user(2) 13.8%

mean(predictions_client_msg_count_2) # client(2), user(0) 1.8%
mean(predictions_client_msg_count_2) # client(2), user(2) 14.2%

mean(predictions_client_msg_count_3) # client(3), user(0) 0.06%
mean(predictions_client_msg_count_3) # client(3), user(2) 14.7%


mean(predictions_client_msg_count_25) # client(.25), user(.5) 12.9%
mean(predictions_client_msg_count_25) # client(.25), user(2) 13.5%

mean(predictions_client_msg_count_2) # client(2), user(.5) 1.8%
mean(predictions_client_msg_count_2) # client(2), user(2) 14.3%

pred.dtf <- data.frame(matrix(nrow=0,ncol=3))

for (k in c(.25,1)) {
  for (i in seq(.25,3,.5)) {
    newData <- reduced_pima
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict(pima.tv3.h2, newData, type="response")
    new.row <- c(i, k, mean(test.dtf, na.rm=TRUE))
    pred.dtf <- rbind(pred.dtf, new.row)
  }
}
colnames(pred.dtf) <- c("user_msgs_per_month","client_msgs_per_month","violation_probability")

# pima.tv3.h2 ## Yes on predictions, but model is confusing
pima.user_msg_count_25 <- pima.relationships_with_failures
pima.user_msg_count_25$user_msgs_per_month <- .25
pima.user_msg_count_25$client_msgs_per_month <- 3

pima.user_msg_count_1 <- pima.relationships_with_failures
pima.user_msg_count_1$user_msgs_per_month <- 1
pima.user_msg_count_1$client_msgs_per_month <- 3

pima.user_msg_count_2 <- pima.relationships_with_failures
pima.user_msg_count_2$user_msgs_per_month <- 2
pima.user_msg_count_2$client_msgs_per_month <- 3

pima.user_msg_count_7 <- pima.relationships_with_failures
pima.user_msg_count_7$user_msgs_per_month <- 7
pima.user_msg_count_7$client_msgs_per_month <- 0

predictions_user_msg_count_25 <- predict(pima.tv3.h2, newdata = pima.user_msg_count_25, type = "response")
predictions_user_msg_count_1 <- predict(pima.tv3.h2, newdata = pima.user_msg_count_1, type = "response")
predictions_user_msg_count_2 <- predict(pima.tv3.h2, newdata = pima.user_msg_count_2, type = "response")
predictions_user_msg_count_7 <- predict(pima.tv3.h2, newdata = pima.user_msg_count_7, type = "response")

mean(predictions_user_msg_count_25) # client(0), user(.25) 11.4%
mean(predictions_user_msg_count_1) # client(0), user(1) 13.6%
mean(predictions_user_msg_count_2) # client(0), user(2) 17.1%
mean(predictions_user_msg_count_7) # client(0), user(7) 43.2%

mean(predictions_user_msg_count_25) # client(1), user(.25) 10.6%
mean(predictions_user_msg_count_1) # client(1), user(1) 12.7%
mean(predictions_user_msg_count_2) # client(1), user(2) 16.1%
mean(predictions_user_msg_count_7) # client(1), user(7) 41.3%

mean(predictions_user_msg_count_25) # client(3), user(.25) 9.2%
mean(predictions_user_msg_count_1) # client(3), user(1) 11.1%
mean(predictions_user_msg_count_2) # client(3), user(2) 14.1%
mean(predictions_user_msg_count_7) # client(3), user(7) 37.6%



# pima.tv4.h ## YES on predictions, but model is confusing
predictions_client_msg_count_0 <- predict(pima.tv4.h, newdata = pima.client_msg_count_0, type = "response")
predictions_client_msg_count_1 <- predict(pima.tv4.h, newdata = pima.client_msg_count_1, type = "response")
mean(predictions_client_msg_count_0) 
mean(predictions_client_msg_count_1) 

predictions_user_msg_count_25 <- predict(pima.tv4.h, newdata = pima.user_msg_count_25, type = "response")
predictions_user_msg_count_1 <- predict(pima.tv4.h, newdata = pima.user_msg_count_1, type = "response")
predictions_user_msg_count_2 <- predict(pima.tv4.h, newdata = pima.user_msg_count_2, type = "response")
predictions_user_msg_count_7 <- predict(pima.tv4.h, newdata = pima.user_msg_count_7, type = "response")

mean(predictions_user_msg_count_25) # client(0), user(.25) 13.5%
mean(predictions_user_msg_count_1) # client(0), user(1) 16.7%
mean(predictions_user_msg_count_2) # client(0), user(2) 21.4%
mean(predictions_user_msg_count_7) # client(0), user(7) 49.3%

mean(predictions_user_msg_count_25) # client(1), user(.25) 5.5%
mean(predictions_user_msg_count_1) # client(1), user(1) 7.1%
mean(predictions_user_msg_count_2) # client(1), user(2) 9.5%
mean(predictions_user_msg_count_7) # client(1), user(7) 28.1%


## What the heck, let's try a random forest!
## Goal = best predictions, not most interpretable model


#start with your logistic regression model

pima.relationships_with_failures$user_id <- as.factor(pima.relationships_with_failures$user_id)

dummy.model <- glm(violation_i ~ client_msgs_per_month + user_msgs_per_month + 
                     months_on_cc + user_id, data = pima.relationships_with_failures,
                   family = binomial)

#see what % of outcomes would be accurately predicted with new data (here I just used the training data, which is a no-no in general)
pred_out <- predict(dummy.model, pima.relationships_with_failures, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(as.factor(as.logical(round(pred_out,0)))==pima.relationships_with_failures$violation_i, na.rm=TRUE)/(nrow(pima.relationships_with_failures))


#taking variable names out to use them in the superlearner model
model_vars <- names(dummy.model$model)

data <- pima.relationships_with_failures[,(model_vars)]

data <- data[complete.cases(data),]

library(SuperLearner)

cv.cntrl <- SuperLearner.CV.control(V = 5L, stratifyCV = FALSE, shuffle = TRUE,
                                    validRows = NULL)

X_train <- data
#outcome data for superlearner needs to be numeric
Y_train <- data$violation_i

#be sure to drop your outcome variable from your training dataset:
X_train$violation_i <- NULL

SL.library <- c("SL.xgboost","SL.ranger","SL.ksvm", "SL.glmnet", "SL.speedglm", "SL.cforest")

sl = SuperLearner(Y = Y_train, X = X_train, family = binomial(), method="method.AUC", verbose=TRUE, 
                  SL.library = SL.library, cvControl = cv.cntrl)

sl
#how accurate are the superlearner predictions (what's the improvement over the logistic regression with the same variables?)
sum(round(sl$SL.predict,0)==Y_train)/length(Y_train)

#be mindful that superlearner needs variables that were numeric in the model to be numeric in the prediction dataset, integer as integer, etc.

#create empty data frame to store results
pred.dtf <- data.frame(matrix(nrow=0,ncol=3))

summary(X_train)
summary(newData)

#see what the effect of a hypothetical change in the data might be (here, predicted effect of moving from 0 to 5 verification documents)
for (k in 0:5) {
for (i in 0:5) {
  newData <- X_train
  newData$user_msgs_per_month <- as.numeric(i)
  newData$client_msgs_per_month <- as.numeric(k)
  test.dtf <- predict.SuperLearner(sl, newData, onlySL=TRUE)
  new.row <- c(i, k, mean(test.dtf$pred, na.rm=TRUE))
  pred.dtf <- rbind(pred.dtf, new.row)
}
}
colnames(pred.dtf) <- c("user_msgs_per_month","client_msgs_per_month","violation_probability")

reduced_pima <- subset(pima.relationships_with_failures, client_msgs_per_month<10 & user_msgs_per_month<10)

pima.tv3.h <- glmer(violation_i ~ client_msgs_per_month * user_msgs_per_month +
                      (1 | user_id) + months_on_cc,
                    data=reduced_pima,
                    family=binomial, control = glmerControl(optimizer = "bobyqa"))
summary(pima.tv3.h)


pima.tv3.h2 <- glmer(violation_i ~ (client_msgs_per_month>.75) + poly(user_msgs_per_month,2) +
                     months_on_cc + (1 | user_id),
                   data=reduced_pima,
                   family=binomial)
summary(pima.tv3.h2)

pred_out <- predict(pima.tv3.h.reduced, reduced_pima, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(round(pred_out,0)==reduced_pima$violation_i, na.rm=TRUE)/(nrow(reduced_pima))


pred.dtf <- data.frame(matrix(nrow=0,ncol=3))

#predict using regression instead
for (k in seq(.08,1.7,.67)) {
  for (i in seq(.08,4,.01)) {
    newData <- pima.relationships_with_failures
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict(pima.tv3.h2, newData, type="response")
    new.row <- c(i, k, mean(test.dtf, na.rm=TRUE))
    pred.dtf <- rbind(pred.dtf, new.row)
  }
}
colnames(pred.dtf) <- c("user_msgs_per_month","client_msgs_per_month","violation_probability")

ggplot()


summary(pima.tv3.h2)
pred.dtf %>% group_by(user_msgs_per_month) %>% ggplot

p1.preds <- ggplot(pred.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y = violation_probability, color = "violation_probability"))


### 

pima.tv3.h2 <- gam(violation_i ~ (client_msgs_per_month) * (user_msgs_per_month) + 
                     months_on_cc + as.factor(user_id), 
                   data=pima.relationships_with_failures, 
                   family=binomial)
summary(pima.tv3.h2)

