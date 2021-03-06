library(tidyverse)

### Descriptive stats about effect of CC usage

# First date of CC
summarize(all_messages, min = min(send_at)) # 2016-05-02

# Filter out any non_CC outcomes prior to this date
non_CC_pretrial_outcomes$new_end_date <- strptime(as.character(non_CC_pretrial_outcomes$end_dt), "%m/%d/%y")
non_CC_pretrial_outcomes$new_end_date <- as.POSIXct(non_CC_pretrial_outcomes$new_end_date)
summarize(non_CC_pretrial_outcomes, min = min(new_end_date)) # none prior

non_CC_probation_outcomes$new_end_date <- strptime(as.character(non_CC_probation_outcomes$end_dt), "%m/%d/%y")
non_CC_probation_outcomes$new_end_date <- as.POSIXct(non_CC_probation_outcomes$new_end_date)
summarize(non_CC_probation_outcomes, min = min(new_end_date)) # none prior

# Compare CC and non-CC failure rates
nrow(slc.pretrial.dtf[slc.pretrial.dtf$supervision_failure == TRUE,])/nrow(slc.pretrial.dtf) # 41.9% of 491 outcomes
nrow(non_CC_pretrial_outcomes[non_CC_pretrial_outcomes$supervision_failure == TRUE,])/nrow(non_CC_pretrial_outcomes) # 36.5% of 3667 outcomes

nrow(slc.probation.dtf[slc.probation.dtf$supervision_failure == TRUE,])/nrow(slc.probation.dtf) # 17.0% of 312 outcomes
nrow(non_CC_probation_outcomes[non_CC_probation_outcomes$supervision_failure == TRUE,])/nrow(non_CC_probation_outcomes) # 28.2% of 1088 outcomes

# Among CC clients, effect of # of client msgs for typical usage POs

# Pretrial

summary(slc.pretrial.dtf$user_msg_count) # middle 50% usage is between 2 and 6
pretrial_typical_PO_usage <- slc.pretrial.dtf[slc.pretrial.dtf$user_msg_count >= 2 & slc.pretrial.dtf$user_msg_count <= 6,]
pretrial_low_PO_usage <- slc.pretrial.dtf[slc.pretrial.dtf$user_msg_count < 2,]

nrow(pretrial_typical_PO_usage[pretrial_typical_PO_usage$supervision_failure == TRUE,])/
  nrow(pretrial_typical_PO_usage) # 42.3% of 286 observations

pretrial_typical_PO_0_client_usage <- pretrial_typical_PO_usage[pretrial_typical_PO_usage$client_msg_count == 0,]
pretrial_typical_PO_not0_client_usage <- pretrial_typical_PO_usage[pretrial_typical_PO_usage$client_msg_count != 0,]

nrow(pretrial_typical_PO_0_client_usage[pretrial_typical_PO_0_client_usage$supervision_failure == TRUE,])/
  nrow(pretrial_typical_PO_0_client_usage) # 45.8% of 144 observations

nrow(pretrial_typical_PO_not0_client_usage[pretrial_typical_PO_not0_client_usage$supervision_failure == TRUE,])/
  nrow(pretrial_typical_PO_not0_client_usage) # 38.7% of 142 observations

pretrial_low_PO_0_client_usage <- pretrial_low_PO_usage[pretrial_low_PO_usage$client_msg_count == 0,]
pretrial_low_PO_not0_client_usage <- pretrial_low_PO_usage[pretrial_low_PO_usage$client_msg_count != 0,]

nrow(pretrial_low_PO_0_client_usage[pretrial_low_PO_0_client_usage$supervision_failure == TRUE,])/
  nrow(pretrial_low_PO_0_client_usage) # 58.1% of 62 observations

nrow(pretrial_low_PO_not0_client_usage[pretrial_low_PO_not0_client_usage$supervision_failure == TRUE,])/
  nrow(pretrial_low_PO_not0_client_usage) # 28.6% of 28 observations

nrow(slc.pretrial.dtf[slc.pretrial.dtf$client_msg_count > 0,])/nrow(slc.pretrial.dtf) # 52.5% of pretrial clients send at least one msg

# Probation

summary(slc.probation.dtf$user_msgs_per_month) # middle 50% usage is between 1.6 and 4.75
probation_typical_PO_usage <- slc.probation.dtf[slc.probation.dtf$user_msgs_per_month >= 1.6 & slc.probation.dtf$user_msgs_per_month <= 4.75,]
probation_low_PO_usage <- slc.probation.dtf[slc.probation.dtf$user_msgs_per_month < 1.6,]

nrow(probation_typical_PO_usage[probation_typical_PO_usage$supervision_failure == TRUE,])/
  nrow(probation_typical_PO_usage) # 13.3% of 158 observations

probation_typical_PO_0_client_usage <- probation_typical_PO_usage[probation_typical_PO_usage$client_msgs_per_month == 0,]
probation_typical_PO_low_client_usage <- probation_typical_PO_usage[probation_typical_PO_usage$client_msgs_per_month < 1.5,]
probation_typical_PO_not0_client_usage <- probation_typical_PO_usage[probation_typical_PO_usage$client_msgs_per_month != 0,]
probation_typical_PO_typical_client_usage <- probation_typical_PO_usage[probation_typical_PO_usage$client_msgs_per_month >= 1.5,]

nrow(probation_typical_PO_0_client_usage[probation_typical_PO_0_client_usage$supervision_failure == TRUE,])/
  nrow(probation_typical_PO_0_client_usage) # 60% of 5 observations

nrow(probation_typical_PO_not0_client_usage[probation_typical_PO_not0_client_usage$supervision_failure == TRUE,])/
  nrow(probation_typical_PO_not0_client_usage) # 11.7% of 153 observations

nrow(probation_typical_PO_low_client_usage[probation_typical_PO_low_client_usage$supervision_failure == TRUE,])/
  nrow(probation_typical_PO_low_client_usage) # 27.8% of 36 observations

nrow(probation_typical_PO_typical_client_usage[probation_typical_PO_typical_client_usage$supervision_failure == TRUE,])/
  nrow(probation_typical_PO_typical_client_usage) # 9.0% of 122 observations

probation_low_PO_low_client_usage <- probation_low_PO_usage[probation_low_PO_usage$client_msgs_per_month < 1.5,]
probation_low_PO_typical_client_usage <- probation_low_PO_usage[probation_low_PO_usage$client_msgs_per_month >= 1.5,]

nrow(probation_low_PO_low_client_usage[probation_low_PO_low_client_usage$supervision_failure == TRUE,])/
  nrow(probation_low_PO_low_client_usage) # 32.3% of 65 observations

nrow(probation_low_PO_typical_client_usage[probation_low_PO_typical_client_usage$supervision_failure == TRUE,])/
  nrow(probation_low_PO_typical_client_usage) # 8.3% of 12 observations

nrow(slc.probation.dtf[slc.probation.dtf$client_msg_count > 0,])/nrow(slc.probation.dtf) # 90.1% of probation clients send at least one msg


### Any evidence of Doleac RCT? 
### Between Feb-May 2017, 330 offenders assigned to treatment (even ID numbers, N = 215) or control (odd ID numbers, N = 115)

# Investigate ID numbers of client_created_at between Feb-May 2017, should be all even.
# Can't use client_created_at because of restart, use min(send_at)

matches_during_RCT <- all_messages %>% group_by(ofndr_num) %>% summarize(first_msg = min(send_at)) %>%
  filter(first_msg > as.POSIXct("2017-02-01", origin = "1970-01-01") &
           first_msg < as.POSIXct("2017-05-30", origin = "1970-01-01")) %>% 
  dplyr::select(ofndr_num) %>% unique()

table(matches_during_RCT %% 2 == 0)
# 93 of the IDs in the CC matches are even, 54 are odd -- about the same proportion of even/odd in Doleac's sample.
# Does not support idea that odd IDs during RCT had no access to CC

# Perhaps odd IDs were signed up for CC but POs didn't use it?
matches_during_RCT$matches_during_RCT <- 1
matched_ofndr_nums <- unique(all_messages[,c("client_id","user_id", "end_dt_num","ofndr_num")])

slc.pretrial.dtf <- merge(slc.pretrial.dtf, matched_ofndr_nums, by = c("client_id","user_id","end_dt_num"), all.x = TRUE, all.y = FALSE)
pretrial_odd_even <- slc.pretrial.dtf %>% mutate(odd_even = case_when(ofndr_num %% 2 == 0 ~ "even",
                                                                      ofndr_num %% 2 != 0 ~ "odd"))

table(pretrial_odd_even$odd_even)

median(pretrial_odd_even$user_msg_count[pretrial_odd_even$odd_even == "even"]) # 4
mean(pretrial_odd_even$user_msg_count[pretrial_odd_even$odd_even == "even"]) # 4.9
median(pretrial_odd_even$user_msg_count[pretrial_odd_even$odd_even == "odd"]) # 3
mean(pretrial_odd_even$user_msg_count[pretrial_odd_even$odd_even == "odd"]) # 4.7

nrow(pretrial_odd_even[pretrial_odd_even$user_msg_count == 0,]) # 0

slc.probation.dtf <- merge(slc.probation.dtf, matched_ofndr_nums, by = c("client_id","user_id","end_dt_num"), all.x = TRUE, all.y = FALSE)
probation_odd_even <- slc.probation.dtf %>% mutate(odd_even = case_when(ofndr_num %% 2 == 0 ~ "even",
                                                                        ofndr_num %% 2 != 0 ~ "odd"))

table(probation_odd_even$odd_even)

median(probation_odd_even$user_msg_count[probation_odd_even$odd_even == "even"]) # 20
mean(probation_odd_even$user_msg_count[probation_odd_even$odd_even == "even"]) # 28.78
median(probation_odd_even$user_msg_count[probation_odd_even$odd_even == "odd"]) # 15.5
mean(probation_odd_even$user_msg_count[probation_odd_even$odd_even == "odd"]) # 22.8

nrow(probation_odd_even[probation_odd_even$user_msg_count == 0,]) # 0

# it really doesn't look like it!


### Investigation of u-shaped curve in probation data

# Are a few POs driving up the failure rate?

table(slc.probation.dtf$user_id, slc.probation.dtf$supervision_failure)

slc.probation.dtf <- slc.probation.dtf %>% group_by(user_id) %>% 
  mutate(fail_rate = (sum(supervision_failure == TRUE))/((sum(supervision_failure == TRUE)) + 
                                                           (sum(supervision_failure == FALSE)))) %>%
  arrange(fail_rate)

mean(slc.probation.dtf$fail_rate) # 17%
fivenum(slc.probation.dtf$fail_rate) # actually, it looks like the fail_rate is being driven DOWN, not up.

# users with 0% fail rate = 4,7,8,15,19,35,55,62,71,99,117,118,124,126
# users with 100% fail rate (in only one relationship) = 13,23,72,127

probation.dtf_w_failures <- slc.probation.dtf %>% group_by(user_id) %>% filter(fail_rate < 1 & fail_rate > 0)
mean(probation.dtf_w_failures$fail_rate) # 22%
fivenum(probation.dtf_w_failures$fail_rate) # 75th %ile = 28%

# users with fail rate in top 25th %ile = 12,14,38,43,59,96,116,125,134,142 -- maybe these caseloads are riskier?
# does the u-curve appear without these users? Try the model with reduced data
# note that there is really very little data here! 174 observations, 11 POs

probation.dtf_lower_risk <- probation.dtf_w_failures %>% filter(!user_id %in% c(12,14,38,43,59,96,116,125,134,142))
probation.dtf_lower_risk$user_id
mean(probation.dtf_lower_risk$fail_rate) # 15%

#taking variable names out to use them in the superlearner model
model_vars <- c("supervision_failure","client_msgs_per_month","user_msgs_per_month","time_on_cc","user_id")

data <- probation.dtf_lower_risk[,(model_vars)]

data <- data[complete.cases(data),]

cv.cntrl <- SuperLearner.CV.control(V = 5L, stratifyCV = FALSE, shuffle = TRUE,
                                    validRows = NULL)

X_train <- data
#outcome data for superlearner needs to be numeric
Y_train <- as.numeric(data$supervision_failure)

#be sure to drop your outcome variable from your training dataset:
X_train$supervision_failure <- NULL

SL.library <- c("SL.xgboost","SL.ranger","SL.ksvm", "SL.glmnet", "SL.speedglm", "SL.cforest")

sl = SuperLearner(Y = Y_train, X = X_train, family = binomial(), method="method.AUC", verbose=TRUE, 
                  SL.library = SL.library, cvControl = cv.cntrl)

sl 


#see what the effect of a hypothetical change in the data might be (here, predicted effect of moving from 0 to 9 messages per month)
pred.1.dtf <- data.frame(matrix(nrow=0,ncol=3))

for (k in c(2.25)) {
  for (i in 0:9) {
    newData <- X_train
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict.SuperLearner(sl, newData, onlySL=TRUE)
    new.row <- c(i, k, mean(test.dtf$pred, na.rm=TRUE))
    pred.1.dtf <- rbind(pred.1.dtf, new.row)
  }
}
colnames(pred.1.dtf) <- c("user_msgs_per_month","client_msgs_per_month","failure_probability")

pred.1.dtf

p1.preds <- ggplot(pred.1.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y =failure_probability, color = "failure_probability"))

p1.preds

# at different levels of client engagement
pred.2.dtf <- data.frame(matrix(nrow=0,ncol=3))

for (k in 0:4) {
  for (i in 0:9) {
    newData <- X_train
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict.SuperLearner(sl, newData, onlySL=TRUE)
    new.row <- c(i, k, mean(test.dtf$pred, na.rm=TRUE))
    pred.2.dtf <- rbind(pred.2.dtf, new.row)
  }
}
colnames(pred.2.dtf) <- c("user_msgs_per_month","client_msgs_per_month","failure_probability")

pred.2.dtf

p2.preds <- ggplot(pred.2.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y =failure_probability, color = "failure_probability"))

p2.preds

# I'm not seeing a u-curve here...
# but still higher PO usage is associated with higher failure rate



### Investigation of high PO msg count

outbound_probation_msgs <- probation_msgs %>% filter(inbound == "False") %>% 
  group_by(client_id, user_id, send_at_month) %>% add_tally()
names(outbound_probation_msgs)[34] <- "monthly_msg_tally"
outbound_probation_msgs$user_id <- as.factor(as.character(outbound_probation_msgs$user_id))
outbound_probation_msgs$client_id <- as.factor(as.character(outbound_probation_msgs$client_id))
outbound_probation_msgs$send_at_month <- as.Date(outbound_probation_msgs$send_at_month)

probation_stats <- outbound_probation_msgs %>%
  group_by(user_id) %>% mutate(med_monthly_tally = median(monthly_msg_tally),
                                             q80 = quantile(monthly_msg_tally, 0.8),
                                              sd = sd(monthly_msg_tally))

View(probation_stats)

probation_stats$med_monthly_tally[probation_stats$user_id == 100]

probation_stats %>% filter(user_id == 100) %>% group_by(client_id, send_at_month) %>% tally() %>% 
  ggplot(aes(x = send_at_month, y = n, color = client_id)) + geom_line() + geom_hline(yintercept = 12) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")
  
probation_stats$med_monthly_tally[probation_stats$user_id == 116]

probation_stats %>% filter(user_id == 116) %>% group_by(client_id, send_at_month) %>% tally() %>% 
  ggplot(aes(x = send_at_month, y = n, color = client_id)) + geom_line() + geom_hline(yintercept = 9) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")



