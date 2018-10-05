library(tidyverse)
library(lme4)

# REPORT 1 model
# user_msg_count, client_msg_count > 0, med_user_msg_length,
# max_polarity, median_subjectivity, n_report_time,
# uc_max_response_time_hr, median_schedule_time_hr > 10

# We no longer have max_polarity, median_subjectivity, or max_response_time variables.
# n_report_time is now future_appointment_date.
# There is no median_schedule_time_hr, but there is max_scheduled_diff.


# REPORT 2

model2 <- glmer(supervision_failure ~ (1 | PO) +
                  (client_msg_count > 0) + (max_scheduled_diff > 72),
                data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model2)
# AIC 1067.8, client_msg_count sig, max_scheduled_diff not

model2.log <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model2.log)
# AIC 1069.9, client_msg_count sig, max_scheduled_diff not


# REPORT 3

model3 <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0 & client_msg_count < 3)  +
                 (client_msg_count > 2),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model3)
# 1069.8, both variables sig


# more explore (for Manya 10/2)

modela <- glmer(supervision_failure ~ (1 | PO) +
                  (client_msg_count > 0)  + future_appointment_date,
                data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modela)

modelb <- glmer(supervision_failure ~ (1 | PO) +
                  (client_msg_count > 0)  + (max_scheduled_diff > 0) + future_appointment_date,
                data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelb)

modelc <- glmer(supervision_failure ~ (1 | PO) + user_msg_count +
                  (client_msg_count > 0)  + future_appointment_date,
                data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelc)


# REPORT 4

initial_msgs_qualities <- user_msgs_qualities[user_msgs_qualities$initial_msg_indicator == 1,] # filter for initial messages

model4 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
                         pls_respond + business + max_reuse_score + info + problem + urgency + polite +
                         greeting + yelling + has_client_name + has_user_name + polite:max_reuse_score +
                         send_at_ToD_bins, 
                       data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(model4)
# variables direction and sig all the same as before except Afternoon not sig

write.csv(initial_msgs_qualities, "initial_msgs_qualities.csv")
