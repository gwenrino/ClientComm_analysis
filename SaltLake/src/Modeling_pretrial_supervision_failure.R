library(tidyverse)
library(lme4)


nrow(slc.pretrial.dtf[slc.pretrial.dtf$supervision_failure == TRUE,])

# With med_user_msg_length and/or max_scheduled_diff, model barely converges or won't converge
# Need to rescale these values if want to include them

model1 <- glmer(supervision_failure ~ (1 | PO) + client_msg_count + user_msg_count + future_appointment_date,
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model1) # AIC 668


model2 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + user_msg_count + future_appointment_date,
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model2) # AIC 664.5


## Ta da ##
model3 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date,
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model3) # AIC 664.4
##  #######

# any use of message scheduling feature at all
model4 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date +
                  (max_scheduled_diff > 0),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model4) # AIC 665.7

model5 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0 & client_msg_count < 3) + 
                  (client_msg_count >= 3) + future_appointment_date,
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model5) # AIC 665.4

nrow(slc.pretrial.dtf[(slc.pretrial.dtf$client_msg_count == 0),]) # 233
nrow(slc.pretrial.dtf[(slc.pretrial.dtf$client_msg_count > 0) & (slc.pretrial.dtf$client_msg_count < 3),]) # 137
nrow(slc.pretrial.dtf[(slc.pretrial.dtf$client_msg_count >= 3),]) # 121

model6 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0 & client_msg_count < 3) + 
                  (client_msg_count >= 3 & client_msg_count < 10),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model6) # AIC 668.7


model7 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model7) # AIC 667.5

model8 <- glmer(supervision_failure ~ (1 | PO) + log(client_msg_count + 1),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model8) # AIC 670.8

model9 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + (future_appointment_date > 0),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model9) # AIC 666.4

# any use of message scheduling feature at all
model10 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + 
                  (max_scheduled_diff > 0),
                data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model10) # AIC 667.7

# ToD
model11 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date +
                   Morning_messages + Afternoon_messages + Evening_messages + Night_messages,
                 data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model11) # AIC 

# DoW
model12 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date +
                   Sun_messages + Mon_messages + Tue_messages + Wed_messages + Thu_messages + Fri_messages + Sat_messages,
                 data = slc.pretrial.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model12) # AIC 



## Effect size client_msg_count

slc.pretrial.dtf_msg_count_0 <- slc.pretrial.dtf
slc.pretrial.dtf_msg_count_0$client_msg_count <- 0

slc.pretrial.dtf_msg_count_2 <- slc.pretrial.dtf
slc.pretrial.dtf_msg_count_2$client_msg_count <- 2


predictions_msg_count_0 <- predict(model3, newdata = slc.pretrial.dtf_msg_count_0, type = "response")
predictions_msg_count_2 <- predict(model3, newdata = slc.pretrial.dtf_msg_count_2, type = "response")

mean(predictions_msg_count_0) # 0.477
mean(predictions_msg_count_2) # 0.368
# chances of failure decrease from 47.7% to 36.8% (a 22.9% decrease)


## Effect size future_appointment_date

slc.pretrial.dtf_future_appt_dt_0 <- slc.pretrial.dtf
slc.pretrial.dtf_future_appt_dt_0$future_appointment_date <- 0

slc.pretrial.dtf_future_appt_dt_1 <- slc.pretrial.dtf
slc.pretrial.dtf_future_appt_dt_1$future_appointment_date <- 1

slc.pretrial.dtf_future_appt_dt_2 <- slc.pretrial.dtf
slc.pretrial.dtf_future_appt_dt_2$future_appointment_date <- 2

slc.pretrial.dtf_future_appt_dt_5 <- slc.pretrial.dtf
slc.pretrial.dtf_future_appt_dt_5$future_appointment_date <- 5

predictions_future_appt_dt_0 <- predict(model3, newdata = slc.pretrial.dtf_future_appt_dt_0, type = "response")
predictions_future_appt_dt_1 <- predict(model3, newdata = slc.pretrial.dtf_future_appt_dt_1, type = "response")
predictions_future_appt_dt_2 <- predict(model3, newdata = slc.pretrial.dtf_future_appt_dt_2, type = "response")
predictions_future_appt_dt_5 <- predict(model3, newdata = slc.pretrial.dtf_future_appt_dt_5, type = "response")

mean(predictions_future_appt_dt_0) # .465
mean(predictions_future_appt_dt_1) # .445
mean(predictions_future_appt_dt_2) # .424
mean(predictions_future_appt_dt_5) # .365
# chances of failure decrease from 46.5% to 36.5% (a 21.5% decrease)

