library(tidyverse)
library(lme4)


# with med_user_msg_length and/or max_scheduled_diff, model barely converges or won't converge
# need to rescale these values if want to include them

model1 <- glmer(supervision_failure ~ (1 | PO) + client_msg_count + user_msg_count + future_appointment_date,
                data = slc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model1) # AIC 656.2


model2 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + user_msg_count + future_appointment_date,
                data = slc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model2) # AIC 652.4


model3 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date,
                data = slc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model3) # AIC 651.9


# any use of message scheduling feature at all
model4 <- glmer(supervision_failure ~ (1 | PO) + (client_msg_count > 0) + future_appointment_date +
                  (max_scheduled_diff > 0),
                data = slc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model4) # AIC 653.1


