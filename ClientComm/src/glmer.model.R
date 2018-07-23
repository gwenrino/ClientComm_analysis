## BEST GLMER MODEL from Modeling2

mod.1 <- glmer(supervision_failure ~ (1 | PO) +
                     (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1),
                   data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.1) # AIC 945.0

####################################################################################
mod.2 <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0 & client_msg_count < 3)  +
                 (client_msg_count > 2),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.2) # AIC 944.7
####################################################################################

mod.3 <- glmer(supervision_failure ~ (1 | PO) +
                 client_msg_count,
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.3) # AIC 946.7

mod.4 <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0 & client_msg_count < 3) +
                 (client_msg_count > 2) + log(as.numeric(max_scheduled_diff) + 1),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.4) # AIC 946.7

mod.5 <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0 & client_msg_count < 3) +
                 (client_msg_count > 2) + (future_appointment_date > 2),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.5) # AIC 946.6

## Check model on test set

# Create training and test sets
set.seed(4646)
train_set <- cc.dtf %>% sample_frac(0.75)
test_set <- anti_join(cc.dtf, train_set)

nrow(train_set)
nrow(test_set)

nrow(test_set[test_set$supervision_failure == TRUE,]) #37 failures out of 436 relationships

# Fit the model to training set
glmer.model <- glmer(supervision_failure ~ (1 | PO) +
                       (client_msg_count > 0 & client_msg_count < 3)  +
                       (client_msg_count > 2),
                     data = train_set, family = binomial, control = glmerControl(optimizer = "bobyqa"))
# Get predictions on test set
pred.glmer.model <- predict(glmer.model, newdata = test_set, type = "response")

# Confusion matrix
table(test_set$supervision_failure, pred.glmer.model > 0.1)

## Effect size client_msg_count

cc.dtf_msg_count_0 <- cc.dtf
cc.dtf_msg_count_0$client_msg_count <- 0

cc.dtf_msg_count_1.5 <- cc.dtf
cc.dtf_msg_count_1.5$client_msg_count <- 1.5

cc.dtf_msg_count_3 <- cc.dtf
cc.dtf_msg_count_3$client_msg_count <- 3


predictions_msg_count_0 <- predict(mod.2, newdata = cc.dtf_msg_count_0, type = "response")
predictions_msg_count_1.5 <- predict(mod.2, newdata = cc.dtf_msg_count_1.5, type = "response")
predictions_msg_count_3 <- predict(mod.2, newdata = cc.dtf_msg_count_3, type = "response")

mean(predictions_msg_count_0) # 0.099
mean(predictions_msg_count_1.5) # 0.065
mean(predictions_msg_count_3) # 0.054
# chances of failure decrease from 9.9% to 6.5% (34%) to 5.4% (45%)

### Nothing below this point pans out

## Add DoW_messages, one day at a time

mod.Mon <- glmer(supervision_failure ~ (1 | PO) +
                      (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                      Mon_messages,
                    data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Mon) # AIC 947.0, right direction but not sig


mod.Tue <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Tue_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Tue) # AIC 942.4, sig in wrong direction

mod.Wed <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Wed_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Wed) # AIC 946.5, right direction but not sig

mod.Thu <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Thu_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Thu) # AIC 946.7, right direction but not sig

mod.Fri <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Fri_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Fri) # AIC 947.0, right direction but not sig

mod.Sat <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Sat_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Sat) # AIC 947.0, right direction but not sig

mod.Sun <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Sun_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Sun) # AIC 946.7, wrong direction not sig

mod.Wkd <- glmer(supervision_failure ~ (1 | PO) +
                   (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                   Wkd_messages,
                 data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.Wkd) # AIC 946.8, wrong direction not sig

## Additive DoW 

mod.MT <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Mon_messages + Tue_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MT) # AIC 944.0 Tue sig in wrong direction

mod.MTW <- glmer(supervision_failure ~ (1 | PO) +
                       (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                       Mon_messages + Tue_messages + Wed_messages,
                     data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTW) # AIC 945.5 Tue sig in wrong direction

mod.MTWT <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Mon_messages + Tue_messages + Wed_messages + Thu_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTWT) # AIC 947.4 Tue sig in wrong direction

mod.MTWTF <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Mon_messages + Tue_messages + Wed_messages + Thu_messages + Fri_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTWTF) # AIC 949.4 Tue sig in wrong direction

mod.MTWTFS <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Mon_messages + Tue_messages + Wed_messages + Thu_messages + Fri_messages + Sat_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTWTFS) # AIC 951.3 Tue sig in wrong direction

mod.MTWTFSS <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Mon_messages + Tue_messages + Wed_messages + Thu_messages + Fri_messages + Sat_messages + Sun_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTWTFSS) # AIC 951.9 Tue sig in wrong direction (Sun also wrong direction)

mod.MTWTSS <- glmer(supervision_failure ~ (1 | PO) +
                            (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                            Mon_messages + Tue_messages + Wed_messages + Thu_messages + Sat_messages + Sun_messages,
                          data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTWTSS) # 949.9 Tue sig in wrong direction (Sun also wrong direction)

mod.MTTFSS <- glmer(supervision_failure ~ (1 | PO) +
                            (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                            Mon_messages + Tue_messages + Thu_messages + Fri_messages + Sat_messages + Sun_messages,
                          data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTTFSS) # 951.0 Tue sig in wrong direction (Sun also wrong direction)

mod.MTTFWkd <- glmer(supervision_failure ~ (1 | PO) +
                           (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                           Mon_messages + Tue_messages + Thu_messages + Fri_messages + Wkd_messages,
                         data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.MTTFWkd) # 949.1 Tue sig in wrong direction (Wkd also wrong direction)

# ToD one at a time

mod.morning <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                   Morning_messages,
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.morning) # AIC 946.9

mod.afternoon <- glmer(supervision_failure ~ (1 | PO) +
                       (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                       Afternoon_messages,
                     data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.afternoon) # AIC 946.7

mod.evening <- glmer(supervision_failure ~ (1 | PO) +
                       (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                       Evening_messages,
                     data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.evening) # AIC 946.4 wrong direction

mod.night <- glmer(supervision_failure ~ (1 | PO) +
                     (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                     Night_messages,
                   data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.night) # AIC 946.5

# ToD additive

mod.daytime <- glmer(supervision_failure ~ (1 | PO) +
                       (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                       Morning_messages + Afternoon_messages,
                     data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.daytime) # AIC 948.1

mod.notnight <- glmer(supervision_failure ~ (1 | PO) +
                        (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                        Morning_messages + Afternoon_messages + Evening_messages,
                      data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.notnight) # AIC 949.4

mod.try <- glmer(supervision_failure ~ (1 | PO) +
                   (client_msg_count > 0) + log(as.numeric(max_scheduled_diff) + 1) +
                   Night_messages + Afternoon_messages + Evening_messages +
                   Mon_messages + Tue_messages + Thu_messages + Fri_messages + Wkd_messages,
                 data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.try) # AIC 951.3




