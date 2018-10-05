library(tidyverse)

## Investigation of u-shaped curve in probation data

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

# I'm not seeing a u-curve here.

#####

# Failure rates

nrow(CC_pretrial_outcomes[CC_pretrial_outcomes$supervision_failure == TRUE,])/nrow(CC_pretrial_outcomes) # 32.4% of 259 outcomes
nrow(non_CC_pretrial_outcomes[non_CC_pretrial_outcomes$supervision_failure == TRUE,])/nrow(non_CC_pretrial_outcomes) # 37.5% of 3234 outcomes

nrow(CC_probation_outcomes[CC_probation_outcomes$supervision_failure == TRUE,])/nrow(CC_probation_outcomes) # 28.0% of 132 outcomes
nrow(non_CC_probation_outcomes[non_CC_probation_outcomes$supervision_failure == TRUE,])/nrow(non_CC_probation_outcomes) # 26.8% of 913 outcomes





