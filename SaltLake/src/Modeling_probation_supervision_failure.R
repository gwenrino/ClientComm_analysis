library(mgcv)
library(lme4)
library(SuperLearner)

names(slc.probation.dtf)

## Naive

model.1 <- glm(supervision_failure ~ client_msgs_per_month, data=slc.probation.dtf, family=binomial)
summary(model.1) # AIC: 270.66



## Logistic regression

model.2 <- glm(supervision_failure ~ (client_msg_count>0) + log(user_msgs_per_month + 1) + time_on_cc, 
                data=slc.probation.dtf, family=binomial)
summary(model.2) # AIC: 268.24

model.3 <- glmer(supervision_failure ~ client_msgs_per_month + user_msgs_per_month + 
                      time_on_cc + (1 | user_id), 
                    data=slc.probation.dtf, control = glmerControl(optimizer = "bobyqa"), 
                    family=binomial)
summary(model.3) # AIC: 264.7

model.4 <- glmer(supervision_failure ~ client_msgs_per_month + user_msgs_per_month + 
                   (1 | user_id), 
                 data=slc.probation.dtf, control = glmerControl(optimizer = "bobyqa"), 
                 family=binomial)
summary(model.4) # AIC: 266.9

model.5 <- glmer(supervision_failure ~ client_msgs_per_month + ((user_msgs_per_month<4) & (user_msgs_per_month>2)) +
                   (user_msgs_per_month>5) + (user_msgs_per_month<1) + (1 | user_id), 
                 data=slc.probation.dtf, control = glmerControl(optimizer = "bobyqa"), 
                 family=binomial)
summary(model.5) # AIC: 272.0



## Survival model

library(survival)
library(survminer)

slc.probation.dtf$supervision_failure <- as.numeric(slc.probation.dtf$supervision_failure == TRUE)

slc.cox <- coxph(Surv(time_on_cc, supervision_failure) ~ client_msgs_per_month, data =  slc.probation.dtf)

slc.cox <- coxph(Surv(time_on_cc, supervision_failure) ~ client_msgs_per_month + 
                   ((user_msgs_per_month<4) & (user_msgs_per_month>2)) + (user_msgs_per_month > 5), 
                 data = slc.probation.dtf)

summary(slc.cox)

ggsurvplot(survfit(slc.cox, data=slc.probation.dtf), palette = "#2E9FDF",ggtheme = theme_minimal())




## Machine learning
## Goal = best predictions, not most interpretable model

#start with your logistic regression model

slc.probation.dtf$user_id <- as.factor(as.character(slc.probation.dtf$user_id))

dummy.model <- glmer(supervision_failure ~ client_msgs_per_month + user_msgs_per_month + 
                       time_on_cc + (1 | user_id), 
                     data=slc.probation.dtf, control = glmerControl(optimizer = "bobyqa"), 
                     family=binomial)

#see what % of outcomes would be accurately predicted with new data 
# (here I just used the training data, which is a no-no in general)
pred_out <- predict(dummy.model, slc.probation.dtf, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(as.factor(as.logical(round(pred_out,0)))==as.factor(as.logical(slc.probation.dtf$supervision_failure)))/
  (nrow(slc.probation.dtf)) # 0.83

#taking variable names out to use them in the superlearner model
model_vars <- c("supervision_failure","client_msgs_per_month","user_msgs_per_month","time_on_cc","user_id")

data <- slc.probation.dtf[,(model_vars)]

data <- data[complete.cases(data),]

cv.cntrl <- SuperLearner.CV.control(V = 5L, stratifyCV = FALSE, shuffle = TRUE,
                                    validRows = NULL)

X_train <- data
#outcome data for superlearner needs to be numeric
Y_train <- data$supervision_failure

#be sure to drop your outcome variable from your training dataset:
X_train$supervision_failure <- NULL

SL.library <- c("SL.xgboost","SL.ranger","SL.ksvm", "SL.glmnet", "SL.speedglm", "SL.cforest")

sl = SuperLearner(Y = Y_train, X = X_train, family = binomial(), method="method.AUC", verbose=TRUE, 
                  SL.library = SL.library, cvControl = cv.cntrl)

sl
#how accurate are the superlearner predictions (what's the improvement over the logistic regression with the same variables?)
sum(round(sl$SL.predict,0)==Y_train)/length(Y_train) # 0.88

#be mindful that superlearner needs variables that were numeric in the model to be numeric in the prediction dataset, integer as integer, etc.

#create empty data frame to store results
pred.1.dtf <- data.frame(matrix(nrow=0,ncol=3))

#see what the effect of a hypothetical change in the data might be (here, predicted effect of moving from 0 to 5 messages per month)
for (k in 0:5) {
  for (i in 0:5) {
    newData <- X_train
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict.SuperLearner(sl, newData, onlySL=TRUE)
    new.row <- c(i, k, mean(test.dtf$pred, na.rm=TRUE))
    pred.1.dtf <- rbind(pred.1.dtf, new.row)
  }
}
colnames(pred.1.dtf) <- c("user_msgs_per_month","client_msgs_per_month","failure_probability")

pred.1a.dtf <- data.frame(matrix(nrow=0,ncol=3))

#finer gradation?
for (k in seq(0,5,1)) {
  for (i in seq(0,5,.1)) {
    newData <- X_train
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict.SuperLearner(sl, newData, onlySL=TRUE)
    new.row <- c(i, k, mean(test.dtf$pred, na.rm=TRUE))
    pred.1a.dtf <- rbind(pred.1a.dtf, new.row)
  }
}
colnames(pred.1a.dtf) <- c("user_msgs_per_month","client_msgs_per_month","failure_probability")

pred.2.dtf <- data.frame(matrix(nrow=0,ncol=3))

#predict using regression instead
for (k in seq(.08,1.7,.67)) {
  for (i in seq(.08,4,.01)) {
    newData <- slc.probation.dtf
    newData$user_msgs_per_month <- as.numeric(i)
    newData$client_msgs_per_month <- as.numeric(k)
    test.dtf <- predict(model.3, newData, type="response")
    new.row <- c(i, k, mean(test.dtf, na.rm=TRUE))
    pred.2.dtf <- rbind(pred.2.dtf, new.row)
  }
}
colnames(pred.2.dtf) <- c("user_msgs_per_month","client_msgs_per_month","failure_probability")


# Visualization
p1.preds <- ggplot(pred.1.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y =failure_probability, color = "failure_probability"))

p1.preds

p1a.preds <- ggplot(pred.1a.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y =failure_probability, color = "failure_probability"))

p1a.preds

p2.preds <- ggplot(pred.2.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y = failure_probability, color = "failure_probability"))

p2.preds
