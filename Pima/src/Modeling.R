library(mgcv)
library(lme4)
library(SuperLearner)

# Naive
pima.tv1 <- glm(violation_i ~ client_msgs_per_month, data=pima.relationships, family=binomial)
summary(pima.tv1)

# Exploring
pima.tv2 <- glm(violation_i ~ (client_msgs_n>0) + log(user_msgs_per_month + 1) + months_on_cc, 
                data=pima.relationships_with_failures[c(pima.relationships_with_failures$user_msgs_per_month<10),], 
                family=binomial)
summary(pima.tv2)

reduced_pima <- subset(pima.relationships_with_failures, client_msgs_per_month<10 & user_msgs_per_month<10)

pima.tv3 <- glm(violation_i ~ (client_msgs_per_month) * (user_msgs_per_month>1) + months_on_cc + as.factor(user_id), 
                data=reduced_pima, 
                family=binomial)
summary(pima.tv3)

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

## Effect size pima.tv3.h ## model makes sense, but predictions are confusing
pima.client_msg_count_25 <- pima.relationships_with_failures
pima.client_msg_count_25$client_msgs_per_month <- .25
pima.client_msg_count_25$user_msgs_per_month <- .5

pima.client_msg_count_2 <- pima.relationships_with_failures
pima.client_msg_count_2$client_msgs_per_month <- 2
pima.client_msg_count_2$user_msgs_per_month <- .5

predictions_client_msg_count_25 <- predict(pima.tv3.h, newdata = pima.client_msg_count_25, type = "response")
predictions_client_msg_count_2 <- predict(pima.tv3.h, newdata = pima.client_msg_count_2, type = "response")

mean(predictions_client_msg_count_25) # client(.25), user(.5) 12.9%
mean(predictions_client_msg_count_25) # client(.25), user(2) 13.5%

mean(predictions_client_msg_count_2) # client(2), user(.5) 1.8%
mean(predictions_client_msg_count_2) # client(2), user(2) 14.3%

## Effect size pima.tv3.h2 ## Yes on predictions, but model is confusing
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

## Machine learning
## Goal = best predictions, not most interpretable model

#start with your logistic regression model

pima.relationships_with_failures$user_id <- as.factor(pima.relationships_with_failures$user_id)

dummy.model <- glm(violation_i ~ client_msgs_per_month + user_msgs_per_month + 
                     months_on_cc + user_id, data = pima.relationships_with_failures,
                   family = binomial)

#see what % of outcomes would be accurately predicted with new data 
# (here I just used the training data, which is a no-no in general)
pred_out <- predict(dummy.model, pima.relationships_with_failures, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(as.factor(as.logical(round(pred_out,0)))==pima.relationships_with_failures$violation_i, na.rm=TRUE)/
  (nrow(pima.relationships_with_failures))

#taking variable names out to use them in the superlearner model
model_vars <- names(dummy.model$model)

data <- pima.relationships_with_failures[,(model_vars)]

data <- data[complete.cases(data),]

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

pima.tv3.h2 <- gam(violation_i ~ log(client_msgs_per_month+1) * poly(user_msgs_per_month,2) +
                     months_on_cc + as.factor(user_id),
                   data=reduced_pima,
                   family=binomial)
summary(pima.tv3.h2)

pred_out <- predict(pima.tv3.h2.reduced, pima.relationships_with_failures, type="response")

#how many predicted outcomes match the actual outcomes divided by the total number of observations
sum(round(pred_out,0)==pima.relationships_with_failures$violation_i, na.rm=TRUE)/(nrow(pima.relationships_with_failures))

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

# Visualization
p1.preds <- ggplot(pred.dtf, aes(x = user_msgs_per_month)) +
  geom_point(aes(y = violation_probability, color = "violation_probability"))



