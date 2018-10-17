## Anomaly detection
library(tidyverse)
library(anomalize)

temp.a <- unique(probation_stats[,c("client_id","monthly_msg_tally","send_at_month")])
temp.a <- spread(temp.a, client_id, monthly_msg_tally)

write.csv(temp.a, 'temp.a.csv') # by hand, replace NAs within series with 0 and eliminate clients with fewer than 6 months on CC
all_time_series <- read.csv('temp.a.reduced.csv')
all_time_series$send_at_month <- as.Date(as.character(all_time_series$send_at_month), format = "%m/%d/%y")

# create time series

# the tidy way
X102 <- all_time_series %>% dplyr::select(send_at_month, X102) %>% 
  filter(!is.na(X102)) %>% arrange(send_at_month) %>% 
  rename(number_of_msgs = X102) %>% as.tibble() 

X4819 <- all_time_series %>% dplyr::select(send_at_month, X4819) %>% 
  filter(!is.na(X4819)) %>% arrange(send_at_month) %>% 
  rename(number_of_msgs = X4819) %>% as.tibble() 

# old school (necessary for the for loop)
X104 <- all_time_series[,c(2,4)] 
colnames(X104)[2] <- "number_of_msgs"
X104 <- X104[!is.na(X104$number_of_msgs),]
X104 <- X104[order(X104$send_at_month),]
X104 <- as.tibble(X104)



# collect time series in a list
ts_list <- list(X102)
ts_list

ts_list[[2]] <- X104
ts_list

ts_list[[3]] <- X4819
ts_list

# automate!

q <- ncol(all_time_series) - 2

for (i in 1:q) {
  time_series <- all_time_series[,c(2,(i+2))]
  colnames(time_series)[2] <- "number_of_msgs"
  time_series <- time_series[!is.na(time_series$number_of_msgs),]
  time_series <- time_series[order(time_series$send_at_month),]
  time_series <- as.tibble(time_series)
  ts_list[[i]] <- time_series
}

time_series_names <- colnames(all_time_series[,-c(1,2)])
names(ts_list) <- time_series_names

# anomalize

# plot that shows anomalies
X102 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
X104 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
X4819 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

# data pts identified as anomalies (tidy)
anomalies <- X102 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>% filter(anomaly == 'Yes')
anomalies <- X104 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>% filter(anomaly == 'Yes')
anomalies <- X4819 %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>% filter(anomaly == 'Yes')

# loop each item in the list through time_decompose() and extract anomalous dates

anomalies_list <- data.frame(matrix(nrow = 500, ncol = 11))

# practice
anomalies <- ts_list[[185]] %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>% filter(anomaly == 'Yes')
x <- nrow(anomalies)
y <- min(which(is.na(anomalies_list[[1]])))
anomalies_list[y:(y+x-1), 1] <- 185
anomalies_list[y:(y+x-1), 2:11] <- anomalies

# do it!

for (i in 1:q){
  tryCatch({
    anomalies <- ts_list[[i]] %>% time_decompose(number_of_msgs) %>% anomalize(remainder) %>% time_recompose() %>% filter(anomaly == 'Yes')
    x <- nrow(anomalies)
    if(x == 0) next
    y <- min(which(is.na(anomalies_list[[1]])))
    anomalies_list[y:(y+x-1), 1] <- i
    anomalies_list[y:(y+x-1), 2:11] <- anomalies
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Well, anomalize() seems to be overkill
# Each time series too small; seasonality and trend doesn't make sense

# Next idea: write a function that takes the mean of number of monthly msgs for observations preceding this observation;
# anomalies are when this month's number of msgs is twice the mean or more
# Then loop ts_list through this function

processed_list <- list()

for(j in 1:length(ts_list)){
  x <- ts_list[[j]]
  x$anomaly <- 0
  for(i in 1:nrow(x)){
    if(i == 1) next
    mean_msgs <- mean(x$number_of_msgs[1:i-1])
    if (x$number_of_msgs[i] >= (2 * mean_msgs)) {x$anomaly[i] <- 1} 
    else {x$anomaly[i] <- 0}
  }
  processed_list <- list(processed_list, x)
}

# Why are the names of items in processed_list so weird? Can they inherit the names from ts_list?
names(processed_list) # returns NULL
# item_name <- names(ts_list[j]) (add to first loop)?
# What are there only 2 items in processed_list
length(processed_list)

# Ah, the problem is how the new items are being appended!

# function to append new items to list
append_list <- function(existinglist, itemtoadd){
  existinglist[[length(existinglist) + 1]] <- itemtoadd
  return(existinglist)
}

# Also, the items in ts_list do not have to be tibbles (since I'm not using anomalize)
# Recreate ts_list with list items as df instead of tibbles.

ts_list <- list()

q <- ncol(all_time_series) - 2

for (i in 1:q) {
  time_series <- all_time_series[,c(2,(i+2))]
  colnames(time_series)[2] <- "number_of_msgs"
  time_series <- time_series[!is.na(time_series$number_of_msgs),]
  time_series <- time_series[order(time_series$send_at_month),]
  ts_list[[i]] <- time_series
}

time_series_names <- colnames(all_time_series[,-c(1,2)])
names(ts_list) <- time_series_names

# Now loop ts_list through process, but with new function for appending the resulting list

processed_list <- list()

for(j in 1:length(ts_list)){
  x <- ts_list[[j]]
  x$anomaly <- 0
  for(i in 1:nrow(x)){
    if(i == 1) next
    mean_msgs <- mean(x$number_of_msgs[1:i-1])
    if (x$number_of_msgs[i] >= (2 * mean_msgs)) {x$anomaly[i] <- 1} 
    else {x$anomaly[i] <- 0}
  }
  processed_list <- append_list(processed_list, x)
}

# update names of objects in new list
ts_list_names <- names(ts_list)
names(processed_list) <- ts_list_names

# check
processed_list[["X102"]]
X102_msgs <- probation_msgs %>% filter(client_id == 102) %>% arrange(send_at) %>%
  select(body, inbound, send_at, supervision_failure)

# export text conversations to study
anonymized_probation_msgs <- probation_msgs %>% arrange(client_id, send_at) %>%
  select(client_id, user_id, body, inbound, send_at, supervision_failure)

ts_list_names <- gsub("X", "", ts_list_names)
ts_list_names <- as.integer(ts_list_names)

# only the ones that are included in processed_list
anonymized_probation_msgs <- anonymized_probation_msgs[anonymized_probation_msgs$client_id %in% ts_list_names,]

write.csv(anonymized_probation_msgs, 'anonymized_probation_msgs.csv')




