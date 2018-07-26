
# Rate of client responses each user gets?
user_response_rate <- cc.dtf %>% group_by(user_id) %>% 
  count(client_msg_count == 0) %>% spread('client_msg_count == 0', n)

names(user_response_rate)[names(user_response_rate) == 'FALSE'] <- 'response'
names(user_response_rate)[names(user_response_rate) == 'TRUE'] <- 'no_response'

user_response_rate <- user_response_rate %>% mutate(response_rate = response/(response+no_response))

# Rate of client failures each user gets?
user_success_rate <- cc.dtf %>% group_by(user_id) %>% 
  count(supervision_failure) %>% spread(supervision_failure, n)

names(user_success_rate)[names(user_success_rate) == 'FALSE'] <- 'success'
names(user_success_rate)[names(user_success_rate) == 'TRUE'] <- 'failure'

user_success_rate <- user_success_rate %>% mutate(success_rate = success/(success+failure))

# Merge
user_rates <- merge(user_response_rate, user_success_rate, by="user_id")

# Top 5 response rates: 31, 15, 14, 11, 29
# Top 5 success rates: 19, 9, 11, 12, 23 
# Bottom 5 response rates: 19, 21, 26, 16, 23
# Bottom 5 success rates: 17, 13, 18, 16, 25

# Top response/top success = 11
# Bottom response/bottom success = 16
# ?? Bottom response/top success = 19, 23
# No top responses among the bottom successes



