library(tidyverse)

# Rate of client responses each user gets?
user_response_rate <- cc.dtf %>% group_by(user_id) %>% 
  count(client_msg_count == 0) %>% spread('client_msg_count == 0', n)

names(user_response_rate)[names(user_response_rate) == 'FALSE'] <- 'response'
names(user_response_rate)[names(user_response_rate) == 'TRUE'] <- 'no_response'

user_response_rate <- user_response_rate %>% mutate(response_rate = response/(response+no_response))


# Are users using mass msg to send initial msgs?
generic_initial_msgs <- initial_msgs_qualities[initial_msgs_qualities$max_reuse_score == 1,]

mass_initial_msgs <- generic_initial_msgs %>% add_count(send_at_num) %>% filter(n != 1)

mass_initial_msgs[mass_initial_msgs$user_id ==10,]
nrow(initial_msgs_qualities[initial_msgs_qualities$user_id == 10,])
