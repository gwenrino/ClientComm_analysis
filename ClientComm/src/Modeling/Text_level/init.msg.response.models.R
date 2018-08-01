Sys.setenv(TZ='America/New_York')

######################################################
## Qualities of initial messages that get responses ##
######################################################

initial_msgs_qualities <- user_msgs_qualities[user_msgs_qualities$initial_msg_indicator == 1,] # filter for initial messages

### All initial messages: what stylistic characteristics are significantly associated with responses?

init_msg_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score, 
                        data = initial_msgs_qualities, family = 'binomial')
summary(init_msg_mod.1)
# sig pos: greeting, user name
# pos: client name
# sig neg: polite, hi reuse score
# neg: closing, yelling
# AIC 3710.0

init_msg_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score +
                          polite:max_reuse_score, # interaction of polite and max_reuse
                        data = initial_msgs_qualities, family = 'binomial')
summary(init_msg_mod.2)
# sig pos: greeting, user name
# pos: client name, polite
# sig neg: hi reuse score, polite:reuse
# neg: closing, yelling
# AIC 3704.4

init_msg_mod.3 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + polite:max_reuse_score +
                          (max_reuse_score < 0.8), # more personalized
                        data = initial_msgs_qualities, family = 'binomial')
summary(init_msg_mod.3)
# sig pos: greeting, polite, user name, reuse score < 0.8
# pos: client name
# sig neg: polite:reuse
# neg: closing, yelling
# AIC 3704.6 
#### this one makes sense to me

init_msg_mod.4 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + polite:max_reuse_score +
                          (max_reuse_score < 0.8) + closing:max_reuse_score, # interaction of closing and max_reuse
                        data = initial_msgs_qualities, family = 'binomial')
# won't converge



### All initial messages: what content areas are significantly associated with reponses?

init_content_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                          info + urgency, 
                        data = initial_msgs_qualities, family = 'binomial')
summary(init_content_mod.1)
# sig pos: future appt date
# pos: business
# sig neg: problem, info
# neg: urgency
# AIC 3758.9

init_content_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + max_reuse_score:business, # interaction of business and max_reuse
                            data = initial_msgs_qualities, family = 'binomial')
summary(init_content_mod.2)
# sig pos: future appt date, business
# pos: 
# sig neg: problem, urgency, reuse score, business:reuse score
# neg: info
# AIC 3729.2 
#### this one makes sense to me

init_content_mod.3 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + max_reuse_score:business + max_reuse_score:problem, # interaction of problem and max_reuse
                            data = initial_msgs_qualities, family = 'binomial')
summary(init_content_mod.3)
# sig pos: future appt date, business
# pos: problem:reuse score
# sig neg: urgency, reuse score, business:reuse score
# neg: info, problem
# AIC 3730.2



### Initial msgs with future appt dates: what stylistic characteristics are significant?

initial_msgs_w_dates <- initial_msgs_qualities[initial_msgs_qualities$future_appointment_date == 1,]

init_date_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score, 
                        data = initial_msgs_w_dates, family = 'binomial')
summary(init_date_mod.1)
# sig pos: greeting, user name
# pos:
# sig neg:
# neg: closing, polite, yelling, client name, reuse score
# AIC 1818.1 

init_date_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score +
                          polite:max_reuse_score, # interaction of polite and max_reuse
                        data = initial_msgs_w_dates, family = 'binomial')
# doesn't converge

init_date_mod.3 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + 
                          (max_reuse_score < 0.8), # more personalized
                        data = initial_msgs_w_dates, family = 'binomial')
summary(init_date_mod.3)
# sig pos: greeting, user name, reuse score < 0.8
# pos:
# sig neg:
# neg: closing, polite, yelling, client name
# AIC 1816.2

