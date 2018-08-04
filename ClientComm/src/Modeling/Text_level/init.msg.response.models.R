Sys.setenv(TZ='America/New_York')

######################################################
## Qualities of initial messages that get responses ##
######################################################

initial_msgs_qualities <- user_msgs_qualities[user_msgs_qualities$initial_msg_indicator == 1,] # filter for initial messages

### All initial messages: what stylistic characteristics are significantly associated with responses?

init_style_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score, 
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_style_mod.1)
# sig pos: greeting, user name
# pos: client name
# sig neg: polite, hi reuse score
# neg: closing, yelling
# AIC 3715.7

init_style_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + max_reuse_score +
                          polite:max_reuse_score, # interaction of polite and max_reuse
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_style_mod.2)
# sig pos: greeting, user name
# pos: client name, polite
# sig neg: hi reuse score, polite:reuse
# neg: closing, yelling
# AIC 3710.4

init_style_mod.4 <- glmer(msg_replied_i ~ (1 | PO) + greeting + closing + polite + 
                          yelling + has_client_name + has_user_name + polite:max_reuse_score +
                          max_reuse_score + closing:max_reuse_score, # interaction of closing and max_reuse
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_style_mod.4)
# sig pos: greeting, user name
# pos: client name, polite, closing
# sig neg: polite:reuse
# neg: closing:reuse, yelling, reuse score
# AIC 3710.8



### All initial messages: what content areas are significantly associated with reponses?

init_content_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                          info + urgency, 
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.1)
# sig pos: future appt date
# pos: business
# sig neg: problem, info
# neg: urgency
# AIC 3756.5

init_content_mod.1a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + info + urgency, 
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.1a)
# sig pos: appointment_date
# pos: info, pls_respond
# sig neg: 
# neg: urgency, court_date, business, problem
# AIC 1827.4! Much more efficient model, but surprises about direction of influence of variable

init_content_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + max_reuse_score:business, # interaction of business and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.2)
# sig pos: future appt date, business
# pos: 
# sig neg: problem, urgency, reuse score, business:reuse score
# neg: info
# AIC 3733.6
#### this one makes sense to me

init_content_mod.2a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond +
                               business + problem + info + urgency + max_reuse_score + 
                               max_reuse_score:business, # interaction of business and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.2a)
# sig pos: appointment_date
# pos: business, info, pls_respond
# sig neg: problem
# neg: court_date, urgency, reuse score, business:reuse score
# AIC 1828.7

init_content_mod.3 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + max_reuse_score:business + max_reuse_score:problem, # interaction of problem and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.3)
# sig pos: future appt date
# pos: problem:reuse score, business
# sig neg: urgency, reuse score
# neg: info, problem, business:reuse score
# AIC 3734.7

init_content_mod.3a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond +
                               business + problem + info + urgency + max_reuse_score + 
                               max_reuse_score:business + max_reuse_score:problem, # interaction of problem and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.3a)
# sig pos: appt date
# pos: business, info, problem:reuse, pls_respond
# sig neg: 
# neg: court date, problem, urgency, reuse score, bus:reuse
# AIC 1828.4

init_content_mod.4 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + future_appointment_date:business, # interaction of date and business
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.4)
# sig pos: future appt date, business
# pos: 
# sig neg: urgency, problem, reuse score, future_appointment_date:business
# neg: info
# AIC 3724.5
#### also study this one!

init_content_mod.5 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + future_appointment_date:business + max_reuse_score:business, 
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.5)
# sig pos: future appt date
# pos: business
# sig neg: urgency, problem, reuse score, future_appointment_date:business
# neg: info, business:max_reuse_score
# AIC 3726.4
#### also study this one!

init_content_mod.6a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + info + urgency + max_reuse_score + 
                               max_reuse_score:business + max_reuse_score:court_date_reminder + 
                               max_reuse_score:appointment_date_reminder + max_reuse_score:problem, # interactions with max_reuse
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.6a)
# nothing significant
# AIC 1826.7

init_content_mod.7a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + urgency + max_reuse_score + 
                               max_reuse_score:business + max_reuse_score:court_date_reminder, 
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.7a)
# sig pos: appt date, court date
# pos: business, pls_respond, reuse_score
# sig neg: problem, court date:reuse
# neg: urgency, business:reuse
# AIC 1823.0
### this one has low AIC and makes sense except reuse_score is positive?

init_content_mod.8a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + urgency + max_reuse_score + info +
                               max_reuse_score:business + max_reuse_score:court_date_reminder, 
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.8a)

# Confusing! Try to track this effect down.

# 1) with future_appointment_date, all makes sense
w1 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + 
             pls_respond + business + max_reuse_score + info + problem + urgency, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w1)

# 2) replace future_appointment_date with court_date_reminder and appointment_date_reminder
#    +/- sign on court_date_reminder and business and info are not intuitive
w2 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w2)

# adding interactive terms with max_reuse_score
# 3a) with court_date_reminder
#     +/- sign on business, max_reuse_score, info not intuitive
w3 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:court_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w3)

# 3b) with appointment_date_reminder
#     +/- sign on court_date, appointment_date, business, info not intuitive
w4 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:appointment_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w4)

# 3c) with business
#     +/- sign on court_date, info not intuitive
w5 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:business, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w5)

# 3d) with info
#     looks like info and max_reuse_score are too correlated
w6 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:info, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))

# 3e) drop info
#     +/- sign on court_date_reminder and business not intuitive (see w2)
w7 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + problem + urgency, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w7)

# 3f) with court_date_reminder and business
#     +/- sign on max_reuse_score and info not intuitive
w8 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:business + max_reuse_score:court_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w8)

# 3g) with court_date_reminder and business -- drop info
#     +/- sign on max_reuse_score not intuitive
w9 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + problem + urgency +
              max_reuse_score:business + max_reuse_score:court_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w9)





### All initial messages: content and style

init_msg_mod.1 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          future_appointment_date + business + problem + info + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.1)
# sig pos: greeting, user name, future appt date, business
# pos: client name
# sig neg: polite, reuse score, problem, urgency
# neg: closing, yelling, info
# AIC 3691.0

init_msg_mod.1a <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          pls_respond + court_date_reminder + appointment_date_reminder + business + problem + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.1a)
# No

init_msg_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite +
                          future_appointment_date + business + problem + info + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.2)
# sig pos: greeting, user name, future appt date, business
# pos: client name, polite
# sig neg: reuse score, problem, urgency, polite:max_reuse_score
# neg: closing, yelling, info
# AIC 3686.4

init_msg_mod.2a <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + pls_respond + court_date_reminder + appointment_date_reminder +
                          business + problem + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.2a)
# No

init_msg_mod.3 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.3)
# sig pos: greeting, user name, future appt date, business
# pos: client name, polite, closing
# sig neg: problem, urgency, polite:max_reuse_score
# neg: yelling, info, reuse score, closing:max_reuse_score
# AIC 3687.5
## this one

init_msg_mod.3a <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          pls_respond + court_date_reminder + appointment_date_reminder + business + problem + urgency,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.3a)
# No

init_msg_mod.4 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency +
                          max_reuse_score:business,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.4)
# sig pos: greeting, user name, future appt date
# pos: client name, polite, closing, business
# sig neg: problem, urgency, polite:max_reuse_score
# neg: yelling, info, reuse score, closing:max_reuse_score, max_reuse_score:business
# AIC 3689.3

init_msg_mod.4a <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          pls_respond + court_date_reminder + appointment_date_reminder + business + problem + urgency +
                          max_reuse_score:business,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.4a)
# No

init_msg_mod.5 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency +
                          max_reuse_score:business + future_appointment_date:business,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.5)
# sig pos: greeting, user name, future appt date
# pos: client name, business, polite, closing, max_reuse_score:business
# sig neg: problem, urgency, polite:max_reuse_score, future_appointment_date:business
# neg: yelling, info, reuse score, closing:max_reuse_score
# AIC 3679.6

init_msg_mod.5a <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          pls_respond + court_date_reminder + appointment_date_reminder + business + problem + urgency +
                          max_reuse_score:business + future_appointment_date:business,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.5a)
# None of these is any good



### Add metadata to best content + style model

initial_msgs_qualities$send_at_ToD_bins <- relevel(initial_msgs_qualities$send_at_ToD_bins, ref="Morning")

init_msg_mod.6 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency +
                          send_at_ToD_bins,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.6)
# Afternoon is significantly better than morning
# AIC 3687.6
## best one

class(initial_msgs_qualities$send_at_DoW) <- "factor"
initial_msgs_qualities$send_at_DoW <- relevel(initial_msgs_qualities$send_at_DoW, ref = "Wed")

init_msg_mod.7 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency +
                          max_reuse_score:business + send_at_DoW,
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.7)
# With Thu as ref, Sun neg (not sig), all other days pos (not sig)
# With Wed as ref, Sun/Mon/Tue/Thu neg (not sig), all other days pos (not sig)
# With Tue as ref, Sun/Thu neg (not sig), all other days pos (not sig)

initial_msgs_qualities$send_at_hr <- hour(initial_msgs_qualities$send_at)

init_msg_mod.8 <- glmer(msg_replied_i ~ (1 | PO) + 
                          greeting + closing + polite + yelling + max_reuse_score + has_client_name + has_user_name + 
                          max_reuse_score:polite + max_reuse_score:closing +
                          future_appointment_date + business + problem + info + urgency +
                          max_reuse_score:business + sin(pi*send_at_hr/12) + cos(pi*send_at_hr/12),
                        data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_msg_mod.8)
# Difficult to understand. ToD bins are good enough!
