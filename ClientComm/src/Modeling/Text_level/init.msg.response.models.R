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
## Makes sense



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
# sig pos: appointment_date, pls_respond, business
# pos: court_date
# sig neg: problem, info
# neg: urgency
# AIC 3749.5

init_content_mod.2 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + max_reuse_score:business, # interaction of business and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.2)
# sig pos: future appt date
# pos: business
# sig neg: problem, urgency, reuse score
# neg: info, business:reuse score
# AIC 3733.6

init_content_mod.2a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond +
                               business + problem + info + urgency + max_reuse_score + 
                               max_reuse_score:business, # interaction of business and max_reuse
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.2a)
# sig pos: appointment_date, pls_respond
# pos: business
# sig neg: problem, reuse score
# neg: court_date, urgency, business:reuse score, info
# AIC 3723.4

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
# sig pos: appt date, pls_respond
# pos: business, problem:reuse
# sig neg: reuse score
# neg: court date, problem, urgency, bus:reuse, info
# AIC 3724.4

init_content_mod.4 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + future_appointment_date:business, # interaction of date and business
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.4)
# sig pos: future appt date, business
# pos: 
# sig neg: urgency, problem, reuse score, future_appointment_date:business
# neg: info
# AIC 3724.5

init_content_mod.5 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + business + problem + 
                              info + urgency + max_reuse_score + future_appointment_date:business + max_reuse_score:business, 
                            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.5)
# sig pos: future appt date
# pos: business
# sig neg: urgency, problem, reuse score, future_appointment_date:business
# neg: info, business:max_reuse_score
# AIC 3726.4

init_content_mod.6a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + info + urgency + max_reuse_score + 
                               max_reuse_score:business + max_reuse_score:court_date_reminder + 
                               max_reuse_score:appointment_date_reminder + max_reuse_score:problem, # interactions with max_reuse
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.6a)
# sig pos: pls_respond, business
# pos: court date
# sig neg: reuse_score, appointment date, appointment:reuse
# neg: problem, info, urgency, court date
# AIC 3716.0
# strange that reminders are neg in this model but pos in previous models -- interaction with reuse

init_content_mod.7a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + urgency + max_reuse_score + 
                               max_reuse_score:business + max_reuse_score:court_date_reminder, 
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.7a)
# sig pos: appt date, pls_respond
# pos: court date, business
# sig neg: reuse score, problem
# neg: urgency, interaction terms
# AIC 3725.4

init_content_mod.8a <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + pls_respond + 
                               business + problem + urgency + max_reuse_score + info +
                               max_reuse_score:business + max_reuse_score:court_date_reminder, 
                             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(init_content_mod.8a)
# sig pos: appt date, pls_respond
# pos: court date, business
# sig neg: reuse score, problem, info
# neg: urgency, interaction terms
# AIC 3724.5



# Confusing! Try to track this effect down.

# 1) with future_appointment_date, all makes sense
w1 <- glmer(msg_replied_i ~ (1 | PO) + future_appointment_date + 
             pls_respond + business + max_reuse_score + info + problem + urgency, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w1)

# 2) replace future_appointment_date with court_date_reminder and appointment_date_reminder
#    AIC 3722.5, direction seems wrong on court date
w2 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w2)

# adding interactive terms with max_reuse_score
# 3a) max_reuse_score:court_date_reminder
#     AIC 3723.7, all directions seem good
w3 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:court_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w3)

# 3b) max_reuse_score:appointment_date_reminder
#     AIC 3715.0, direction seems wrong on date reminders
w4 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:appointment_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w4)

# 3c) max_reuse_score:business
#     AIC 3723.4, direction seems wrong on court date
w5 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:business, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w5)

# 3d) max_reuse_score:info
#     AIC 3724.5, direction seems wrong on court date
w6 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:info, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w6)

# 3e) max_reuse_score:court_date_reminder and max_reuse_score:business
#     AIC 3724.5, all directions seem good
w7 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              max_reuse_score:business + max_reuse_score:court_date_reminder, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w7)


# Seems like the interaction terms don't really help the model or the explanation. 
# Choose w2 (no interaction terms)

# Add style terms to w2

# No interactions
# AIC 3683.5
w8 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              greeting + closing + polite + yelling + has_client_name + has_user_name, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w8)

# Try taking out politeness term (likely correlated with max_reuse)
# AIC 3697.0 *** NO ***
w9 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency +
              greeting + closing + yelling + has_client_name + has_user_name, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w9)

# Try with polite:max_reuse
# AIC 3678.1
w10 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
              pls_respond + business + max_reuse_score + info + problem + urgency + polite +
              greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score, 
            data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w10)

# Try without court_date_reminder (?correlation with max_reuse)
# AIC 3681.8
w11 <- glmer(msg_replied_i ~ (1 | PO) + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w11)

# Without court_date_reminder, with polite:max_reuse
# AIC 3676.8
w12 <- glmer(msg_replied_i ~ (1 | PO) + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w12)

# Try with court_date, without info
# AIC 3679.2
w13 <- glmer(msg_replied_i ~ (1 | PO) + appointment_date_reminder + court_date_reminder +
               pls_respond + business + max_reuse_score + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w13)

## Best model with content and style is w10 AIC 3678.1
## Substantially better than content-only model w2 AIC 3722.5
## And better than style-only model init_style_mod.2 AIC 3710.4



### Add metadata to best content + style model (w10)

initial_msgs_qualities$send_at_ToD_bins <- relevel(initial_msgs_qualities$send_at_ToD_bins, ref="Morning")

w14 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score +
               send_at_ToD_bins, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w14)
# Afternoon is significantly better than morning
# AIC 3679.7

class(initial_msgs_qualities$send_at_DoW) <- "factor"
initial_msgs_qualities$send_at_DoW <- relevel(initial_msgs_qualities$send_at_DoW, ref = "Wed")

w15 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score +
               send_at_DoW, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w15)
# DoW not significant

initial_msgs_qualities$send_at_hr <- hour(initial_msgs_qualities$send_at)

w16 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score +
               sin(pi*send_at_hr/12) + cos(pi*send_at_hr/12), 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
summary(w16)
# ToD not significant and also difficult to understand. ToD bins are good enough!

####### Best model #########
w14 <- glmer(msg_replied_i ~ (1 | PO) + court_date_reminder + appointment_date_reminder + 
               pls_respond + business + max_reuse_score + info + problem + urgency + polite +
               greeting + closing + yelling + has_client_name + has_user_name + polite:max_reuse_score +
               send_at_ToD_bins, 
             data = initial_msgs_qualities, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))
############################
