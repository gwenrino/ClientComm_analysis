Sys.setenv(TZ='America/New_York')

## Qualities of messages that get responses ##

msg_mod.1 <- glm(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling, 
                 data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.1)
# all sig except urgency
# closing/polite negatively associated -- what does this mean?
# AIC 12654

msg_mod.2 <- glm(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling +
                    future_appointment_date, data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.2)
# same, fut appt dates sig
# AIC 12594

msg_mod.3 <- glm(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling +
                   future_appointment_date + PO, data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.3)
# POs matter -- try glmer

msg_mod.4 <- glmer(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling +
                     future_appointment_date + (1 | PO), data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.4)

msg_mod.5 <- glmer(first_msg_replied_i ~ greeting + closing + polite + business + problem + yelling +
                     future_appointment_date + (1 | PO), data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.5)
# without urgency is better than with
# AIC 12469.7

msg_mod.6 <- glmer(first_msg_replied_i ~ greeting + business + problem + yelling +
                     future_appointment_date + (1 | PO), data = user_msgs_qualities, family = 'binomial')
summary(msg_mod.6)
# removing closing and polite makes model less efficient
# AIC 12528.9
# what does it mean that closing and polite are significantly negatively associated with getting a response?


## Qualities of first messages that get responses ##

first_msgs_qualities <- user_msgs_qualities[user_msgs_qualities$initial_msg_indicator == 1,] # filter for initial messages


first_msg_mod.1 <- glm(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling, 
                       data = first_msgs_qualities, family = 'binomial')
summary(first_msg_mod.1)
# sig pos associations: greeting, business
# sig neg associations: polite, urgency, yelling
# not sig pos: closing
# not sig neg: problem
# AIC 3797.7

first_msg_mod.2 <- glm(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling +
                   future_appointment_date, data = first_msgs_qualities, family = 'binomial')
summary(first_msg_mod.2)
# same, fut appt dates sig
# AIC 3788.3

first_msg_mod.4 <- glmer(first_msg_replied_i ~ greeting + closing + polite + business + problem + urgency + yelling +
                     future_appointment_date + (1 | PO), data = first_msgs_qualities, family = 'binomial')
summary(first_msg_mod.4)

first_msg_mod.5 <- glmer(first_msg_replied_i ~ greeting + polite + business + yelling +
                     future_appointment_date + (1 | PO), data = first_msgs_qualities, family = 'binomial')
summary(first_msg_mod.5)
# leaving out not sig variables doesn't improve the model's AIC 3741.1
# problem, urgency, yelling all negatively associated with desired outcome:: these don't belong in first messages?

first_msg_mod.6 <- glmer(first_msg_replied_i ~ greeting + business + problem + urgency + future_appointment_date + yelling +
                           (1 | PO), data = first_msgs_qualities, family = 'binomial')
summary(first_msg_mod.6)
# just the variables whose pos/neg makes sense in this model
# problem not sig
# AIC 3758.6 not as good as first_msg_mod.4
