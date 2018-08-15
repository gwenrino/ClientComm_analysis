### VISUALIZING THE EFFECT OF CLIENT PARTICIPATION ###

nrow(cc.dtf) # 1854
nrow(cc.dtf[cc.dtf$supervision_failure == 1,]) # 142
142/1854 # = 0.0766 -- percent of clients who failed supervision

mod.2 <- glmer(supervision_failure ~ (1 | PO) +
                 (client_msg_count > 0 & client_msg_count < 3)  +
                 (client_msg_count > 2),
               data = cc.dtf, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(mod.2) 


cc.dtf_msg_count_0 <- cc.dtf
cc.dtf_msg_count_0$client_msg_count <- 0

cc.dtf_msg_count_1.5 <- cc.dtf
cc.dtf_msg_count_1.5$client_msg_count <- 1.5

cc.dtf_msg_count_3 <- cc.dtf
cc.dtf_msg_count_3$client_msg_count <- 3

predictions_msg_count_0 <- predict(mod.2, newdata = cc.dtf_msg_count_0, type = "response")
predictions_msg_count_1.5 <- predict(mod.2, newdata = cc.dtf_msg_count_1.5, type = "response")
predictions_msg_count_3 <- predict(mod.2, newdata = cc.dtf_msg_count_3, type = "response")

mean(predictions_msg_count_0) 
# 0.095 -- percent predicted to fail if no msgs sent
mean(predictions_msg_count_1.5) 
# 0.065 -- percent predicted to fail if 1-2 msgs sent
mean(predictions_msg_count_3) 
# 0.053 -- percent predicted to fail if 3 msgs sent


## USE THESE ##
sent_no_msgs <- cc.dtf[cc.dtf$client_msg_count == 0,]
sent_one_plus_msg <- cc.dtf[cc.dtf$client_msg_count >= 1,]

nrow(sent_no_msgs[sent_no_msgs$supervision_failure == 1,])/nrow(sent_no_msgs)
nrow(sent_one_plus_msg[sent_one_plus_msg$supervision_failure == 1,])/nrow(sent_one_plus_msg)

client_responses <- data.frame("Clients" = c("Sent 0 msgs", "Sent 1+ msg", "Total"),
                               "Count" = c(866,988,1854), "Failed" = c(83,59,142), "Percent failed" = c("9.6%", "6.0%", "7.7%"))

cc.dtf.plot <- cc.dtf %>% mutate(client_msg_TF=
                                   case_when(client_msg_count == 0 ~ FALSE,
                                             client_msg_count >= 1 ~ TRUE))

cc.dtf.plot <- ggplot(cc.dtf.plot, aes(x=client_msg_TF, fill=supervision_failure), cex=10) + 
  geom_bar(position = "stack") +  
  theme_minimal() +
  theme(plot.title = element_text(size = 24, face = "bold"), 
        plot.subtitle = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20, face = "bold")) +
  labs(title="Impact of Client Engagement on Supervision Outcomes", 
       subtitle="n=1854",
       x="Client Engagement", 
       y="Number") + 
  scale_fill_manual("Outcome", values=c("aquamarine1","gold1"), labels=c("Supervision success", "Supervision failure")) +
  annotate("text", x = 1, y = 30, label = "9.6% failure rate", size = 10, color="black") +
  annotate("text", x = 2, y = 30, label = "6.0% failure rate", size = 10, color="black") +
  annotate("text", x = 1, y = 500, label = "866 clients \n sent 0 messages", size = 10, color="black") +
  annotate("text", x = 2, y = 500, label = "988 clients \n sent 1+ messages", size = 10, color="black")

ggsave(filename="Client_Engagement.pdf", plot=cc.dtf.plot)


## The importance of the initial message ##

nrow(temp4) # 30357
nrow(temp4[temp4$msg_replied_i == 1,]) # 1654
nrow(temp4[temp4$initial_msg_indicator == 1,]) # 3339
nrow(temp4[temp4$initial_msg_indicator == 1 & temp4$msg_replied_i == 1,]) # 888
