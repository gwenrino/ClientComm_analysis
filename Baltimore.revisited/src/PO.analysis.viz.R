library(tidyverse)

##### Do POs who use CC more (i.e. more client messages) have better outcomes? #####

p1 <- ggplot(PO_data, aes(x = mean_msg_count, y = percent_failures)) + geom_point() + geom_smooth(method = "lm", se = TRUE)# Yes
p2 <- ggplot(PO_data, aes(x = max_msg_count, y = percent_failures)) + geom_point() + geom_smooth(method = "lm", se = FALSE) # Yes
p5 <- ggplot(PO_data, aes(x = med_msg_count, y = percent_failures)) + geom_point()

##### Do POs who write more personal messages (i.e. lower overall reuse score) have better outcomes? #####

p3 <- ggplot(PO_data, aes(x = mean_reuse_score, y = percent_failures)) + geom_point() # Hm, no

p4 <- ggplot(PO_data, aes(x = mean_msg_count, y = percent_failures, color = mean_reuse_score)) + geom_point()

ggsave(filename="mean_use_of_CC.pdf", plot=p1)
ggsave(filename="max_use_of_CC.pdf", plot=p2)
ggsave(filename="mean_reuse_score.pdf", plot=p3)
