library(tidyverse)

msg_check.1 <- read.csv("msg_check.1.csv")
users <- read.csv("users.csv")

View(users[,c("id","full_name")])

treatment_group <- c(10,12:15,17,18,20,25)

msg_check.1.treatment <- msg_check.1[msg_check.1$user_id %in% treatment_group,]
msg_check.1.control <- msg_check.1[!(msg_check.1$user_id %in% treatment_group),]

View(msg_check.1.treatment[,c(1,2,4)])
View(msg_check.1.control[,c(1,2,4)])
