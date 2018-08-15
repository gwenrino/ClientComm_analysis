Sys.setenv(TZ='America/New_York')
library(tidyverse)
library(stringi)

pima.data <- read.csv(file = file.path("data", "pima.data.csv"))
pima.outcomes <- read.csv(file = file.path("data", "pima.outcomes.csv"))

pima.active <- pima.data[pima.data$active == TRUE,]
pima.active$active <- NULL

pima.outcomes.unique <- unique(pima.outcomes[,c(1:3,10,12)])

temp1 <- inner_join(pima.outcomes.unique, pima.active, by = c("client_last_name", "client_first_name"))
# I can't dedupe this set without losing CC info because some clients are associated with more than one user_id
# Already subset for active reporting relationship, so not sure what to do about this

temp1.unique <- unique(temp1[,c(1:7,10,12)]) # deduped, but info lost

temp2 <- anti_join(pima.active, temp1.unique, by = c("client_last_name", "client_first_name"))

temp3 <- anti_join(pima.outcomes.unique, temp1.unique, by = c("client_last_name", "client_first_name"))

ln.join <- inner_join(temp2, temp3, by = c("client_last_name"))

ln.join$first_name_match <- stri_detect(ln.join$client_first_name.x, fixed = ln.join$client_first_name.y)

ln.join <- ln.join %>% filter(first_name_match == TRUE)

names(ln.join)[3] <- "client_first_name"
# Same dedupe problem as temp1

ln.join.unique <- unique(ln.join[,c(11,2,3,13,14,1,4,7,9)]) # dedeuped, but info lost

temp2 <- anti_join(temp2, ln.join.unique, by = c("client_last_name", "client_first_name"))

temp3 <- anti_join(temp3, ln.join.unique, by = c("client_last_name","client_first_name"))

unmatched.pima.data <- temp2

unmatched.pima.outcomes <- temp3

pima.matched.data <- rbind(temp1.unique, ln.join.unique)
# There are still deduping problems. See Richard Bailey and Nicole Benson, e.g.
