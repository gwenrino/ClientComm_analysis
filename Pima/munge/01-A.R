Sys.setenv(TZ='America/New_York')
library(tidyverse)
library(stringi)

pima.data <- read.csv(file = file.path("data", "pima.data.csv"))
pima.outcomes <- read.csv(file = file.path("data", "pima.outcomes.csv"))

pima.active <- pima.data[pima.data$active == TRUE,]
pima.active$active <- NULL

pima.outcomes.unique <- unique(pima.outcomes[,c(1:3,10,12)])

temp1 <- inner_join(pima.outcomes.unique, pima.active, by = c("client_last_name", "client_first_name"))

temp2 <- anti_join(pima.active, temp1, by = c("client_last_name", "client_first_name"))

temp3 <- anti_join(pima.outcomes.unique, temp1, by = c("client_last_name", "client_first_name"))

ln.join <- inner_join(temp2, temp3, by = c("client_last_name"))

ln.join$first_name_match <- stri_detect(ln.join$client_first_name.x, fixed = ln.join$client_first_name.y)

ln.join.match <- ln.join %>% filter(first_name_match == TRUE)

names(ln.join.match)[3] <- "client_first_name"

ln.join.match <- ln.join.match[,c(11,2,3,13,14,1,4:10)]

temp2 <- anti_join(temp2, ln.join.match, by = c("client_last_name", "client_first_name"))

temp3 <- anti_join(temp3, ln.join.match, by = c("client_last_name","client_first_name"))

unmatched.pima.data <- temp2

unmatched.pima.outcomes <- temp3

pima.matched.data <- rbind(temp1, ln.join.match)
# There are still deduping problems. See Richard Bailey and Nicole Benson, e.g.


