Sys.setenv(TZ='America/New_York')

pima.data <- read.csv(file = file.path("data", "pima.data.csv"))
pima.outcomes <- read.csv(file = file.path("data", "pima.outcomes.csv"))

pima.outcomes.unique <- unique(cbind(pima.outcomes[,c(1:3)]))

temp2 <- inner_join(pima.outcomes, pima.data, by = c("client_last_name", "client_first_name"))

temp2.unique <- inner_join(pima.outcomes.unique, pima.data, by = c("client_last_name", "client_first_name"))

temp2.unique <- unique(cbind(temp2.unique[,c(1:3)]))

temp3 <- anti_join(pima.data, temp2.unique, by = c("client_last_name", "client_first_name"))

temp4 <- anti_join(pima.outcomes.unique, temp2.unique, by = c("client_last_name", "client_first_name"))

ln.join <- inner_join(temp3, temp4, by = c("client_last_name"))

ln.join$first_name_match <- stri_detect(ln.join$client_first_name.x, fixed = ln.join$client_first_name.y)

ln.join <- ln.join %>% filter(first_name_match == TRUE)

names(ln.join)[3] <- "client_first_name"

temp3 <- anti_join(temp3, ln.join, by = c("client_last_name", "client_first_name"))

temp4 <- anti_join(temp4, ln.join, by = c("client_last_name" = "client_last_name", "client_first_name" = "client_first_name.y"))

unmatched.pima.data <- temp3
unmatched.pima.outcomes <- temp4


