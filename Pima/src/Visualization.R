library(tidyverse)

## Median monthly user message count vs. rate of technical violations
p1.violations <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<6 & min_outcomes$percent_violations>0),], 
                        aes(x = med_msg_count, y = percent_violations)) + 
  geom_point() + geom_smooth(method = "gam", se = FALSE, formula=y~ log(x)) +
  labs(title="Pima Probation Officers' ClientComm Engagement",
       subtitle="vs. Rate of Technical Violations",
       x="Median Messages Sent Per Month",
       y="Rate of Technical Violations")

p1.violations
ggsave("Rate_of_techinal_violations.png", width = 6, height = 4)

## Median monthly user message count vs. rate of new offenses
p1.crimes <- ggplot(min_outcomes[c(min_outcomes$med_msg_count<5),], aes(x = med_msg_count, y = percent_crimes)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE)

p1.crimes
ggsave("Median_msg_count_crimes.png", width = 6, height = 4)

## Median monthly user message count vs. rate of negative outcomes
p1.failures <- ggplot(min_outcomes, aes(x = med_msg_count)) + 
  geom_point(aes(y = percent_violations, color = "percent_violations")) + 
  geom_smooth(aes(y = percent_violations, color = "percent_violations"), method = "gam", se = FALSE, formula=y~ log(x)) + 
  geom_point(aes(y = percent_crimes, color = "percent_crimes")) +
  geom_smooth(aes(y = percent_crimes, color = "percent_crimes"), method = "lm", se = FALSE) + 
  scale_colour_manual("", 
                      breaks = c("percent_violations", "percent_crimes"), 
                      values = c("blue", "red")) +
  labs(title="Pima Probation Officers' ClientComm Engagement",
       subtitle="vs. Rates of Negative Outcomes", 
       x="Median Messages Sent Per Month", 
       y="Rates of Negative Outcomes")

p1.failures
ggsave("Rates_of_negative_outcomes.png", width = 6, height = 4)

## Viz of pima.tv3.h2 predictions: see Modeling script line 238

