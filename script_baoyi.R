# genhlth: Health Status---Would you say that in general your health is?
#   1 Excellent;2 Very good; 3 Good; 4 Fair; 5 Poor; 7 Don't know/Not sure; 9 Refused

# hlthpln1：Do you have any kind of health care coverage? 
#   1 Yes; 2 No; 7 Don't know/Not sure; 9 Refused

# exerany2: Exercise---During the past month, other than your regular job, 
# did you participate in any physical activities or exercises? 
#   1 Yes;2 No; 7 Don't know/Not sure; 9 Refused

library(dplyr)
library(ggplot2)

df = read.csv("data/data_ver_3.csv") %>%
  select(genhlth, hlthpln1, exerany2, sleptim1, chcscncr, chcocncr) %>% 
  mutate(cancer = ifelse(chcscncr==1 | chcocncr==1, "Yes", "DK"),
         cancer = ifelse(chcscncr==2 & chcocncr==2, "No", cancer),
         genhlth = as.factor(genhlth),
         hlthpln1 = as.factor(hlthpln1),
         exerany2 = as.factor(exerany2))

levels(df$genhlth) = c("Excellent","Very good","Good","Fair",
                       "Poor","Don't know/Not sure","Refused")

levels(df$hlthpln1) = c("Yes","No","Don't know/Not sure","Refused")

levels(df$exerany2) = c("Yes","No","Don't know/Not sure","Refused")

ggplot(data = na.omit(df), 
       aes(x = cancer, fill = genhlth)) + geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Cancer", fill = "General Health",
       title = "General Health Status Proportions Across Cancer Group")

ggplot(data = na.omit(df), 
       aes(x = cancer, fill = hlthpln1)) + geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Cancer", fill = "Health Care Coverage",
       title = "Health Care Coverage Proportions Across Cancer Group")

ggplot(data = na.omit(df), 
       aes(x = cancer, fill = exerany2)) + geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Cancer", fill = "Exersise",
       title = "Exercise Proportions Across Cancer Group")