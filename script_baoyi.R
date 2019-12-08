# genhlth: Health Status---Would you say that in general your health is?
#   1 Excellent;2 Very good; 3 Good; 4 Fair; 5 Poor; 7 Don't know/Not sure; 9 Refused

# hlthpln1：Do you have any kind of health care coverage? 
#   1 Yes; 2 No; 7 Don't know/Not sure; 9 Refused

# exerany2: Exercise---During the past month, other than your regular job, 
# did you participate in any physical activities or exercises? 
#   1 Yes;2 No; 7 Don't know/Not sure; 9 Refused

# smoke100: Tobacco Use---Have you smoked at least 100 cigarettes in your entire life?
#   1 Yes;2 No; 7 Don't know/Not sure; 9 Refused 

# alcday5_mod: Alcohol Consumption---During the past 30 days, how many days did you have 
# at least one drink of any alcoholic beverage?

library(dplyr)
library(ggplot2)
library(tidyverse)

df = read_rds("./data/primary_df_1.rds") %>%
  select(genhlth, hlthpln1, exerany2, alcday5_mod, smoke100, sleptim1, cancer, x.llcpwt) %>% 
  mutate(cancer = as.factor(cancer),
         genhlth = as.factor(genhlth),
         hlthpln1 = as.factor(hlthpln1),
         exerany2 = as.factor(exerany2),
         smoke100 = as.factor(smoke100)) 

levels(df$genhlth) = c("Excellent","Very good","Good","Fair",
                       "Poor","Don't know/Not sure","Refused")

levels(df$hlthpln1) = c("Yes","No","Don't know/Not sure","Refused")

levels(df$exerany2) = c("Yes","No","Don't know/Not sure","Refused")

levels(df$smoke100) = c("Yes","No","Don't know/Not sure","Refused")

ggplot(data = na.omit(df), 
       aes(x = genhlth, fill = cancer, weight = x.llcpwt)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "General Health", fill = "Cancer",
       title = "Cancer Proportions Across Health Status Group") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

ggplot(data = na.omit(df), 
       aes(x = hlthpln1, fill = cancer, weight = x.llcpwt)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Health Care Coverage", fill = "Cancer",
       title = "Cancer Proportions Across Health Care Coverage")

ggplot(data = na.omit(df), 
       aes(x = exerany2, fill = cancer, weight = x.llcpwt)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Exersise", fill = "Cancer",
       title = "Cancer Proportions Across Exercise Group")

ggplot(data = na.omit(df), 
       aes(x = smoke100, fill = cancer, weight = x.llcpwt)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", x = "Smoking", fill = "Cancer",
       title = "Cancer Proportions Across Smoking Group")

ggplot(data = na.omit(df), 
       aes(x = cancer, y = alcday5_mod, weight = x.llcpwt)) + 
  geom_boxplot(na.rm = TRUE) +
  labs(y = "Alcohol", x = "Cancer", 
       title = "Alcohol Box Across Cancer Group")
  