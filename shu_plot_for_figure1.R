library(dplyr)
library(ggplot2)
library(tidyverse)
library(survey)
library(envalysis)
theme_set(theme_classic(base_size = 8))



df = read_rds("data/primary_df_1.rds") %>%
  # select(genhlth, hlthpln1, exerany2, alcday5_mod, smoke100, sleptim1, cancer, x.llcpwt) %>% 
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

df$cancer %>% table()
svy_obj_2 <- svydesign(id=~x.psu, weights= ~x.llcpwt, strata = ~x.state , data=df, nest=TRUE)

colnames(df)
# df_genhlth = svymean(~genhlth,svy_obj_2,na.rm =TRUE)


df_caner_genhlth  = svyby( ~ cancer,  ~ genhlth, svy_obj_2, na.rm = TRUE, svymean)
plot_heath = df_caner_genhlth %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes,
         name = c("Excellent","Very good", "Good","Fair","Poor", "Don't know/Not sure", "Refused")
  ) %>% 
  ggplot() +
  geom_col(aes(x = genhlth, y = cancerYes)) +
  geom_errorbar(aes(x = name, ymin = lower, ymax = upper))+
  theme_publish() + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("General Health")
plot_heath 
#-- gender 

df_caner_gender  = svyby( ~ cancer,  ~ sex1, svy_obj_2, na.rm = TRUE, svymean)
plot_gender = df_caner_gender %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = sex1, y = cancerYes)) +
  geom_errorbar(aes(x = sex1, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("Gender")
plot_gender 


df_caner_race  = svyby( ~ cancer,  ~ x.imprace, svy_obj_2, na.rm = TRUE, svymean)
df_caner_race_plot = df_caner_race %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = x.imprace, y = cancerYes)) +
  geom_errorbar(aes(x = x.imprace, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("Race")
df_caner_race_plot 

# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod

df_caner_age  = svyby( ~ cancer,  ~ age_l, svy_obj_2, na.rm = TRUE, svymean)
df_caner_age = df_caner_age %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = age_l, y = cancerYes)) +
  geom_errorbar(aes(x = age_l, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("Age Groups")
df_caner_age 

# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod

df_caner_healthcare  = svyby( ~ cancer,  ~ health_cover, svy_obj_2, na.rm = TRUE, svymean)
df_caner_healthcare = df_caner_healthcare %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = health_cover, y = cancerYes)) +
  geom_errorbar(aes(x = health_cover, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("healthcare coverage")
df_caner_healthcare 

# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod

df_caner_exe  = svyby( ~ cancer,  ~ exercise_cat, svy_obj_2, na.rm = TRUE, svymean)
df_caner_exe = df_caner_exe %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = exercise_cat, y = cancerYes)) +
  geom_errorbar(aes(x = exercise_cat, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("Exercise status")
df_caner_exe 

# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod
# df$alcday5_mod %>% table()
# df %>% mutate(
#   alcohol_mod = 
# )
# 
# df_caner_alcohol  = svyby( ~ cancer,  ~ alcday5_mod, svy_obj_2, na.rm = TRUE, svymean)
# df_caner_alcohol = df_caner_race %>%
#   mutate(upper = cancerYes + 1.96 * se.cancerYes,
#          lower = cancerYes - 1.96 * se.cancerYes
#   ) %>% 
#   ggplot() +
#   geom_col(aes(x = alcday5_mod, y = cancerYes)) +
#   geom_errorbar(aes(x = alcday5_mod, ymin = lower, ymax = upper))+
#   theme_publish() +
#   theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
#   ylab("Weighted Propotion have cancer") +
#   xlab("Alcohol status")
df_caner_alcohol  = 
  ggplot(data = df,
         aes(x = cancer, y = alcday5_mod, weight = x.llcpwt)) +
  geom_boxplot(na.rm = TRUE) +
  theme_publish() +
  labs(y = "Alcohol", x = "Cancer")


# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod

df_caner_smoke  = svyby( ~ cancer,  ~ smoke_mod, svy_obj_2, na.rm = TRUE, svymean)
df_caner_smoke = df_caner_smoke %>%
  mutate(upper = cancerYes + 1.96 * se.cancerYes,
         lower = cancerYes - 1.96 * se.cancerYes
  ) %>% 
  ggplot() +
  geom_col(aes(x = smoke_mod, y = cancerYes)) +
  geom_errorbar(aes(x = smoke_mod, ymin = lower, ymax = upper))+
  theme_publish() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  ylab("Weighted Propotion have cancer") +
  xlab("Smoking Status")
df_caner_smoke 

df_caner_gender
df_caner_race 
df_caner_age 
df_caner_healthcare 
df_caner_exe 
df_caner_alcohol 
df_caner_smoke 
# colnames(df) # age_l/ health_cover /exercise_cat / alcday5_mod /smoke_mod
# df_genhlth =
#   as.data.frame(df_genhlth) %>%
#   mutate(upper = mean + 1.96 * SE,
#          lower = mean - 1.96 * SE,
#          name = c("Excellent","Very good", "Good","Fair","Poor", "Don't know/Not sure", "Refused")
#          )
# df_genhlth$name = factor(df_genhlth$name, c("Excellent","Very good", "Good","Fair","Poor", "Don't know/Not sure", "Refused"))
# 
# df_genhlth %>%
#   ggplot() +
#   geom_col(aes(x = name, y = mean)) +
#   geom_errorbar(aes(x = name, ymin = lower, ymax = upper))+
#   theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
#   ylab("Weighted Propotion") +
#   xlab("General Health")
library(ggpubr)
figure <- ggarrange(plot_gender, plot_heath, df_caner_race, df_caner_age, df_caner_healthcare ,
                     df_caner_exe,df_caner_alcohol ,df_caner_smoke,       
                    labels = c("Gender", "Heath", "Race/Ethnic","Age","Health Care", "Exercise","Alcohol","Smoke"))

figure

ggarrange( plot_gender,plot_heath, df_caner_race_plot, df_caner_age,   
          labels = c( "Gender","Heath", "Race/Ethnic","Age"))


ggarrange(df_caner_healthcare ,df_caner_exe,df_caner_alcohol ,df_caner_smoke,       
          labels = c("Health Care", "Exercise","Alcohol","Smoke"))






