# import data 
library(dplyr)
library(ggpubr)
#### Read raw data 
brfss2018_raw = readRDS(file = "data/brfss2018.rds")
vars_of_interest = tolower(c(
  "x.psu",
  "x.age.g",
  "x.state",
  "x.race",
  "x.LLCPWT",
  "x.IMPRACE",
  "GENHLTH",
  "HLTHPLN1",
  "EXERANY2",
  "SLEPTIM1", # On average, how many hours of sleep do you get in a 24-hour period?
  "CHCSCNCR", # Skin Cancer
  "CHCOCNCR", # Any Other Cancer
  "SEX1", # 1 Male 2 Female
  "EDUCA",
  "EMPLOY1",
  "CHILDREN",
  "INCOME2",
  "x.incomg",
  "WEIGHT2",
  "HEIGHT3",
  "SMOKE100",
  "ALCDAY5",
  "CNCRTYP1"))


# dimemsion of data
dim(brfss2018_raw[vars_of_interest])
# nrow() = 437436 

processed_df = brfss2018_raw[vars_of_interest]
processed_df = processed_df %>% 
  mutate( alcday5_mod  = 
            case_when(
              alcday5 <= 300 & alcday5 >= 200 ~  as.numeric(alcday5-200),
              alcday5 <= 200 & alcday5 >= 100 ~ as.numeric((alcday5-100) *4.0),
              alcday5 == 888 ~ as.numeric(0),
              TRUE ~ as.numeric(NA)
            )
  )

processed_df = 
  processed_df %>% 
  mutate(cancer = ifelse(chcscncr==1 | chcocncr==1, "Yes", "DK"),
         cancer = ifelse(chcscncr==2 & chcocncr==2, "No", cancer)) %>% 
  mutate(age_l = ifelse(x.age.g==1, "18-24", "65+"),
         age_l = ifelse(x.age.g==2, "25-34",age_l),
         age_l = ifelse(x.age.g==3, "35-44",age_l),
         age_l = ifelse(x.age.g==4, "45-54", age_l),
         age_l = ifelse(x.age.g==5, "55-64",age_l)) %>% 
  mutate(
    sleptim1_cat = 
      case_when(
        sleptim1 < 7  ~"1 Insufficient sleep",
        sleptim1 >= 7 & sleptim1 <= 10 ~"2 Adquate sleep",
        sleptim1 > 10 & sleptim1 <= 24 ~"3 Excessive sleep",
        TRUE ~ NA_character_
      ) ,
    sex1 = 
      case_when(
        sex1 == 1  ~"Male",
        sex1 == 2 ~"Female",
        TRUE ~ NA_character_
      ),
    x.imprace = 
      case_when(
        x.imprace == 1  ~"Non-Hispanic, White",
        x.imprace == 2  ~"Non-Hispanic, Black",
        x.imprace == 5  ~"Hispanic",
        TRUE ~ "Non-Hispanic, Other"
      )
  )

processed_df = processed_df %>% mutate(
  smoke_mod = 
    case_when(
      smoke100 == 1  ~"yes",
      smoke100 == 2  ~"no",
      TRUE ~ NA_character_
    ),
  cancer_num = 
    ifelse(cancer == "Yes",1,0),
  
  exercise_cat = 
    case_when(
      exerany2 == 1  ~"yes",
      exerany2 == 2  ~"no",
      TRUE ~ NA_character_
    ),
  
  health_cover = 
    case_when(
      hlthpln1  == 1  ~"yes",
      hlthpln1  == 2  ~"no",
      TRUE ~ NA_character_
    ),
  edu_cat_mod = 
    case_when(
      educa == 1  ~"  Less than HS",
      educa == 2  ~"  Less than HS",
      educa == 3  ~"  Less than HS",
      educa == 4  ~" HS graduate",
      educa == 5 ~" Some College",
      educa == 6  ~"College Graduate",
      TRUE ~ NA_character_
    )
)
# change state name base on fib code
fip_state_ref = read.csv("data/state_fip_code.csv")
processed_df$x.state = as.numeric(processed_df$x.state)
processed_df = left_join(processed_df, fip_state_ref,  by = c("x.state" = "state_code"))
# filter NA 
processed_df = filter(processed_df, cancer != "DK" )
# save
saveRDS(processed_df, file="data/primary_df_1.rds")


#### Build models 
raw_df = read_rds("data/primary_df_1.rds")
raw_df$cancer = as.factor(raw_df$cancer)
raw_df$educa = as.factor(raw_df$educa)
raw_df$x.incomg = as.factor(raw_df$x.incomg)
raw_df$x.incomg %>% table()
raw_df$educa %>% table()
raw_df$educa[raw_df$educa == 9] = NA
raw_df$educa = factor(raw_df$educa)
raw_df$cncrtyp1 %>% table()

#### survey design 
svy_obj_2 <- svydesign(id=~x.psu, weights= ~x.llcpwt, strata = ~x.state , data=raw_df, nest=TRUE)
glm_model_4 = svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + alcday5_mod + smoke_mod + health_cover + exercise_cat + x.incomg + educa , design = svy_obj_2, family = "binomial") 
glm_model_4 %>% summary()

#### code for Figure 1 and Figure 2 
df = read_rds("data/primary_df_1.rds") %>%
  mutate(cancer = as.factor(cancer),
         genhlth = as.factor(genhlth),
         hlthpln1 = as.factor(hlthpln1),
         exerany2 = as.factor(exerany2),
         smoke100 = as.factor(smoke100)) 
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

ggarrange( plot_gender,plot_heath, df_caner_race_plot, df_caner_age,   
           labels = c( "Gender","Heath", "Race/Ethnic","Age"))


ggarrange(df_caner_healthcare ,df_caner_exe,df_caner_alcohol ,df_caner_smoke,       
          labels = c("Health Care", "Exercise","Alcohol","Smoke"))

#### generate data for table 2 (the glm result)
confint_glm <- function(object, parm, level = 0.95, ...) {
  coef = coef(summary(object)) %>% as.data.frame()
  coef_CI = object %>% confint() %>% as.data.frame()
  table = cbind(coef, coef_CI) %>% 
    mutate(Exp.Est = round(exp(Estimate),4),
           CIL = round(`2.5 %`,4),
           CIU = round(`97.5 %`,4),
           Std.Error = round(`Std. Error`,4),
           Estimate = round(Estimate,4)) %>%
    dplyr::select(Estimate, Exp.Est, Std.Error, CIL, CIU)
  rownames(table) <- rownames(coef)
  return(table)
}
confint_glm(glm_model_4) %>% knitr::kable()

# code for rename 
# mutate(
#   vars_rename = 
#     case_when(
#       vars  == "(Intercept)" ~ "Inter",
#       vars  == "age_l25-34"  ~ "Age (25-34)",
#       vars  == "age_l35-44"  ~ "Age (35-44)",
#       vars  == "age_l45-54"  ~ "Age (45-54)",
#       vars  == "age_l55-64"  ~ "Age (55-64)",
#       vars  == "age_l65+"  ~   "Age (65+)",
#       vars  == "sex1Male"  ~   "Sex (Male)",
#       vars  == "x.impraceNon-Hispanic, Black"  ~ "Non-Hispanic, Black",
#       vars  == "x.impraceNon-Hispanic, Other"  ~ "Non-Hispanic, Other",
#       vars  == "x.impraceNon-Hispanic, White"  ~ "Non-Hispanic, White",
#       vars  == "sleptim1_cat2 Adquate sleep"   ~ "Sleep (Adequate)",
#       vars  == "sleptim1_cat3 Excessive sleep" ~ "Sleep (Excessive)",
#       vars  == "alcday5_mod"      ~ "Days of drinks",
#       vars  == "smoke_modyes"     ~ "Smoke Status",
#       vars  == "health_catyes"    ~ "Health Coverage",
#       vars  == "exercise_catyes"  ~ "Exercise",
#       TRUE ~ NA_character_
#     )
# )