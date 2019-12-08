library(tidyverse)
raw_df = read_rds("data/primary_df_1.rds") 

cancer_type_df = read.csv("data/brfss_2018_cancer_type.csv") %>% janitor::clean_names()
raw_df = raw_df %>% mutate(
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
  
  health_cat = 
    case_when(
      hlthpln1  == 1  ~"yes",
      hlthpln1  == 2  ~"no",
      TRUE ~ NA_character_
    ),
  cencer_type = 
    case_when(
      cncrtyp1 == 1  ~"Non-Hispanic, White",
      cncrtyp1 == 2  ~"Non-Hispanic, Black",
      cncrtyp1 == 5  ~"Hispanic",
      TRUE ~ "Non-Hispanic, Other"
    )
)
raw_df$cancer = as.factor(raw_df$cancer)
raw_df$educa = as.factor(raw_df$educa)


cancer_type_df %>% 
  ggplot() +
  geom_bar(aes(x = reorder(value_label,weighted_percentage), y = weighted_percentage),stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(x = "Cancer Type", y = "Weighted Percentage")

filter(raw_df,cancer == "Yes")["cncrtyp1"] %>% table()
# number of replied cancer type 
filter(raw_df,!is.na(cncrtyp1)) %>% count()


##### ------------------------------------------
# svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + educa + alcday5_mod + smoke_mod, design = svy_obj_1, family = "binomial") 
# svyby(cancer ~ age_l + sex1 ,~x.state, svy_tbl_1, na.rm = TRUE, svyglm)

##### ----------Domain analysis for CANCER_TYPE --------------------------------
raw_df$cncrtyp1 %>% table()
# cancer_by_type_df = left_join(raw_df,cancer_type_df[1:2],  by = c("cncrtyp1" = "value"))


raw_df$cncrtyp1[raw_df$cncrtyp1 >= 77 | is.na(raw_df$cncrtyp1) ] = 0 
raw_df$cncrtyp1 %>% table()
raw_df$cncrtyp1 %>% class()
cancer_by_type_df = left_join_NA(raw_df,cancer_type_df[1:2],  by = c("cncrtyp1" = "value"))


cancer_by_type_df$value_label = as.character(cancer_by_type_df$value_label)
cancer_by_type_df$value_label = replace_na(cancer_by_type_df$value_label,"non") 
cancer_by_type_df$value_label %>% table()
cancer_by_type_df$value_label = factor(cancer_by_type_df$value_label)
cancer_by_type_df$value_label = relevel(cancer_by_type_df$value_label,"non")

cancer_by_type_df$x.llcpwt = as.numeric(cancer_by_type_df$x.llcpwt)
cancer_by_type_df$alcday5_mod= as.numeric(cancer_by_type_df$alcday5_mod)
svy_obj_cancer_type <- svydesign(id=~x.psu, weights=~x.llcpwt, data=cancer_by_type_df)
foo_1 = svyglm(value_label ~ age_l + sex1 +  x.imprace + sleptim1_cat + alcday5_mod + smoke_mod, design = svy_obj_cancer_type, family = "binomial")
summary(foo_1)


##### ----------Domain analysis for AGE --------------------------------
raw_df$age_l %>% table()
svy_tbl_1 <- svydesign(id=~x.psu, weights=~x.llcpwt, data=raw_df)

dsub = subset(svy_obj_1, age_l == "18-24")
foo_age_18 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_18)

dsub = subset(svy_obj_1, age_l == "25-34")
foo_age_25 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_25)

dsub = subset(svy_obj_1, age_l == "35-44")
foo_age_35 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_35)

dsub = subset(svy_obj_1, age_l == "45-54")
foo_age_45 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_45)

dsub = subset(svy_obj_1, age_l == "55-64")
foo_age_55 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_55)

dsub = subset(svy_obj_1, age_l == "65+")
foo_age_65 = svyglm(cancer ~  sex1 +  x.imprace + sleptim1_cat  + alcday5_mod + smoke_mod, design = dsub, family = "binomial") 
summary(foo_age_65)

# use sas for domain analysis 
# write.csv(raw_df,"data/brfss_cancer.csv")
glm_by_age = tibble(
  age = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  lower = c (
    confint(foo_age_18)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp(),
    confint(foo_age_25)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp(),
    confint(foo_age_35)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp(),
    confint(foo_age_45)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp(),
    confint(foo_age_55)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp(),
    confint(foo_age_65)["sleptim1_cat2 Adquate sleep", ]["2.5 %"] %>% exp()
  ),
  coef = c(
    coefficients(foo_age_18)["sleptim1_cat2 Adquate sleep"] %>% exp(),
    coefficients(foo_age_25)["sleptim1_cat2 Adquate sleep"] %>% exp(),
    coefficients(foo_age_35)["sleptim1_cat2 Adquate sleep"] %>% exp(),
    coefficients(foo_age_45)["sleptim1_cat2 Adquate sleep"] %>% exp(),
    coefficients(foo_age_55)["sleptim1_cat2 Adquate sleep"] %>% exp(),
    coefficients(foo_age_65)["sleptim1_cat2 Adquate sleep"] %>% exp()
  ),
  higher = c (
    confint(foo_age_18)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp(),
    confint(foo_age_25)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp(),
    confint(foo_age_35)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp(),
    confint(foo_age_45)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp(),
    confint(foo_age_55)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp(),
    confint(foo_age_65)["sleptim1_cat2 Adquate sleep", ]["97.5 %"] %>% exp()
  )
)

glm_by_age %>% 
  ggplot() +
  geom_point(aes(x=age,y = coef)) +
  geom_errorbar(mapping= aes(x=age, ymin=lower, ymax=higher)) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  labs(y = "OR of having cancer") +
  coord_flip()

