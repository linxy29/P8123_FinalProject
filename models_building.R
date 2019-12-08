library(tidyverse)
library(survey)
library(broom)
`%notin%` <- Negate(`%in%`)

theme_set(theme_classic(base_size = 16))
raw_df = read_rds("data/primary_df_1.rds")
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

raw_df$cncrtyp1 %>% table()

# raw_df$alcday5_mod %>% hist()
# raw_df$educa %>% table()
# raw_df$cancer_num %>% table()
# raw_df %>% group_by(cancer,cancer_num) %>% summarize(n=n())

colnames(raw_df)


svy_obj_1 <- svydesign(id=~x.psu, weights = ~x.llcpwt, data = raw_df)
svy_obj_2 <- svydesign(id=~x.psu, weights= ~x.llcpwt, strata = ~x.state , data=raw_df, nest=TRUE)

glm_model_1 = svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + educa + alcday5_mod + smoke_mod, design = svy_obj_1, family = "binomial") 
glm_model_1 %>% summary()
glm_model_1 %>% confint()

glm_model_2 = svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + alcday5_mod + smoke_mod, design = svy_obj_1, family = "binomial") 
glm_model_2 %>% summary()

glm_model_3 = svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + alcday5_mod + smoke_mod + health_cat + exercise_cat, design = svy_obj_1, family = "binomial") 
glm_model_3 %>% summary()
glm_model_3 %>% confint()
anova(glm_model_2,glm_model_3)
anova(glm_model_2,glm_model_1)


glm_model_3 %>% summary()
coefficients(glm_model_3) %>% exp() 
plot_df_1 = glm_model_3 %>% confint() %>% exp()

plot_df_1 = as.data.frame(plot_df_1)
plot_df_1$estimate = (coefficients(glm_model_3) %>% exp()) %>% as.vector()

plot_df_1$vars = rownames(plot_df_1)
colnames(plot_df_1) = c("lower", "upper", "OR", "vars")

plot_df_1 = 
  plot_df_1 %>% 
  mutate(
    vars_rename = 
      case_when(
        vars  == "(Intercept)" ~ "Inter",
        vars  == "age_l25-34"  ~ "Age (25-34)",
        vars  == "age_l35-44"  ~ "Age (35-44)",
        vars  == "age_l45-54"  ~ "Age (45-54)",
        vars  == "age_l55-64"  ~ "Age (55-64)",
        vars  == "age_l65+"  ~   "Age (65+)",
        vars  == "sex1Male"  ~   "Sex (Male)",
        vars  == "x.impraceNon-Hispanic, Black"  ~ "Non-Hispanic, Black",
        vars  == "x.impraceNon-Hispanic, Other"  ~ "Non-Hispanic, Other",
        vars  == "x.impraceNon-Hispanic, White"  ~ "Non-Hispanic, White",
        vars  == "sleptim1_cat2 Adquate sleep"   ~ "Sleep (Adequate)",
        vars  == "sleptim1_cat3 Excessive sleep" ~ "Sleep (Excessive)",
        vars  == "alcday5_mod"      ~ "# of drinks",
        vars  == "smoke_modyes"     ~ "Smoke Status",
        vars  == "health_catyes"    ~ "Health Status",
        vars  == "exercise_catyes"  ~ "Exercise",
        TRUE ~ NA_character_
      )
  )



ggplot(plot_df_1) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")

# over all plot 
ggplot(plot_df_1) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + 
  geom_hline(yintercept=1, linetype="dashed", color = "red")


# age 
ggplot(subset(plot_df_1, vars %in% c("age_l25-34","age_l35-44", "age_l45-54", "age_l55-64", "age_l65+"))) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")  +
  coord_flip() +
  xlab("Reference: Age (18-25)")

# Race 
ggplot(subset(plot_df_1, vars %in% c("x.impraceNon-Hispanic, Black","x.impraceNon-Hispanic, Other", "x.impraceNon-Hispanic, White"))) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")  +
  coord_flip() +
  xlab("Reference: Hispanic")

# Sleep
ggplot(subset(plot_df_1, vars %in% c("sleptim1_cat2 Adquate sleep","sleptim1_cat3 Excessive sleep"))) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")  +
  coord_flip() +
  xlab("Reference: Inadequate Sleep (< 7 Hours)")

# others 
not_age = subset(plot_df_1, vars %notin% c("(Intercept)","age_l25-34","age_l35-44", "age_l45-54", "age_l55-64", "age_l65+","x.impraceNon-Hispanic, Black","x.impraceNon-Hispanic, Other", "x.impraceNon-Hispanic, White","sleptim1_cat2 Adquate sleep","sleptim1_cat3 Excessive sleep")) 
not_age$vars_rename = factor(not_age$vars_rename, levels = not_age$vars_rename[order(not_age$OR)])

ggplot(not_age) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")  +
  coord_flip() +
  xlab("")

# Drinks 

ggplot(subset(plot_df_1, vars %in% c("alcday5_mod"))) +
  geom_point(aes(x=vars_rename,y = OR)) +
  geom_errorbar(mapping= aes(x=vars_rename, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")  +
  coord_flip() +
  ylim(0.99,1.01)


ggplot(subset(plot_df_1, vars %in% c("sex1Male","x.impraceNon-Hispanic, Black", "x.impraceNon-Hispanic, Other-54", "x.impraceNon-Hispanic, White"))) +
  geom_point(aes(x=vars,y = OR)) +
  geom_errorbar(mapping= aes(x=vars, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=1, linetype = "dashed", color = "red")


ggplot(subset(plot_df_1, vars %in% c("alcday5_mod","smoke_modyes", "health_catyes", "exercise_catyes"))) +
  geom_point(aes(x=vars,y = OR)) +
  geom_errorbar(mapping= aes(x=vars, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red")

ggplot(subset(plot_df_1, vars %in% c("alcday5_mod", "exercise_catyes"))) +
  geom_point(aes(x=vars,y = OR)) +
  geom_errorbar(mapping= aes(x=vars, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red")

ggplot(subset(plot_df_1, vars %in% c("sleptim1_cat2 Adquate sleep","sleptim1_cat3 Excessive sleep"))) +
  geom_point(aes(x=vars,y = OR)) +
  geom_errorbar(mapping= aes(x=vars, ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red")
