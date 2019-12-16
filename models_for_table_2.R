library(tidyverse)
library(survey)

#### process data 
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
    ),
  alcweek5_mod = alcday5_mod/7   # change data in to the "week" unit
)
raw_df$edu_cat_mod %>% table()
raw_df$cancer = as.factor(raw_df$cancer)
raw_df$educa = as.factor(raw_df$educa)
raw_df$x.incomg = as.factor(raw_df$x.incomg)
# remove education unknown
raw_df$educa[raw_df$educa == 9] = NA
raw_df$educa = factor(raw_df$educa)


#### Model part 

svy_obj_2 <- svydesign(id=~x.psu, weights= ~x.llcpwt, strata = ~x.state , data=raw_df, nest=TRUE)
glm_model_4 = svyglm(cancer ~ age_l + sex1 +  x.imprace + sleptim1_cat + alcweek5_mod + smoke_mod + health_cover + exercise_cat + x.incomg + edu_cat_mod , design = svy_obj_2, family = "binomial") 


confint_glm <- function(object, parm, level = 0.95, ...) {
  coef = coef(summary(object)) %>% as.data.frame()
  coef_CI = object %>% confint() %>% as.data.frame()
  table = cbind(coef, coef_CI) %>% 
    mutate(Exp.Est = round(exp(Estimate),4),
           CIL = round(`2.5 %`,4),
           CIU = round(`97.5 %`,4),
           Std.Error = round(`Std. Error`,4),
           Estimate = round(Estimate,4),
           p.value = round(`Pr(>|t|)`,4)) %>%
    dplyr::select(Estimate, Exp.Est, Std.Error, CIL, CIU, p.value)
  rownames(table) <- rownames(coef)
  return(table)
}
glm_result_1 = confint_glm(glm_model_4) 


#### add variable names
glm_result_1 = 
  glm_result_1 %>%
  rownames_to_column() %>%
  mutate(
    rownames_new =
      case_when(
        rowname  == "(Intercept)" ~ "Intercept",
        rowname  == "age_l25-34"  ~ "Age (25-34)",
        rowname  == "age_l35-44"  ~ "Age (35-44)",
        rowname  == "age_l45-54"  ~ "Age (45-54)",
        rowname  == "age_l55-64"  ~ "Age (55-64)",
        rowname  == "age_l65+"  ~   "Age (65+)",
        rowname  == "sex1Male"  ~   "Sex (Male)",
        rowname  == "x.impraceNon-Hispanic, Black"  ~ "Non-Hispanic, Black",
        rowname  == "x.impraceNon-Hispanic, Other"  ~ "Non-Hispanic, Other",
        rowname  == "x.impraceNon-Hispanic, White"  ~ "Non-Hispanic, White",
        rowname  == "sleptim1_cat2 Adquate sleep"   ~ "Sleep (Adequate)",
        rowname  == "sleptim1_cat3 Excessive sleep" ~ "Sleep (Excessive)",
        rowname  == "alcweek5_mod"      ~ "Weeks of drinks",
        rowname  == "smoke_modyes"     ~ "Smoke Status",
        rowname  == "health_coveryes"    ~ "Health Coverage",
        rowname  == "exercise_catyes"    ~ "Exercise Status",
        rowname  == "x.incomg2"  ~ "$15,000 ~ $25,000",
        rowname  == "x.incomg3"  ~ "$25,000 ~ $35,000",
        rowname  == "x.incomg4"  ~ "$35,000 ~ $50,000",
        rowname  == "x.incomg5"  ~ "More than $50,000 ",
        rowname  == "x.incomg9"  ~ "unknown",
        # rowname  == "educa2"  ~ "Elementary",
        # rowname  == "educa3"  ~ "Some high school",
        # rowname  == "educa4"  ~ "High school graduate",
        # rowname  == "educa5"  ~ "Some college",
        # rowname  == "educa6"  ~ "College graduate",
        rowname  == "edu_cat_mod HS graduate"  ~ "High School Graduate",
        rowname  == "edu_cat_mod Some College"  ~ "Some College",
        rowname  == "edu_cat_modCollege Graduate"  ~ "College Graduate",
        TRUE ~ NA_character_
      )
  ) %>%
  remove_rownames() %>%
  dplyr::select(-rowname) %>%
  column_to_rownames(var = "rownames_new")

#### print result 
glm_result_1 %>% knitr::kable()
