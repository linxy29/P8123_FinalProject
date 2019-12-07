# table 2 
# logistric regression 

# setup complex survery design
library(tidyverse)
library(survey)
library(broom)
raw_df = read_csv("data/table_1_data.csv")

# filter out some rows 
tbl2_df = 
  raw_df %>% 
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

tbl2_df$educa[tbl2_df$educa == 9] = NA
tbl2_df = tbl2_df %>% filter(educa != 9)
tbl2_df$educa %>% table()


factor_cols <- c("x.age.g", "x.state", "x.imprace", "genhlth","hlthpln1","educa" ,"sex1","chcocncr", "sleptim1_cat")
tbl2_df[factor_cols] <- lapply(tbl2_df[factor_cols], as.factor)  

tbl2_df$chcocncr = relevel(tbl2_df$chcocncr, ref = 2)





svy_tbl_2 <- svydesign(id=~x.psu, weights=~x.llcpwt, data=tbl2_df)
glm_model_1 = svyglm(chcocncr~x.age.g + sex1 +  x.imprace + sleptim1_cat + educa, design = svy_tbl_2, family = "binomial") 
glm_model_1 %>% summary()
glm_model_1 %>% confint()

glm_model_2 = svyglm(chcocncr~x.age.g + sex1 +  x.imprace + sleptim1 + educa, design = svy_tbl_2, family = "binomial")
exp(tidy(glm_model_2)[-1])


confint(glm_model_2)



# ger from freqeuncy
get_freq(tbl2_df, svy_tbl_2, "sleptim1_cat")

get_freq(tbl2_df, svy_tbl_2, "x.imprace")
tbl2_df$x.imprace %>% table()

get_freq(tbl2_df, svy_tbl_2, "educa")

get_freq(tbl2_df, svy_tbl_2, "x.age.g")

get_freq(tbl2_df, svy_tbl_2, "sex1")
