# 11/30/2019
library(dplyr)
# Process raw data 

# Read raw data 
brfss2018_raw = readRDS(file = "data/brfss2018.rds")

# pick variable of interest
# to find variable of interest, refer to "code book" in data folder
# If you add more variable, do not remove existing variables.
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
  "WEIGHT2",
  "HEIGHT3",
  "SMOKE100",
  "ALCDAY5"))


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

fip_state_ref = read.csv("data/state_fip_code.csv")
processed_df$x.state = as.numeric(processed_df$x.state)
processed_df = left_join(processed_df, fip_state_ref,  by = c("x.state" = "state_code"))

# filter NA 
processed_df = filter(processed_df, cancer != "DK" )
# removed 1953 samples :  0.0044%
skimr::skim(processed_df)

# recode factor 
# factor_cols <- c("age_l","cancer","sex1", "x.state", "x.imprace", "genhlth","hlthpln1","educa" ,"chcocncr", "sleptim1_cat")
# tbl2_df[factor_cols] <- lapply(tbl2_df[factor_cols], as.factor)  
# tbl2_df$chcocncr = relevel(tbl2_df$chcocncr, ref = 2)

# save files
# saveRDS(processed_df, file="data/primary_df_1.rds")





# missing values 
# skimr::skim(brfss2018_raw[vars_of_interest])

####--------------------------------------------------------####
# Save customized file 


# for table 1 
table_1_df = brfss2018_raw[vars_of_interest]

# need to categorize sleep hours 
# table_1_df$sleptim1 %>% table()
# < 7 for insufficient
# 7-10 adquate 
# >10 over sleep
# base one National Sleep Foundation https://www.sciencedirect.com/science/article/pii/S2352721815000157
table_1_df = 
  table_1_df %>% 
  mutate(sleptim1_cat = case_when(
    sleptim1 < 7  ~"1 Insufficient sleep",
    sleptim1 >= 7 & sleptim1 <= 10 ~"2 Adquate sleep",
    sleptim1 > 10 & sleptim1 <= 24 ~"3 Excessive sleep",
    TRUE ~ NA_character_
  )
 )

table_1_df$sleptim1_cat %>% table()
# write.csv(table_1_df,"data/table_1_data.csv")
# saveRDS(table_1_df, file="table_1_df.rds")
