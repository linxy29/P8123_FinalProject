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
  "SMOKDAY2",
  "ALCDAY5",
  "AVEDRNK2"))


# dimemsion of data
dim(brfss2018_raw[vars_of_interest])
# nrow() = 437436 

processed_df= brfss2018_raw[vars_of_interest]
processed_df %>% 
  mutate( alcday5_mod  = 
  case_when(
    alcday5 <= 300 & alcday5 >= 200 ~  as.numeric(alcday5-200),
    alcday5 <= 200 & alcday5 >= 100 ~ as.numeric((alcday5-100) *4.0),
    alcday5 == 888 ~ as.numeric(0),
    TRUE ~ as.numeric(NA)
  )
  )



# saveRDS(processed_df, file="data/shared_df_1.rds")





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
