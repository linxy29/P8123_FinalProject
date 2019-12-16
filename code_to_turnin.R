# import data 
library(dplyr)
# Process raw data 

# Read raw data 
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
