# maps 
library(tidyverse)
library(maps)
library(ggthemes)
library(rasterVis)
library(viridis)
# library(tigris)
# data(fips_codes)

# brfss2018_raw = readRDS(file = "data/brfss2018.rds")
brfss2018_raw = readRDS(file = "data/primary_df_1.rds")
brfss2018_map = brfss2018_raw[c("x.state","sleptim1")]
brfss2018_map$x.state %>% table()

brfss2018_raw$cancer %>% table()

brfss2018_map = 
  brfss2018_map %>% mutate(
  region = plyr::mapvalues(brfss2018_map$x.state,
                    from = state.fips$fips,
                    to = state.fips$polyname)
)

# brfss2018_map$state = recode(brfss2018_map$state, '2'='alaska', '15'='hawaii','66'='guam','72'='Puerto Rico ')


# ********* set up fips reference code ********* #
# fip_state_ref = fips_codes[,c("state_code","state_name")] %>% unique()
# fip_state_ref$state_code = as.numeric(fip_state_ref$state_code)
# fip_state_ref$state_name = tolower(fip_state_ref$state_name)
# write_csv(fip_state_ref,"data/state_fip_code.csv")

# fip_state_ref_2 = fips_codes[,c("state_code","state_name","state")] %>% unique()
# fip_state_ref_2$state_code = as.numeric(fip_state_ref_2$state_code)
# fip_state_ref_2$state_name = tolower(fip_state_ref_2$state_name)
# write_csv(fip_state_ref_2,"data/state_fip_code_2.csv")

#####***** load subpopulation analysis from shu_data_explore file.
svy_tbl_1 <- svydesign(id=~x.psu, weights=~x.llcpwt, data=raw_df)
svymean(~ sleptim1, svy_tbl_1, na.rm = TRUE) 
svymean(sleptim1~x.state , svy_tbl_1, na.rm = TRUE) 


# subpopulation analysis 

# sleep by states
sleep_by_states = svyby(~ sleptim1,~x.state, svy_tbl_1, na.rm = TRUE, svymean)
sleep_by_states$x.state = rownames(sleep_by_states) %>% as.numeric()
summary(sleep_by_states$sleptim1)







fip_state_ref = read.csv("data/state_fip_code.csv")
fip_state_ref_2 = read.csv("data/state_fip_code_2.csv") # with state Abbrv
sleep_by_states$x.state = as.numeric(sleep_by_states$x.state)
state_sleep_data = left_join(sleep_by_states[1:2],fip_state_ref,  by = c("x.state" = "state_code"))

arrange(state_sleep_data,sleptim1)



# start map plot.
us_states <- map_data("state")
us_states_names <- us_states %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))
us_states_names = left_join(us_states_names,fip_state_ref_2, by = c("region"="state_name"))
us_states_sleep <- left_join(us_states, state_sleep_data, by = c("region"="state_name"))

p <- ggplot(data = us_states_sleep,
            aes(
              x = long,
              y = lat,
              group = group,
              fill = sleptim1
            )) + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  labs(title = "Sleep Hours by States 2018",fill = "   Average \n Sleep Hour") +
  theme(legend.position = "right") +
  geom_text(
    data = us_states_names,
    aes(x = long, y = lat, label = state, fill = NULL),
    color = "White",
    size = 3,
    inherit.aes = FALSE
  ) 
 

# p +  scale_fill_viridis(discrete=FALSE,direction = 1, option="A") 


# national cancer mean 
svymean(~ cancer, svy_tbl_1, na.rm = TRUE) 

# cancer by states
cancer_by_states = svyby(~cancer,~x.state, svy_tbl_1, na.rm = TRUE, svymean)
cancer_by_states$x.state = rownames(sleep_by_states) %>% as.numeric()

# cancer by states 
cancer_by_states$x.state = as.numeric(cancer_by_states$x.state)
state_cancer_data = left_join(cancer_by_states[,c(1,3)],fip_state_ref,  by = c("x.state" = "state_code"))

arrange(state_cancer_data,cancerYes)


# cancer risk by states 
us_states_cancer <- left_join(us_states, state_cancer_data, by = c("region"="state_name"))
p <- ggplot(data = us_states_cancer,
            aes(x = long, y = lat,
                group = group, fill = cancerYes))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  labs(title = "Cancer by States 2018",fill = "Percentage \n  Cancer") +
  theme(legend.position = "right") +
  geom_text(
    data = us_states_names,
    aes(x = long, y = lat, label = state, fill = NULL),
    color = "White",
    size = 3,
    inherit.aes = FALSE
  ) 
  # + scale_fill_continuous(type = "viridis",trans = 'reverse')

## check significants 


    
  
