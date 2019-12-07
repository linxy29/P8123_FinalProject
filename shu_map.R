# maps 
library(tidyverse)
library(maps)
library(ggthemes)
# library(tigris)
# data(fips_codes)

brfss2018_raw = readRDS(file = "data/brfss2018.rds")
brfss2018_map = brfss2018_raw[c("x.state","sleptim1")]
brfss2018_map$x.state %>% table()

brfss2018_map = 
  brfss2018_map %>% mutate(
  region = plyr::mapvalues(brfss2018_map$x.state,
                    from = state.fips$fips,
                    to = state.fips$polyname)
)

# brfss2018_map$state = recode(brfss2018_map$state, '2'='alaska', '15'='hawaii','66'='guam','72'='Puerto Rico ')

brfss2018_map$state %>% unique()
brfss2018_map$state %>% table()

# ********* set up fips reference code ********* #
# fip_state_ref = fips_codes[,c("state_code","state_name")] %>% unique()
# fip_state_ref$state_code = as.numeric(fip_state_ref$state_code)
# fip_state_ref$state_name = tolower(fip_state_ref$state_name)
# write_csv(fip_state_ref,"data/state_fip_code.csv")

#####***** load subpopulation analysis from shu_data_explore file.
fip_state_ref = read.csv("data/state_fip_code.csv")
sleep_by_states$x.state = as.numeric(sleep_by_states$x.state)
state_sleep_data = left_join(sleep_by_states[1:2],fip_state_ref,  by = c("x.state" = "state_code"))

# cancer by states 
cancer_by_states$x.state = as.numeric(cancer_by_states$x.state)
state_cancer_data = left_join(cancer_by_states[1:2],fip_state_ref,  by = c("x.state" = "state_code"))



# start map plot.
us_states <- map_data("state")

us_states_sleep <- left_join(us_states, state_sleep_data, by = c("region"="state_name"))
p <- ggplot(data = us_states_sleep,
            aes(x = long, y = lat,
                group = group, fill = sleptim1))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  labs(title = "Sleep Hours by States 2018",fill = "   Average \n Sleep Hour") +
  theme(legend.position = "right") +
  scale_fill_continuous(type = "viridis",trans = 'reverse')
  

# cancer risk by states 
us_states_cancer <- left_join(us_states, state_cancer_data, by = c("region"="state_name"))
p <- ggplot(data = us_states_cancer,
            aes(x = long, y = lat,
                group = group, fill = chcocncr1))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  labs(title = "Cancer by States 2018",fill = "   ") +
  theme(legend.position = "right") +
  scale_fill_continuous(type = "viridis",trans = 'reverse')

## check significants 


    
  
