###########################################################################
# 05 SPATIAL DISTRIBUTION OF SCHOOL FUNDING
###########################################################################

###########################################################################
# SET UP AND DATA CLEAN
###########################################################################

library(leaidr)
library(sf)

wa <- lea_get("wa")%>% 
  sf::st_as_sf() 

df_rev_map <- df_rev_merge%>%
  filter(rev_source!="Other")%>%
  filter(schyear!="2021")%>%
  filter(schyear!="2015")%>%
  select(coudis, schyear, rev_source, rev_pp_18d)

df_rev_map <- df_rev_map%>%
  group_by(coudis, schyear, rev_source)%>%
  summarise(rev_pp_18d_sum = sum(rev_pp_18d))

df_rev_map_wide <- df_rev_map %>%
  pivot_wider(names_from = rev_source, values_from = rev_pp_18d_sum)

df_rev_map_merge <- df_rev_map_wide%>%
  rename(rev_fed_pp_18d = Federal,
         rev_st_pp_18d = State,
         rev_loc_pp_18d = Local)%>%
  mutate(rev_total_pp_18d = rev_fed_pp_18d + rev_st_pp_18d + rev_loc_pp_18d)%>%
  mutate(schyear_n = as.numeric(schyear),
         post_mcl_reform = ifelse(schyear>= 2018, "1", "0"))

df_rev_map_merge <- df_rev_map_merge%>%
  group_by(schyear)%>%
  mutate(
    rev_fed_mean = mean(rev_fed_pp_18d, na.rm = TRUE),
    rev_st_mean = mean(rev_st_pp_18d, na.rm = TRUE), 
    rev_loc_mean = mean(rev_loc_pp_18d, na.rm = TRUE),
    rev_total_mean = mean(rev_total_pp_18d, na.rm = TRUE))

df_rev_map_merge <- df_rev_map_merge%>%
  group_by(coudis, post_mcl_reform)%>%
  mutate(rev_fed_mean_reform = mean(rev_fed_pp_18d, na.rm = TRUE), 
         rev_st_mean_reform = mean(rev_st_pp_18d, na.rm = TRUE), 
         rev_loc_mean_reform = mean(rev_loc_pp_18d, na.rm = TRUE),
         rev_total_mean_reform = mean(rev_total_pp_18d, na.rm = TRUE))

df_rev_map_merge <- df_rev_map_merge%>%
  mutate(
    rev_fed_diff= (rev_fed_pp_18d-rev_fed_mean),
    rev_st_diff= (rev_st_pp_18d-rev_st_mean),
    rev_loc_diff=(rev_loc_pp_18d-rev_loc_mean),
    rev_total_diff=(rev_total_pp_18d-rev_total_mean))

df_rev_map_merge <- full_join(df_rev_map_merge, df_leaid, by=c("coudis"="coudis_s"))

df_rev_map_merge <- df_rev_map_merge%>%
  mutate(leaid=as.character(leaid))

mccleary_revenue <- collap(df_rev_map_merge, rev_fed_mean_reform + rev_st_mean_reform + rev_loc_mean_reform + rev_total_mean_reform~ leaid + post_mcl_reform, FUN = list(fmean))

mccleary_rev_wide <- mccleary_revenue%>%
  group_by(leaid)%>% 
  pivot_wider(names_from = post_mcl_reform, 
              values_from = c(rev_fed_mean_reform, rev_st_mean_reform, rev_loc_mean_reform, rev_total_mean_reform))

mccleary_rev_wide <- mccleary_rev_wide%>%
  mutate(
    rev_fed_mean_reform_diff = (rev_fed_mean_reform_1 - rev_fed_mean_reform_0),
    rev_st_mean_reform_diff = (rev_st_mean_reform_1 - rev_st_mean_reform_0),
    rev_loc_mean_reform_diff = (rev_loc_mean_reform_1 - rev_loc_mean_reform_0),
    rev_total_mean_reform_diff = (rev_total_mean_reform_1 - rev_total_mean_reform_0))

wa_rev_map_merge <- left_join(df_rev_map_merge, wa, by = c("leaid" = "GEOID"), copy=TRUE) 

wa_mccleary_rev_wide <- left_join(mccleary_rev_wide, wa, by = c("leaid" = "GEOID"), copy=TRUE) 

#What percentage higher than lower than state average for the state/local revenue
#Distribution of dollars -> district that serve large % of SOC/low-income

#Merge dist map

wa_dist_map <- full_join(df_demos_dist, df_leaid, by=c("coudis_s"="coudis_s"))

wa_dist_map$leaid <- as.character(wa_dist_map$leaid)

wa_dist_map <- left_join(wa_dist_map, wa, by = c("leaid" = "GEOID"), copy=TRUE)

###########################################################################
# DEMOGRAPHIC MAPS
###########################################################################

wa_dist_map %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = PctHispanic), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="Percentage") +
  labs(title="Latinx Students Percentage of Total Student Enrollment, 2018-2019 ")+
  theme_uw_map()

wa_dist_map %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = PctBlack), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="Percentage") +
  labs(title="Black Students Percentage of Total Student Enrollment, 2018-2019 ")+
  theme_uw_map()

wa_dist_map %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = PctAmericanIndianOrAlaskan), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="Percentage") +
  labs(title="Indigenous Students Percentage of Total Student Enrollment, 2018-2019 ")+
  theme_uw_map()

wa_dist_map %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = PctAsian), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="Percentage") +
  labs(title="Asian Students Percentage of Total Student Enrollment, 2018-2019 ")+
  theme_uw_map()

wa_dist_map %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = PctMultirac), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="Percentage") +
  labs(title="Multiracial Students Percentage \nof Total Student Enrollment, 2018-2019")+
  theme_uw_map()

###########################################################################
# REVENUE MAPS
###########################################################################

### ADD REGIONALIZATION FACTOR AND CWI NEXT TO EACH OTHER
### ADD COMMUTING ZONES ON THE MAP
### OLD CWI (LABOR MARKET VARIABLE), perfectly links to school districts, boundaries of districts and labor markets

#DRAW SAME GRAPH USING TERCILES 

wa_df196$rfactor_cis_2019 <- as.factor(wa_df196$rfactor_cis_2019)

wa_df196 %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rfactor_cis_2019), size = .25) +
  labs(title="Regionalization Factor, 2019")+
  theme_uw_map()

#Need to figure out what colors work best with this map to make it pop

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_fed_pp_18d), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Federal Revenue Per Student, 2018-2019")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_st_pp_18d), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="State Revenue Per Student, 2018-2019")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_loc_pp_18d), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Local  Revenue Per Student, 2018-2019")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_total_pp_18d), size = .25) +
  scale_fill_gradient2(low="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Total Revenue Per Student, 2018-2019")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_fed_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Federal Revenue Per Student, 2018-2019",
       subtitle = "Dollar Difference Against State Mean for the School Year")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_st_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="State Revenue Per Pupil, 2018-2019",
       subtitle = "Dollar Difference Against State Mean for the School Year")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_loc_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Local Revenue Per Pupil, 2018-2019",
       subtitle = "Dollar Difference Against State Mean for the School Year")+
  theme_uw_map()

wa_rev_map_merge %>%
  sf::st_as_sf() %>% 
  filter(schyear==2019)%>%
  ggplot() +
  geom_sf(aes(fill = rev_total_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Total Revenue Per Pupil, 2018-2019",
       subtitle = "Dollar Difference Against State Mean for the School Year")+
  theme_uw_map()

###########################################################################
# MCCLEARY MAPS
###########################################################################

wa_mccleary_rev_wide %>%
  sf::st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = rev_fed_mean_reform_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Federal Revenue Difference Post-McCleary Reform",
       subtitle = "Between 2016/2017/2018 (3 Year Average) and 2019/2020 (2 Year Average)")+
  theme_uw_map()

wa_mccleary_rev_wide %>%
  sf::st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = rev_st_mean_reform_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="State Revenue Difference Post-McCleary Reform",
       subtitle = "Between 2016/2017/2018 (3 Year Average) and 2019/2020 (2 Year Average)")+
  theme_uw_map()

wa_mccleary_rev_wide %>%
  sf::st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = rev_loc_mean_reform_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Local Revenue Difference Post-McCleary Reform",
       subtitle = "Between 2016/2017/2018 (3 Year Average) and 2019/2020 (2 Year Average)")+
  theme_uw_map()

wa_mccleary_rev_wide %>%
  sf::st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = rev_total_mean_reform_diff), size = .25) +
  scale_fill_gradient2(low="#FF0000", midpoint = 0, mid="#FFFFFF", high="#4b2e83", name="2018 Dollars") +
  labs(title="Total Revenue Difference Post-McCleary Reform",
       subtitle = "Between 2016/2017/2018 (3 Year Average) and 2019/2020 (2 Year Average)")+
  theme_uw_map()

# Difference is calculated as % change in revenue per pupil over 3 year period 
# (2015-16, 2016-17, 2017-18, 3 year average) to difference of the two year average (2018-19 and 19-2020), 
# divided by average of pre-McCleary as base to get percentage change
