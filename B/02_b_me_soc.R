##############################################################################
# 02 MARGINAL EFFECTS PLOTS: STUDENTS OF COLOR
##############################################################################

###########################################################################
# FEDERAL
###########################################################################

# ESTABLISH MARGINAL EFFECTS BY GROUP

fed_margins_soc_25 <- margins(model_fed, 
                               data = df_rev_fed[df_rev_fed$dist_group_soc == "0-24%", ],
                               variables="schyear")

fed_margins_soc_50 <- margins(model_fed, 
                               data = df_rev_fed[df_rev_fed$dist_group_soc == "25-49%", ],
                               variables="schyear")

fed_margins_soc_75 <- margins(model_fed, 
                               data = df_rev_fed[df_rev_fed$dist_group_soc == "50-74%", ],
                               variables="schyear")

fed_margins_soc_100 <- margins(model_fed, 
                                data = df_rev_fed[df_rev_fed$dist_group_soc == "75-100%", ],
                                variables="schyear")

# GENERATE AME PLOT

fed_margins_plot_soc <- data.frame(soc_group=NA,
                                   ame_2015 = 0,
                                   ame_2016=NA,
                                   ame_2017=NA,
                                   ame_2018=NA,
                                   ame_2019=NA,
                                   ame_2020=NA,
                                   ame_2021=NA)

fed_margins_plot_soc <- fed_margins_plot_soc%>%
  tibble::add_row(soc_group=1,
                  ame_2015 = 0,
                  ame_2016=mean(fed_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(fed_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(fed_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(fed_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(fed_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(fed_margins_soc_25$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=2,
                  ame_2015 = 0,
                  ame_2016=mean(fed_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(fed_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(fed_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(fed_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(fed_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(fed_margins_soc_50$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=3,
                  ame_2015 = 0,
                  ame_2016=mean(fed_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(fed_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(fed_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(fed_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(fed_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(fed_margins_soc_75$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=4,
                  ame_2015 = 0,
                  ame_2016=mean(fed_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(fed_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(fed_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(fed_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(fed_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(fed_margins_soc_100$dydx_schyear2021, na.rm = TRUE))%>%
  drop_na()

fed_margins_plot_soc_long <- fed_margins_plot_soc %>%
  pivot_longer(cols = starts_with("ame_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "ame_2015" ~ "2015",
                             schyear == "ame_2016" ~ "2016",
                             schyear == "ame_2017" ~ "2017",
                             schyear == "ame_2018" ~ "2018",
                             schyear == "ame_2019" ~ "2019",
                             schyear == "ame_2020" ~ "2020",
                             schyear == "ame_2021" ~ "2021")
  )

ggplot(fed_margins_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="Federal Revenue Amount Per Student",
       subtitle = "Marginal effects including district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))


# GENERATE AME + 2015 ADJUSTED DOLLARS

fed_adj_plot_soc <- data.frame(soc_group=NA,
                           adj_2015 = NA,
                           adj_2016 = NA,
                           adj_2017 = NA,
                           adj_2018 = NA,
                           adj_2019 = NA,
                           adj_2020 = NA,
                           adj_2021 = NA)

fed_adj_plot_soc <- fed_adj_plot_soc%>%
  tibble::add_row(soc_group=1,
                  adj_2015 = mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015]),
                  adj_2016=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(fed_margins_soc_25$rev_pp_18d[fed_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_25$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=2,
                  adj_2015 = mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015]),
                  adj_2016=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(fed_margins_soc_50$rev_pp_18d[fed_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_50$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=3,
                  adj_2015 = mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015]),
                  adj_2016=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(fed_margins_soc_75$rev_pp_18d[fed_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_75$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=4,
                  adj_2015 = mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015]),
                  adj_2016=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(fed_margins_soc_100$rev_pp_18d[fed_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(fed_margins_soc_100$dydx_schyear2021, na.rm = TRUE)
  )%>%
  drop_na()

fed_adj_plot_soc_long <- fed_adj_plot_soc %>%
  pivot_longer(cols = starts_with("adj_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "adj_2015" ~ "2015",
                             schyear == "adj_2016" ~ "2016",
                             schyear == "adj_2017" ~ "2017",
                             schyear == "adj_2018" ~ "2018",
                             schyear == "adj_2019" ~ "2019",
                             schyear == "adj_2020" ~ "2020",
                             schyear == "adj_2021" ~ "2021")
  )

ggplot(fed_adj_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="Federal Revenue Amount Per Student",
       subtitle = "Funding amounts adjusted for district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))


#df_rev_fed$rev_adjusted <- predict.lm(model_fed, newdata=df_rev_fed)

###########################################################################
# STATE
###########################################################################

# ESTABLISH MARGINAL EFFECTS BY GROUP

state_margins_soc_25 <- margins(model_state, 
                                 data = df_rev_state[df_rev_state$dist_group_soc == "0-24%", ],
                                 variables="schyear")

state_margins_soc_50 <- margins(model_state, 
                                 data = df_rev_state[df_rev_state$dist_group_soc == "25-49%", ],
                                 variables="schyear")

state_margins_soc_75 <- margins(model_state, 
                                 data = df_rev_state[df_rev_state$dist_group_soc == "50-74%", ],
                                 variables="schyear")

state_margins_soc_100 <- margins(model_state, 
                                  data = df_rev_state[df_rev_state$dist_group_soc == "75-100%", ],
                                  variables="schyear")

# GENERATE AME PLOT

state_margins_plot_soc <- data.frame(soc_group=NA,
                                 ame_2015 = 0,
                                 ame_2016=NA,
                                 ame_2017=NA,
                                 ame_2018=NA,
                                 ame_2019=NA,
                                 ame_2020=NA,
                                 ame_2021=NA)

state_margins_plot_soc <- state_margins_plot_soc%>%
  tibble::add_row(soc_group=1,
                  ame_2015 = 0,
                  ame_2016=mean(state_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(state_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(state_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(state_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(state_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(state_margins_soc_25$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=2,
                  ame_2015 = 0,
                  ame_2016=mean(state_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(state_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(state_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(state_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(state_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(state_margins_soc_50$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=3,
                  ame_2015 = 0,
                  ame_2016=mean(state_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(state_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(state_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(state_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(state_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(state_margins_soc_75$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=4,
                  ame_2015 = 0,
                  ame_2016=mean(state_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(state_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(state_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(state_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(state_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(state_margins_soc_100$dydx_schyear2021, na.rm = TRUE))%>%
  drop_na()

state_margins_plot_soc_long <- state_margins_plot_soc %>%
  pivot_longer(cols = starts_with("ame_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "ame_2015" ~ "2015",
                             schyear == "ame_2016" ~ "2016",
                             schyear == "ame_2017" ~ "2017",
                             schyear == "ame_2018" ~ "2018",
                             schyear == "ame_2019" ~ "2019",
                             schyear == "ame_2020" ~ "2020",
                             schyear == "ame_2021" ~ "2021")
  )

ggplot(state_margins_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="State Revenue Amount Per Student",
       subtitle = "Marginal effects including district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))


# GENERATE AME + 2015 ADJUSTED DOLLARS

state_adj_plot_soc <- data.frame(soc_group=NA,
                             adj_2015 = NA,
                             adj_2016 = NA,
                             adj_2017 = NA,
                             adj_2018 = NA,
                             adj_2019 = NA,
                             adj_2020 = NA,
                             adj_2021 = NA)

state_adj_plot_soc <- state_adj_plot_soc%>%
  tibble::add_row(soc_group=1,
                  adj_2015 = mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015]),
                  adj_2016=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(state_margins_soc_25$rev_pp_18d[state_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_25$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=2,
                  adj_2015 = mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015]),
                  adj_2016=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(state_margins_soc_50$rev_pp_18d[state_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_50$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=3,
                  adj_2015 = mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015]),
                  adj_2016=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(state_margins_soc_75$rev_pp_18d[state_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_75$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=4,
                  adj_2015 = mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015]),
                  adj_2016=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(state_margins_soc_100$rev_pp_18d[state_margins_soc_100$schyear==2015], na.rm=TRUE) +
                    mean(state_margins_soc_100$dydx_schyear2021, na.rm = TRUE)
  )%>%
  drop_na()

state_adj_plot_soc_long <- state_adj_plot_soc %>%
  pivot_longer(cols = starts_with("adj_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "adj_2015" ~ "2015",
                             schyear == "adj_2016" ~ "2016",
                             schyear == "adj_2017" ~ "2017",
                             schyear == "adj_2018" ~ "2018",
                             schyear == "adj_2019" ~ "2019",
                             schyear == "adj_2020" ~ "2020",
                             schyear == "adj_2021" ~ "2021")
  )

ggplot(state_adj_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="State Revenue Amount Per Student",
       subtitle = "Funding amounts adjusted for district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))


#df_rev_state$rev_adjusted <- predict.lm(model_state, newdata=df_rev_state)

###########################################################################
# LOCAL
###########################################################################

local_margins_soc_25 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_soc == "0-24%", ],
                                 variables="schyear")

local_margins_soc_50 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_soc == "25-49%", ],
                                 variables="schyear")

local_margins_soc_75 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_soc == "50-74%", ],
                                 variables="schyear")

local_margins_soc_100 <- margins(model_local, 
                                  data = df_rev_local[df_rev_local$dist_group_soc == "75-100%", ],
                                  variables="schyear")

local_margins_plot_soc <- data.frame(soc_group=NA,
                                 ame_2015 = 0,
                                 ame_2016=NA,
                                 ame_2017=NA,
                                 ame_2018=NA,
                                 ame_2019=NA,
                                 ame_2020=NA,
                                 ame_2021=NA)

local_margins_plot_soc <- local_margins_plot_soc%>%
  tibble::add_row(soc_group=1,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_soc_25$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=2,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_soc_50$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=3,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_soc_75$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(soc_group=4,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_soc_100$dydx_schyear2021, na.rm = TRUE))%>%
  drop_na()

local_margins_plot_soc_long <- local_margins_plot_soc %>%
  pivot_longer(cols = starts_with("ame_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "ame_2015" ~ "2015",
                             schyear == "ame_2016" ~ "2016",
                             schyear == "ame_2017" ~ "2017",
                             schyear == "ame_2018" ~ "2018",
                             schyear == "ame_2019" ~ "2019",
                             schyear == "ame_2020" ~ "2020",
                             schyear == "ame_2021" ~ "2021")
  )

ggplot(local_margins_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="Local Revenue Amount Per Student",
       subtitle = "Marginal effects including district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))

####

local_adj_plot_soc <- data.frame(soc_group=NA,
                             adj_2015 = NA,
                             adj_2016 = NA,
                             adj_2017 = NA,
                             adj_2018 = NA,
                             adj_2019 = NA,
                             adj_2020 = NA,
                             adj_2021 = NA)

local_adj_plot_soc <- local_adj_plot_soc%>%
  tibble::add_row(soc_group=1,
                  adj_2015 = mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015]),
                  adj_2016=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_soc_25$rev_pp_18d[local_margins_soc_25$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_25$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=2,
                  adj_2015 = mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015]),
                  adj_2016=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_soc_50$rev_pp_18d[local_margins_soc_50$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_50$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=3,
                  adj_2015 = mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015]),
                  adj_2016=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_soc_75$rev_pp_18d[local_margins_soc_75$schyear==2015], na.rm=TRUE) +
                    mean(local_margins_soc_75$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(soc_group=4,
                  adj_2015 = mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2015]),
                  adj_2016=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2016], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2017], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2018], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2019], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2020], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_soc_100$rev_pp_18d[local_margins_soc_100$schyear==2021], na.rm=TRUE) +
                    mean(local_margins_soc_100$dydx_schyear2021, na.rm = TRUE)
  )%>%
  drop_na()

local_adj_plot_soc_long <- local_adj_plot_soc %>%
  pivot_longer(cols = starts_with("adj_"),
               names_to = "schyear")%>%
  mutate(soc_group=factor(soc_group))%>%
  mutate(schyear = case_when(schyear == "adj_2015" ~ "2015",
                             schyear == "adj_2016" ~ "2016",
                             schyear == "adj_2017" ~ "2017",
                             schyear == "adj_2018" ~ "2018",
                             schyear == "adj_2019" ~ "2019",
                             schyear == "adj_2020" ~ "2020",
                             schyear == "adj_2021" ~ "2021")
  )

ggplot(local_adj_plot_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="Local Revenue Amount Per Student",
       subtitle = "Funding amounts adjusted for district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))

##############################################################################
# STATE AND LOCAL
##############################################################################

statelocal_adj_soc_long <- bind_rows(state_adj_plot_soc_long, 
                                 local_adj_plot_soc_long)%>%
  mutate(soc_group = factor(soc_group))%>%
  group_by(soc_group, schyear) %>%
  summarise_all(funs(sum))

ggplot(statelocal_adj_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="State and Local Combined Revenue Amount Per Student",
       subtitle = "Funding amounts adjusted for district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))

##############################################################################
# FEDERAL, STATE, AND LOCAL
##############################################################################

fedstatelocal_adj_soc_long <- bind_rows(fed_adj_plot_soc_long,
                                    state_adj_plot_soc_long, 
                                    local_adj_plot_soc_long)%>%
  mutate(soc_group = factor(soc_group))%>%
  group_by(soc_group, schyear) %>%
  summarise_all(funs(sum))

ggplot(fedstatelocal_adj_soc_long, 
       aes(x=schyear, y=value, group=soc_group, color=soc_group)) +
  geom_point()+
  geom_line()+
  labs(title="Federal, State, and Local Combined Revenue Amount Per Student",
       subtitle = "Funding amounts adjusted for district size, urbanicity/locale, 
       and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Students of color district percentages are calculated\nby percentage of students who do not identify as White only
       \n\nData Source: F-196",
       color="Student of Color\nDistrict Percentage")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  theme(legend.title = element_text(size=10))+
  scale_color_manual(labels = c("0-24%", 
                                "25-49%",
                                "50-74%", 
                                "75-100%"), 
                     values=c("#dabddb", "#af8dc3", "#762a83", "#4b2e83"))
