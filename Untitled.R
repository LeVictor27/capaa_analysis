
##### LOCAL
df_rev_local <- df_rev_merge%>%
  filter(rev_source=="Local")

model_local <- lm(rev_pp_18d ~ log(TotalEnrollment) + LOCALE + PctSpecialEducation + 
                    PctTransitionalBilingual + PctFreeorReducedPricedMeals + schyear + 
                    (PctFreeorReducedPricedMeals*schyear), data=df_rev_local)


local_margins_frpl_25 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_frpl == 1, ],
                                 variables="schyear")

local_margins_frpl_50 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_frpl == 2, ],
                                 variables="schyear")

local_margins_frpl_75 <- margins(model_local, 
                                 data = df_rev_local[df_rev_local$dist_group_frpl == 3, ],
                                 variables="schyear")

local_margins_frpl_100 <- margins(model_local, 
                                  data = df_rev_local[df_rev_local$dist_group_frpl == 4, ],
                                  variables="schyear")

local_margins_plot <- data.frame(frpl_group=NA,
                                 ame_2015 = 0,
                                 ame_2016=NA,
                                 ame_2017=NA,
                                 ame_2018=NA,
                                 ame_2019=NA,
                                 ame_2020=NA,
                                 ame_2021=NA)

local_margins_plot <- local_margins_plot%>%
  tibble::add_row(frpl_group=1,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_frpl_25$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_frpl_25$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_frpl_25$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_frpl_25$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_frpl_25$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_frpl_25$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(frpl_group=2,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_frpl_50$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_frpl_50$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_frpl_50$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_frpl_50$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_frpl_50$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_frpl_50$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(frpl_group=3,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_frpl_75$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_frpl_75$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_frpl_75$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_frpl_75$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_frpl_75$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_frpl_75$dydx_schyear2021, na.rm = TRUE))%>%
  tibble::add_row(frpl_group=4,
                  ame_2015 = 0,
                  ame_2016=mean(local_margins_frpl_100$dydx_schyear2016, na.rm = TRUE),
                  ame_2017=mean(local_margins_frpl_100$dydx_schyear2017, na.rm = TRUE),
                  ame_2018=mean(local_margins_frpl_100$dydx_schyear2018, na.rm = TRUE),
                  ame_2019=mean(local_margins_frpl_100$dydx_schyear2019, na.rm = TRUE),
                  ame_2020=mean(local_margins_frpl_100$dydx_schyear2020, na.rm = TRUE),
                  ame_2021=mean(local_margins_frpl_100$dydx_schyear2021, na.rm = TRUE))%>%
  drop_na()

local_margins_plot_long <- local_margins_plot %>%
  pivot_longer(cols = starts_with("ame_"),
               names_to = "schyear")%>%
  mutate(frpl_group=factor(frpl_group))

ggplot(local_margins_plot_long, 
       aes(x=schyear, y=value, group=frpl_group, color=frpl_group)) +
  geom_point()+
  geom_line()

####

local_adj_plot <- data.frame(frpl_group=NA,
                             adj_2015 = NA,
                             adj_2016 = NA,
                             adj_2017 = NA,
                             adj_2018 = NA,
                             adj_2019 = NA,
                             adj_2020 = NA,
                             adj_2021 = NA)

local_adj_plot <- local_adj_plot%>%
  tibble::add_row(frpl_group=1,
                  adj_2015 = mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2015]),
                  adj_2016=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2016], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2017], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2018], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2019], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2020], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_frpl_25$rev_pp_18d[local_margins_frpl_25$schyear==2021], na.rm=TRUE) +
                    mean(local_margins_frpl_25$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(frpl_group=2,
                  adj_2015 = mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2015]),
                  adj_2016=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2016], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2017], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2018], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2019], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2020], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_frpl_50$rev_pp_18d[local_margins_frpl_50$schyear==2021], na.rm=TRUE) +
                    mean(local_margins_frpl_50$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(frpl_group=3,
                  adj_2015 = mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2015]),
                  adj_2016=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2016], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2017], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2018], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2019], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2020], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_frpl_75$rev_pp_18d[local_margins_frpl_75$schyear==2021], na.rm=TRUE) +
                    mean(local_margins_frpl_75$dydx_schyear2021, na.rm = TRUE)
  )%>%
  tibble::add_row(frpl_group=4,
                  adj_2015 = mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2015]),
                  adj_2016=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2016], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2016, na.rm = TRUE),
                  adj_2017=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2017], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2017, na.rm = TRUE),
                  adj_2018=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2018], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2018, na.rm = TRUE),
                  adj_2019=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2019], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2019, na.rm = TRUE),
                  adj_2020=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2020], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2020, na.rm = TRUE),
                  adj_2021=mean(local_margins_frpl_100$rev_pp_18d[local_margins_frpl_100$schyear==2021], na.rm=TRUE) +
                    mean(local_margins_frpl_100$dydx_schyear2021, na.rm = TRUE)
  )%>%
  drop_na()

local_adj_plot_long <- local_adj_plot %>%
  pivot_longer(cols = starts_with("adj_"),
               names_to = "schyear")%>%
  mutate(frpl_group=factor(frpl_group))

ggplot(local_adj_plot_long, 
       aes(x=schyear, y=value, group=frpl_group, color=frpl_group)) +
  geom_point()+
  geom_line()

##### STATE AND LOCAL

statelocal_adj_long <- bind_rows(state_adj_plot_long, 
                                 local_adj_plot_long)%>%
  mutate(frpl_group = factor(frpl_group))%>%
  group_by(frpl_group, schyear) %>%
  summarise_all(funs(sum))

ggplot(statelocal_adj_long, 
       aes(x=schyear, y=value, group=frpl_group, color=frpl_group)) +
  geom_point()+
  geom_line()
