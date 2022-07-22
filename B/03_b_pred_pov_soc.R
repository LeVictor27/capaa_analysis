##############################################################################
# 03 (DEPRECIATED) PREDICTED PLOTS: FRPL & STUDENTS OF COLOR
##############################################################################

###########################################################################
# SET UP MODELS
###########################################################################

##FEDERAL (CWI_18D)
df_rev_fed <- df_rev_merge%>%
  filter(rev_source=="Federal")

model_fed <- lm(rev_pp_18d ~ log(TotalEnrollment) + LOCALE + PctSpecialEducation + PctTransitionalBilingual  + PctFreeorReducedPricedMeals + schyear, data=df_rev_fed) 
#Brief model has main effect of school year and indicator of perctile FRL (.1, 0.9) and add interaction term between the 0.1 and 0.9
#Coefficient is the difference between the 90th from state average and the 10th from the state average

df_rev_fed$rev_adjusted <- predict.lm(model_fed, newdata=df_rev_fed)

#Have poverty as part of predictor and have marginal estimate as part of the predictor
#Have poverty as a continuous variable and estimate marginal effect at .1 and .9 percentile (30/70% poverty)

#STATE
df_rev_state <- df_rev_merge%>%
  filter(rev_source=="State")

model_state <- lm(rev_pp_18d ~ log(TotalEnrollment) + LOCALE + PctSpecialEducation + PctTransitionalBilingual + PctFreeorReducedPricedMeals + schyear, data=df_rev_state)

df_rev_state$rev_adjusted <- predict.lm(model_state, newdata=df_rev_state)

#LOCAL
df_rev_loc <- df_rev_merge%>%
  filter(rev_source=="Local")

model_loc <- lm(rev_pp_18d ~ log(TotalEnrollment) + LOCALE + PctSpecialEducation + PctTransitionalBilingual + PctFreeorReducedPricedMeals + schyear, data=df_rev_loc)

df_rev_loc$rev_adjusted <- predict.lm(model_loc, newdata=df_rev_loc)

df_rev_total <- data.frame(
  df_rev_fed$schyear,
  df_rev_fed$leaid,
  df_rev_fed$coudis,
  df_rev_fed$rev_adjusted,
  df_rev_state$rev_adjusted,
  df_rev_loc$rev_adjusted)

df_rev_total <- df_rev_total%>%
  mutate(rev_adjusted = df_rev_fed.rev_adjusted + df_rev_state.rev_adjusted + df_rev_loc.rev_adjusted)%>%
  select(-c(df_rev_fed.rev_adjusted, df_rev_state.rev_adjusted, df_rev_loc.rev_adjusted))%>%
  rename(coudis=df_rev_fed.coudis,
         schyear=df_rev_fed.schyear,
         leaid=df_rev_fed.leaid)
#rename_with(~paste0(sub("^([^.]+\\.)", "", .)), -1)

df_rev_total <- full_join(df_rev_total, df_demos_dist, by=c("coudis"="coudis_s", "schyear"="schyear"))

###########################################################################
# GENERATE PLOTS: REVENUE SOURCE BY TIME FOR FRPL
###########################################################################

#FED FRPL

### LOW AND HIGH POVERTY

df_rev_fed_figure_pov_frpl <- df_rev_fed%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl))

levels(df_rev_fed_figure_pov_frpl$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

###########################################################################

df_rev_fed_figure_pov_frpl <- df_rev_fed%>%
  mutate(dist_group_frpl = as.factor(dist_group_frpl))

levels(df_rev_fed_figure_pov_frpl$dist_group_frpl) <- c("0-24%","25-49%","50-74%", "75-100%")


ggplot(df_rev_fed_figure_pov_frpl, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_frpl)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_frpl), alpha=0.25) +
  labs(title="Federal Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Student poverty levels calculated by free and reduced price lunch percentages
       \n\nData Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  scale_fill_manual(values=c("#9ebcda", "#8c96c6", "#8c6bb1", "#6e016b"), 
                    name="fill")

###########################################################################

#STATE FRPL

df_rev_state_figure_pov_frpl <- df_rev_state%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl))

levels(df_rev_state_figure_pov_frpl$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

###########################################################################

df_rev_state_figure_pov_frpl <- df_rev_state%>%
  mutate(dist_group_frpl = as.factor(dist_group_frpl))

levels(df_rev_state_figure_pov_frpl$dist_group_frpl) <- c("0-24%","25-49%","50-74%", "75-100%")


ggplot(df_rev_state_figure_pov_frpl, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_frpl)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_frpl), alpha=0.25) +
  labs(title="State Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Student poverty levels calculated by free and reduced price lunch percentages
       \n\nData Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() + 
  scale_fill_manual(values=c("#9ebcda", "#8c96c6", "#8c6bb1", "#6e016b"), 
                    name="fill")

###########################################################################

#LOCAL FRPL

df_rev_loc_figure_pov <- df_rev_loc%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl))

levels(df_rev_loc_figure_pov$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

###########################################################################

df_rev_loc_figure_pov <- df_rev_loc%>%
  mutate(dist_group_frpl = as.factor(dist_group_frpl))

levels(df_rev_loc_figure_pov$dist_group_frpl) <- c("0-24%","25-49%","50-74%", "75-100%")


ggplot(df_rev_loc_figure_pov, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_frpl)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_frpl), alpha=0.25) +
  labs(title="Local Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Student poverty levels calculated by free and reduced price lunch percentages
       \n\nData Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() + 
  scale_fill_manual(values=c("#9ebcda", "#8c96c6", "#8c6bb1", "#6e016b"), 
                    name="fill")

###########################################################################
#TOTAL FRPL

df_rev_total_figure_pov <- df_rev_total%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl))

levels(df_rev_total_figure_pov$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

###########################################################################

df_rev_total_figure_pov <- df_rev_total%>%
  mutate(dist_group_frpl = as.factor(dist_group_frpl))

levels(df_rev_total_figure_pov$dist_group_frpl) <- c("0-24%","25-49%","50-74%", "75-100%")

ggplot(df_rev_total_figure_pov, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_frpl)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_frpl), alpha=0.25) +
  labs(title="Total Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, and percent of students enrolled in free and reduced price lunch,
       special education and classified as limited English proficient",
       caption="Note: Student poverty levels calculated by free and reduced price lunch percentages
       \n\nData Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() + 
  scale_fill_manual(values=c("#9ebcda", "#8c96c6", "#8c6bb1", "#6e016b"), 
                    name="fill")

###########################################################################
# GENERATE PLOTS: REVENUE SOURCE BY TIME FOR SOC
###########################################################################

ggplot(df_rev_fed, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_soc)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_soc), alpha=0.25) +
  labs(title="Federal Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, average cost of labor, and percent of students enrolled in 
       free and reduced lunch, special education and classified as limited English proficient",
       caption="Data Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  scale_fill_manual(values=c("#85754d", "#a6611a", "#c2a5cf","#7b3294"), 
                    name="fill")

###########################################################################

ggplot(df_rev_state, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_soc)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_soc), alpha=0.25) +
  labs(title="State Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, average cost of labor, and percent of students enrolled in 
       free and reduced lunch, special education and classified as limited English proficient",
       caption="Data Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  scale_fill_manual(values=c("#85754d", "#a6611a", "#c2a5cf","#7b3294"), 
                    name="fill")

###########################################################################

ggplot(df_rev_loc, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_soc)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_soc), alpha=0.25) +
  labs(title="Local Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, average cost of labor, and percent of students enrolled in 
       free and reduced lunch, special education and classified as limited English proficient",
       caption="Data Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  scale_fill_manual(values=c("#85754d", "#a6611a", "#c2a5cf","#7b3294"), 
                    name="fill")

###########################################################################

ggplot(df_rev_total, 
       aes(x=schyear, y=rev_adjusted, group=dist_group_soc)) + 
  stat_summary(geom="line", fun="mean", alpha=0.5) +
  stat_summary(geom="ribbon", 
               fun.data = mean_se, #mean_cl_normal
               aes(fill=dist_group_soc), alpha=0.25) +
  labs(title="Total Revenue Amount Per Student",
       subtitle = "Regression adjusted for difference of cost factors including district size, 
       urbanicity/locale, average cost of labor, and percent of students enrolled in 
       free and reduced lunch, special education and classified as limited English proficient",
       caption="Data Source: F-196")+
  xlab("School Year (Spring)") + 
  ylab("2018 Dollars") + 
  geom_vline(xintercept = "2018", colour='#FFFFFF', linetype="dashed")+
  theme_uw() +
  scale_fill_manual(values=c("#85754d", "#a6611a", "#c2a5cf","#7b3294"), 
                    name="fill")

