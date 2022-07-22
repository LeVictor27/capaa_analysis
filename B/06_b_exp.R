###########################################################################
# 06 PLOTS: EXPENDITURE 
###########################################################################

###########################################################################
# EXPENDITURE CATEGORIES
###########################################################################

#REMOVE FEDERAL AND CATEGORICAL FUNDS, IF JUST WANT TO FOCUS ON SUM OF STATE AND LOCAL
#LOOK AT ACT_DESC UNDER BASIC EDUCATION, MCCLEARY FOCUSED ON BASIC EDUCATION, SEE WHAT THE BREAKDOWN IS

df_e_2020 <- df_e_merge_raw%>%
  filter(schyear == 2020)%>% 
  drop_na(dist_group_soc)%>%
  mutate(dist_group_soc = as.factor(dist_group_soc),
         amount_pp = (amount/TotalEnrollment))

levels(df_e_2020$dist_group_soc) <- c("0-24%","25-49%","50-74%", "75-100%")

df_e_2020_sum <- df_e_2020%>%
  group_by(coudis_s, prog_desc, dist_group_soc)%>%
  summarize(amount_pp_total=sum(amount_pp))

df_e_2020_avg <- df_e_2020_sum %>%
  group_by(dist_group_soc, prog_desc)%>%
  summarize(amount_pp_avg=mean(amount_pp_total))

df_e_2020_avg%>%
  filter(prog_desc!="NA")%>%
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title="Average Per-Student Dollar Amount by All Expenditure Categories, 2019-2020")+
  xlab("") + 
  ylab("") +
  theme_uw()

df_e_2020_avg%>%
  filter(prog_desc!="Basic Education" & prog_desc!="Districtwide Support")%>% #Remove basic education program and Districtwide support
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title="Average Per-Student Dollar Amount by Expenditure Category, 2019-2020",
       subtitle = "Excluding Top Two Categories (Basic Education and Districtwide Support) ")+
  xlab("") + 
  ylab("") +
  theme_uw()

#REVIEW: 
#Which expenditure category to focus on (filter by federal, state, others)? 
#Do we want to cut off the categories with minimal amounts? 
#Group the bottom chunk with OTHER category
#look a regionalization factor

df_e_2020_avg%>%
  filter(prog_desc!="NA")%>% 
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg, 
        group=dist_group_soc,
        fill=dist_group_soc)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title = "Average Per-Student Dollar Amount by Expenditure Category, 2019-2020",
       subtitle = "By District Percentage Student of Color")+
  xlab("") + 
  ylab("") +
  scale_fill_manual(values = palette_uw_reverse(4)) + 
  facet_wrap(~dist_group_soc,
             nrow=1)+
  theme_uw()

df_e_2020_avg%>%
  filter(prog_desc!="Basic Education" & prog_desc!="Districtwide Support")%>% #Remove basic education program and Districtwide support
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg, 
        group=dist_group_soc,
        fill=dist_group_soc)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title = "Average Per-Student Dollar Amount by Expenditure Category, 2019-2020",
       subtitle = "By District Percentage Student of Color and Excluding Top Two Categories ")+
  xlab("") + 
  ylab("") +
  scale_fill_manual(values = palette_uw_reverse(4)) + 
  facet_wrap(~dist_group_soc,
             nrow=1)+
  theme_uw()

#REVIEW:
#Same questions as above.
#I separated SOC into quartiles, do we want to categorize them differently? 
#What's both interesting and consumable? 

###########################################################################
# EXPENDITURE CATEGORIES: POV
###########################################################################

df_e_2020_pov <- df_e_merge_raw%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  filter(schyear == 2020)%>% 
  drop_na(deciles_frpl)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl),
         amount_pp = (amount/TotalEnrollment))

levels(df_e_2020_pov$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

df_e_2020_pov_sum <- df_e_2020_pov%>%
  group_by(coudis_s, prog_desc, deciles_frpl)%>%
  summarize(amount_pp_total=sum(amount_pp))

df_e_2020_pov_avg <- df_e_2020_pov_sum %>%
  group_by(deciles_frpl, prog_desc)%>%
  summarize(amount_pp_avg=mean(amount_pp_total))

df_e_2020_pov_avg%>%
  filter(prog_desc!="NA")%>% 
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg, 
        group=deciles_frpl,
        fill=deciles_frpl)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title = "Average Per-Student Dollar Amount by Expenditure Category, 2019-2020",
       subtitle = "By District Percentage of Students Qualifying for Free and Reduced Price Lunch")+
  xlab("") + 
  ylab("") +
  scale_fill_manual(values=palette_uw_reverse(2))+
  facet_wrap(~deciles_frpl,
             nrow=1)+
  theme_uw()

df_e_2020_pov_avg%>%
  filter(prog_desc!="Basic Education" & prog_desc!="Districtwide Support")%>% #Remove basic education program and Districtwide support
  ggplot(
    aes(reorder(prog_desc, amount_pp_avg, sum), 
        amount_pp_avg, 
        group=deciles_frpl,
        fill=deciles_frpl)) +
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  labs(title = "Average Per-Student Dollar Amount by Expenditure Category, 2019-2020",
       subtitle = "By District Percentage of Students Qualifying for Free and Reduced Price Lunch and Excluding Top Two Categories")+
  xlab("") + 
  ylab("") +
  scale_fill_manual(values=palette_uw_reverse(2))+
  facet_wrap(~deciles_frpl,
             nrow=1)+
  theme_uw()

#We focus on the 2019-2020 school year, the second year of the McCleary reform.