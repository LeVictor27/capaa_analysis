###########################################################################
# 04 (DATA SYNC ISSUE) REVENUE BREAKDOWN BY CATEGORY
###########################################################################

# NOTE: CURRENTLY NOT SYNCED WITH UPDATED SCHFINWAY DATA AS OF JAN 2022  UPDATE
# NEED TO LOOK AT REVENUE BREAKDOWN FOR CLEANED AND UPDATED 195
# F195 NEEDS TO HAVE REV CATEGORY BREAKDOWN AS DETAILED IN LINE 17 - 27
###########################################################################

#ACCOUNTING GUIDEBOOKS FROM THE STATE
#AVERAGE STUDENT, WEIGHT BY STUDENTS WITHIN
#FOR THE AVERAGE STUDENT WITHIN THE BIN 
#INCLUDE 3 YEAR AVERAGE VS 3 DIFFERNT GRAPHS, LOOK TO THIS CONTEXT
#SO CAN MAKE A COMPARISON TO SEE WHAT THE IMPACT OF A SINGLE YEAR IS
#UNWEIGHTED AVERAGES WITHIN THE DISTRICT AND THEN WEIGHTED AVERAGES ACROSS THE DISTRICTS

df_f195_cat <- df_f195%>%
  mutate(rev_category = case_when(
    str_detect(item_desc, regex("\\bTOTAL FEDERAL, GENERAL PURPOSE\\b", ignore_case=FALSE)) ~ "Federal",                          
    str_detect(item_desc, regex("\\bTOTAL FEDERAL, SPECIAL PURPOSE\\b", ignore_case=FALSE)) ~ "Federal",
    str_detect(item_desc, regex("\\bTOTAL STATE, GENERAL PURPOSE\\b", ignore_case=FALSE)) ~ "State",                                
    str_detect(item_desc, regex("\\bTOTAL STATE, SPECIAL PURPOSE\\b", ignore_case=FALSE)) ~ "State",
    str_detect(item_desc, regex("\\bTOTAL LOCAL SUPPORT NONTAX\\b", ignore_case=FALSE)) ~ "Local",                                    
    str_detect(item_desc, regex("\\bTOTAL LOCAL TAXES\\b", ignore_case=FALSE)) ~ "Local",                                             
    str_detect(item_desc, regex("\\bTOTAL OTHER FINANCING SOURCES\\b", ignore_case=FALSE)) ~ "Other",                           
    str_detect(item_desc, regex("\\bTOTAL REVENUES FROM OTHER ENTITIES\\b", ignore_case=FALSE)) ~ "Other",                            
    str_detect(item_desc, regex("\\bTOTAL REVENUES FROM OTHER SCHOOL DISTRICTS\\b", ignore_case=FALSE)) ~ "Other",   
    TRUE ~ NA_character_
  )
  )

df_f195_cat <- df_f195_cat%>%
  fill(rev_category, .direction = "down")%>%
  filter(amount>0)

df_f195_all_cwi<- full_join(df_f195_cat, df_cwi_cpi, by=c("CCDDD"="coudis_s", "year"="schyear"))
df_f195_all_merge_raw <- full_join(df_f195_all_cwi, df_demos_dist, by=c("CCDDD"="coudis_s", "year"="schyear"))

df_f195_all_merge <- df_f195_all_merge_raw%>%
  filter(!str_detect(item_desc, "\\bTOTAL FEDERAL, GENERAL PURPOSE\\b"),
         !str_detect(item_desc, "\\bTOTAL FEDERAL, SPECIAL PURPOSE\\b"),
         !str_detect(item_desc, "\\bTOTAL LOCAL SUPPORT NONTAX\\b"),
         !str_detect(item_desc, "\\bTOTAL LOCAL TAXES\\b"),
         !str_detect(item_desc, "\\bTOTAL STATE, GENERAL PURPOSE\\b"),
         !str_detect(item_desc, "\\bTOTAL STATE, SPECIAL PURPOSE\\b"),
         !str_detect(item_desc, "\\TOTAL OTHER FINANCING SOURCES\\b"),
         !str_detect(item_desc, "\\TOTAL REVENUES FROM OTHER ENTITIES\\b"),
         !str_detect(item_desc, "\\TOTAL REVENUES FROM OTHER SCHOOL DISTRICTS\\b"))

###########################################################################
# CLEANING DATA AND GENERATING PLOTS: FRPL 
###########################################################################

df_f195_all_merge_pov <- df_f195_all_merge%>%
  drop_na(deciles_frpl)%>%
  filter(year == 2019 | year == 2018)%>%
  filter(deciles_frpl==1 | deciles_frpl == 9)%>%
  mutate(deciles_frpl = as.factor(deciles_frpl),
         amount_pp = (amount/TotalEnrollment))

levels(df_f195_all_merge_pov$deciles_frpl) <- c("Low-Poverty", "High-Poverty")

df_f195_all_merge_pov_sum <- df_f195_all_merge_pov%>%
  group_by(CCDDD, item_desc, deciles_frpl, rev_category)%>%
  summarize(amount_pp_total=sum(amount_pp))

df_f195_all_merge_pov_avg <- df_f195_all_merge_pov_sum %>%
  group_by(deciles_frpl, item_desc)%>%
  summarize(amount_pp_avg=mean(amount_pp_total),
            rev_category=rev_category)

df_f195_all_merge_pov_avg%>%
  filter(item_desc!="NA",
         amount_pp_avg > 100)%>%
  ggplot(aes(reorder(item_desc, amount_pp_avg), 
             y=amount_pp_avg, fill=rev_category, group=deciles_frpl)) +
  geom_bar(position = position_dodge(width=0.5), stat="identity")+
  coord_flip()+
  labs(title="Two Year Average Per-Student Dollar Amount\n by All Revenue Categories, 2018/2019",
       caption="Note: High and low-poverty districts estimated at the 10th and 90th percentile 
       of district percentage of students qualifying for free and reduced price lunch, 
       with an equal number of districts in each quantile")+
  scale_fill_manual(values = palette_uw(4), limits=c("Federal","State","Local","Other")) + 
  xlab("") + 
  ylab("") + 
  facet_wrap(~deciles_frpl,
             nrow=1)+
  theme_uw() 

###########################################################################
# CLEANING DATA AND GENERATING PLOTS: SOC 
###########################################################################

df_f195_all_merge <- df_f195_all_merge%>%
  filter(year == 2019 | year==2018)%>% 
  drop_na(dist_group_soc)%>%
  mutate(dist_group_soc = as.factor(dist_group_soc),
         amount_pp = (amount/TotalEnrollment))

levels(df_f195_all_merge$dist_group_soc) <- c("0-24%","25-49%","50-74%", "75-100%")

df_f195_all_merge_sum <- df_f195_all_merge%>%
  group_by(CCDDD, item_desc, dist_group_soc, rev_category)%>%
  summarize(amount_pp_total=sum(amount_pp))

df_f195_all_merge_avg <- df_f195_all_merge_sum %>%
  group_by(dist_group_soc, item_desc)%>%
  summarize(amount_pp_avg=mean(amount_pp_total),
            rev_category=rev_category)

df_f195_all_merge_avg%>%
  filter(item_desc!="NA",
         amount_pp_avg > 100)%>%
  ggplot(aes(reorder(item_desc, amount_pp_avg), 
             y=amount_pp_avg, fill=rev_category)) +
  geom_bar(position = position_dodge(width=0.5), stat="identity")+
  coord_flip()+
  labs(title="Two Year Average Per-Student Dollar Amount\n by All Revenue Categories, 2018/2019 ")+
  scale_fill_manual(values = palette_uw(4), limits=c("Federal","State","Local","Other")) + 
  xlab("") + 
  ylab("") +
  theme_uw()

df_f195_all_merge_avg%>%
  filter(item_desc!="NA",
         amount_pp_avg > 100)%>%
  ggplot(aes(reorder(item_desc, amount_pp_avg), 
             y=amount_pp_avg, fill=rev_category, group=dist_group_soc)) +
  geom_bar(position = position_dodge(width=0.5), stat="identity")+
  coord_flip()+
  labs(title="Two Year Average Per-Student Dollar Amount\n by All Revenue Categories, 2018/2019",
       subtitle="By District Ratio Student of Color")+
  scale_fill_manual(values = palette_uw(4), limits=c("Federal","State","Local","Other")) + 
  xlab("") + 
  ylab("") + 
  facet_wrap(~dist_group_soc,
             nrow=1) +
  theme_uw()


