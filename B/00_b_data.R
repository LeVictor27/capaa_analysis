##############################################################################
# 00 MAIN DATA SET-UP
##############################################################################

# SESSION -> SET WORKING DIRECTORY -> TO SOURCE FILE LOCATION

setwd('..') # This moves working directory up to root Github directory
library(here)
source("theme_uw.R")

library(tidyverse)
library(googledrive)
library(haven)
library(ggtext)
library(caret)
library(car)
library(ggeffects)
library(margins) #R version of Stata's margins
#https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
library(collapse)
library(purrr)

##############################################################################
# READ ALL DATA IN

# NOTE: If reproducing code, will need to check the Google Drive links to note last
# update and change Datasets load as of June 2022, but data updates beyond may change 
# the structure of the data and the subsequent code
##############################################################################

id_f196_expenditures <- drive_get(as_id("https://drive.google.com/file/d/17AZv1i2plWBko1HYKhZz6NdJimfxnj0c/view?usp=sharing"))
#2015-2020

id_f196_revenues <- drive_get(as_id("https://drive.google.com/file/d/13bZJ6QuJmY-vgBgIZO1Dme5e3qwRgVHx/view?usp=sharing"))

id_f196_rev_dict <- drive_get(as_id("https://drive.google.com/file/d/1NK3PJXYMrcKaZUQuE2bWLqkwK0XjeVnX/view?usp=sharing"))

id_f195_revenues <- drive_get(as_id("https://drive.google.com/file/d/10XytJG-r2zfM3vpbClTJP9Jnz-6nX0Ef/view?usp=sharing"))

id_demos <- drive_get(as_id("https://drive.google.com/file/d/1coJ9kMZUpRmxNyli6xZ31WITaZdyJXIk/view?usp=sharing"))

id_demos_2021 <- drive_get(as_id("https://drive.google.com/file/d/1L8CS0KH1_CvGr7mC61ooOU9JdmG1My86/view?usp=sharing"))

id_cwi <- drive_get(as_id("https://drive.google.com/file/d/1--RKu4geGLO9sOxag7pyJtkw40ZYYkeI/view?usp=sharing"))

df_f196_expenditures <- id_f196_expenditures %>%
  drive_read_raw() %>%
  read_dta()

df_f196_revenues <- id_f196_revenues %>%
  drive_read_raw() %>%
  read_dta()

df_f196_rev_dict <- id_f196_rev_dict %>%
  drive_read_raw() %>%
  read_dta()

df_f195_revenues <- id_f195_revenues %>%
  drive_read_raw() %>%
  read_dta()

df_demos <- id_demos %>%
  drive_read_raw() %>%
  read_dta()

df_demos_2021 <- id_demos_2021 %>%
  drive_read_raw() %>%
  read_csv()

df_cwi <- id_cwi %>%
  drive_read_raw() %>%
  read_csv()

##############################################################################
# GENERAL DATA CLEANING
##############################################################################

df_demos_dist <- df_demos_2021%>%
  filter(OrganizationLevel=="District")%>%
  filter(Gradelevel=="AllGrades")%>%
  drop_na(ESDOrganizationID)%>%
  filter(!str_detect(ESDName, "Charter School"))%>%
  filter(!str_detect(DistrictName, "Charter"))%>%
  select(-OrganizationLevel, -County, ESDName, ESDOrganizationID,
         -DistrictOrganizationId, -SchoolCode, -SchoolName,
         -SchoolOrganizationid, -CurrentSchoolType, -Gradelevel,
         -DataAsOf)%>%
  mutate(coudis_s = str_pad(as.character(DistrictCode), 5, side="left", pad="0"),
         schyear = paste("20", gsub("[0-9]+-", "", SchoolYear), sep=""))%>%
  rename("TotalEnrollment" = "All Students",
         "NumberFemales" = "Female",
         "NumberMales" = "Male",
         "NumberGenderX" = "Gender X",
         "NumberAmericanIndianOrAlaskan" = "American Indian/ Alaskan Native",
         "NumberAsian" = "Asian",
         "NumberBlack" = "Black/ African American",
         "NumberHispanic" = "Hispanic/ Latino of any race(s)",
         "NumberAsianPacificIslander" = "Native Hawaiian/ Other Pacific Islander",
         "NumberMultirac" = "Two or More Races",
         "NumberWhite" = "White",
         "NumberFreeorReducedPricedMeals" = "Low-Income",
         "NumberSpecialEducation" = "Students with Disabilities",
         "NumberTransitionalBilingual" = "English Language Learners")

df_demos_dist <- df_demos_dist%>%
  select(coudis_s,
         schyear,
         "TotalEnrollment",
         "NumberAmericanIndianOrAlaskan",
         "NumberAsian",
         "NumberBlack",
         "NumberHispanic",
         "NumberAsianPacificIslander",
         "NumberMultirac",
         "NumberWhite",
         "NumberFreeorReducedPricedMeals",
         "NumberSpecialEducation",
         "NumberTransitionalBilingual")

df_demos_dist <- df_demos_dist%>%
  mutate(PctAmericanIndianOrAlaskan = NumberAmericanIndianOrAlaskan/TotalEnrollment,
         PctAsian = NumberAsian/TotalEnrollment,
         PctBlack = NumberBlack/TotalEnrollment,
         PctHispanic = NumberHispanic/TotalEnrollment,
         PctAsianPacificIslander = NumberAsianPacificIslander/TotalEnrollment,
         PctMultirac = NumberMultirac/TotalEnrollment,
         PctWhite = NumberWhite/TotalEnrollment,
         PctFreeorReducedPricedMeals = NumberFreeorReducedPricedMeals/TotalEnrollment,
         PctSpecialEducation = NumberSpecialEducation/TotalEnrollment,
         PctTransitionalBilingual = NumberTransitionalBilingual/TotalEnrollment
  )%>%
  mutate_at(vars(PctAmericanIndianOrAlaskan:PctTransitionalBilingual), ~replace_na(., 0))

df_demos_dist$dist_nstu_soc = rowSums((df_demos_dist[,c("NumberAsian", "NumberBlack", "NumberHispanic", "NumberAmericanIndianOrAlaskan", "NumberAsianPacificIslander", "NumberMultirac")]), na.rm = TRUE)

df_demos_dist <- df_demos_dist%>%
  mutate(dist_pctstu_soc = as.numeric(dist_nstu_soc/TotalEnrollment),
         dist_group_soc = case_when(dist_pctstu_soc >= 0  & dist_pctstu_soc < 0.25 ~ '1',
                                    dist_pctstu_soc >= 0.25  & dist_pctstu_soc < 0.50 ~ '2',
                                    dist_pctstu_soc >= 0.50  & dist_pctstu_soc < 0.75 ~ '3',
                                    dist_pctstu_soc >= 0.75  & dist_pctstu_soc <= 1 ~ '4'),
         dist_group_frpl = case_when(PctFreeorReducedPricedMeals >= 0  & PctFreeorReducedPricedMeals < 0.25 ~ '1',
                                     PctFreeorReducedPricedMeals >= 0.25  & PctFreeorReducedPricedMeals < 0.50 ~ '2',
                                     PctFreeorReducedPricedMeals >= 0.50  & PctFreeorReducedPricedMeals < 0.75 ~ '3',
                                     PctFreeorReducedPricedMeals >= 0.75  & PctFreeorReducedPricedMeals <= 1 ~ '4'),
         
  )

df_demos_dist$deciles_frpl <- ntile(df_demos_dist$PctFreeorReducedPricedMeals, 10)
df_demos_dist$quartile_frpl <- ntile(df_demos_dist$PctFreeorReducedPricedMeals, 4)

df_f196_expenditures <- df_f196_expenditures%>%
  mutate(coudis_s=as.character(coudis_s),
         schyear=as.character(schyear))

df_e_merge_raw <- full_join(df_f196_expenditures, df_demos_dist, by=c("coudis_s"="coudis_s", "schyear"="schyear"))

df_f196_revenues <- df_f196_revenues%>%
  mutate(rev_type_s = haven::as_factor(df_f196_revenues$rev_type, levels="label"))%>%
  mutate(fund_s = haven::as_factor(df_f196_revenues$fund, levels="label"))

df_cwi <- df_cwi%>%
  mutate(coudis_s = str_pad(as.character(dist_id), 5, side="left", pad="0"))

##############################################################################
# REVENUE DATA CLEAN
##############################################################################

#Pull revenue categories: Federal, State, Local, and Other
df_rev_merge_source <- df_f196_revenues%>%
  mutate(rev_source = case_when(str_detect(rev_type_s, 'Federal') ~ "Federal",
                                str_detect(rev_type_s, 'State') ~ "State",
                                str_detect(rev_type_s, 'Local') ~ "Local",
                                TRUE ~ 'Other'))

df_rev_collapse <- collap(df_rev_merge_source, rev~ coudis + schyear + rev_source, 
                          FUN = list(fsum))

df_rev_collapse <- df_rev_collapse%>%
  rename(revenue_amount=rev)

#Bring in NCES Geographic designation.
#https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

id_nces_geo <-drive_get(as_id("https://drive.google.com/file/d/1YXv-RxZMVEO3YiIF-RS4IFTClFgUOHgh/view?usp=sharing"))

df_nces_geo <- id_nces_geo %>%
  drive_read_raw() %>%
  read_csv()

df_locale_wa <- df_nces_geo%>%
  filter(STATE=="WA")%>%
  select(LEAID, NAME, LOCALE)%>%
  mutate(LOCALE=as.character(LOCALE),
         LEAID=as.numeric(LEAID))

#Add CPI (Generated by Chris)
df_cpi18 <- df_f196_revenues%>%
  select(schyear, cpi18)%>%
  mutate(schyear=as.character(schyear))%>%
  distinct()

df_leaid <- df_cwi%>%
  select(-cwi, -cwi_se, -cwi_2020_extrapolated, -year)%>%
  distinct()

df_rev_leaid<- full_join(df_rev_collapse, df_leaid, by=c("coudis"="coudis_s"))

df_rev_leaid <- df_rev_leaid%>%
  mutate(schyear=as.character(schyear))

df_rev_leaid_cpi<- full_join(df_rev_leaid, df_cpi18, by=c("schyear"="schyear"))

df_rev_merge<- full_join(df_rev_leaid_cpi, df_demos_dist, by=c("coudis"="coudis_s", "schyear"="schyear"))

df_rev_merge<- full_join(df_rev_merge, df_locale_wa, by=c("leaid"="LEAID"))