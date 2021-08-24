# -----------------------------------------------------------------------------
# Haiti ITS Analysis R Code 
# Author: Will Eaton
# R code for data manipulation, cleaning, and ITS Analysis of data through 2020 
# Date Created: 8-24-21
# Date Modified: 8-24-21 - Modified code for cleaning 2019 data on 7-5-20
# Repository information: 
# -----------------------------------------------------------------------------

# Load packages ---------------------------------------------------------------

library(tidyverse)
# library(dplyr)
library(padr)
# install.packages("remotes")
library(remotes)
# remotes::install_github("skgrange/threadr")
library(naniar)
library(zoo)
library(threadr)
library(reshape)

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Remove all objects from the current workspace (R memory)
# -----------------------------------------------------------------------------

rm(list = ls())
# This function closes the specified plot (by default the current device) 
# and if it is an imguR device, uploads the plots for web hosting
dev.off()
# Clear startup screen/console in R / RStudio
cat("\014") 

# Set working drive -------------------------------------------------------

setwd("/Users/willeaton/Box/Malaria Zero (Tulane)/ITS Analysis/Data") #Mac

### --- Import data sets --- ### 

# Bring in 2020 SISNU/HSIS dataset that requires variable translation -----
# Note that this data set was provided by Phoebe and has not been validated
# Requested latest validated from Val - new epi person @ CHAI
# n = 59,917 observations and 74 variables

hti_20_invalid <-read.csv("PNCM_Données partielles de 2020.csv")

# Bring in 2019 SISNU/HSIS Dataset ----------------------------------------
# n = 68,187 observations and 83 variables

hti_19 <-read.csv("2012_2019 OU Long 2020 06 12.csv")

# Bring n=55 health facilities to map  ------------------------------------

hf_55_gps <-read.csv("haiti2GPS_cx.csv")

# Bring in haiti_prelim_2015_2018_9_24_19 dataset for population data then create growth rate 
# * REVISIT THIS - CONSIDER Multiple population sources for sensitivity analysis
haiti_prelim_2015_2018_9_24_19 <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Final Dataset/haiti_prelim_2015_2018_9_24_19.csv")

# Check for missing variables ---------------------------------------------
table(hti_19$ADM1Name, useNA = "always")
# Artibonite     Centre Grand'Anse     Nippes       Nord   Nord'Est Nord'Ouest      Ouest        Sud    Sud'Est       <NA> 
#     9844       3928       4820       3036       7038       4022       7619      17971       5505       4404          0 
table(hti_19$Year, useNA = "always")
# 2012  2013  2014  2015  2016  2017  2018  2019  <NA> 
#     5957  6665  8283  8585  8516  8826 12216  9139     0 

# Subset Grand'Anse department --------------------------------------------
ga_19 <- hti_19 %>% filter(ADM1Name == "Grand'Anse") # n = 4,820 obs
# Subset 2014-2019, n = 3,840
ga_14_19 <- ga_19 %>% filter(Year %in% c("2014", "2015", "2016", "2017", "2018",
                                         "2019"))

# Check for missing values again
table(ga_14_19$OUName_MSPP, useNA = "always") # no missing values
table(ga_14_19$HF_Code_Final, useNA = "always") # 455 missing
table(ga_14_19$MZ_HF_Code, useNA = "always") # no missing values
unique(ga_14_19$MZ_HF_Code, useNA = "always") # n = 73 unique MZ_HF_Code identifiers
table(ga_14_19$Latitude_Final, useNA = "always") # n = 407 missing
table(ga_14_19$Longitude_Final, useNA = "always") # n = 407 missing
table(ga_14_19$Type_new_final, useNA = "always") # none missing
#                 Private  Public    <NA> 
# 364      49     492    2340       0 
table(ga_14_19$HF_Catchment_Pop_MAP, useNA = "always") # n = 3,840 missing - all missing
table(ga_14_19$Type, useNA = "always")
# Private   Privee   Public Publique     <NA> 
#     61       49      267      433     1372     1063        0 
table(ga_14_19$Category_MSPP, useNA = "always")
#                             .         CAL         CSL       Disp. Dispensaire         HCR          HD        <NA> 
# 525          49         162         297         470        1508          60         115          59           0
table(ga_14_19$ConfirmedcasesMonthlytotal, useNA = "always") # n = 162 missing
table(ga_14_19$OUName_MSPP, useNA = "always") # none missing
# Abricot Anse d'Hainault        Beaumont          Bonbon      Chambellan          Corail      Dame Marie           Irois         Jérémie 
#     120             237             184             118             180             124             300             116            1002 
# Moron          Pestel         Roseaux            <NA> 
#   178             379             307               0 
table(ga_14_19$Month, useNA = "always")
#   1    2    3    4    5    6    7    8    9   10   11   12 <NA> 
# 275  269  272  266  266  274  268  276  272  274  270  263    0 
table(ga_14_19$ConfirmedcasesCOMMUNITYMonth, useNA = "always") # n = 1,336 missing
table(ga_14_19$ConfirmedcasesPOCMonthl, useNA = "always") # n = 191 missing
table(ga_14_19$ConfirmedcasesMonthlytotal, useNA = "always") # n = 162 missing
table(ga_14_19$TestedTotalmonthly, useNA = "always") # no missing values
table(ga_14_19$TestedPASSIVEPointofCareMoRDT, useNA = "always") # n = 34 missing values
table(ga_14_19$TestedCOMMUNITYMonthly, useNA = "always") # n = 3577 missing values


# Show n= 162 health facilities that have missing values
hf_miss_cases <- ga_14_19 %>% filter(is.na(ConfirmedcasesMonthlytotal))
table(hf_miss_cases$MZ_HF_Code, useNA = "always") # no missing values
# These health facilities have missing values for confirmed cases monthly total
# MZ_1010 MZ_1018 MZ_1033 MZ_1069 MZ_1072 MZ_1074 MZ_1077 MZ_1082 MZ_1089 
# 11       7      12       3      10       3      12       1       8 
# MZ_1146 MZ_1151 MZ_1163   MZ_64  MZ_683  MZ_691  MZ_702  MZ_718  MZ_722 
# 8       8       6       2       1       2       1       6       7 
# MZ_724  MZ_727  MZ_729  MZ_730  MZ_731  MZ_900  MZ_922  MZ_952  MZ_994 
# 1       1       4      12      11       7       4       3      11 
# <NA> 
#     0 

#Examine HF MZ_1151 ------------
MZ_1151_data <- ga_14_19 %>% filter(MZ_HF_Code == "MZ_1151") 

# Variables from STATA ----------------------------------------------------
# mz_hf_code month year monthid adm1name ouname_mspp hf_name hf_code_final type_new_final itvn_arm ///
#     longitude_final latitude_final month year confirmmonthtotal confirmedcasesmonthlytotal  med_pop incidence_per_1000  ///
#     bednet2017 tmda_irs_2018 chw_2018 ///

#--------------------------------------------------------------
# Variables I want with slimmer data set for easier management
#--------------------------------------------------------------
# 1) ADM1Name
# 2) OUName
# 3) Month
# 4) Year
# 5) MZ_HF_Code
# 6) Latitude_Final
# 7) Longitude_Final
# 8) OU_Population_UN_projected
# 9) OU_MAPPop16
# 10) OU_MAPPop15
# 11) Type_new_final
# 12) TestedTotalmonthly
# 13) ConfirmedcasesPOCMonthl
# 14) ConfirmedcasesMonthlytotal
# 15) ConfirmedcasesCOMMUNITYMonth
# 16) elevation (SRTM) - NEED TO BRING IN
# 17) population HFCA - NEED TO BRING IN
# 18) ctl or intvn group
# 19) LLIN variable 0,1,2,3,4,5,
# 20) itvn 0,1,2,3,4,5,6,7
# 21) LLIN binary 0/1
# 22) itvn binary 0/1

#--------------------------------------------------------------

# Slim down ga_14_19 so it's easier to manage -----------------------------
# n = 15 variables
# n = 3,840 obs
ga_14_19_slim <- ga_14_19 %>% 
    as_data_frame %>%
    select(ADM1Name, OUName_MSPP, MZ_HF_Code, Month, Year,
           Latitude_Final, Longitude_Final,
           OU_Population_UN_projected, OU_MAPPop16, OU_MAPPop15,
           Type_new_final, TestedTotalmonthly, ConfirmedcasesPOCMonthl,
           ConfirmedcasesMonthlytotal, ConfirmedcasesCOMMUNITYMonth)

# Tasks -------------------------------------------------------------------
# 1) pad dataset - done on n = 44, do on all GA for missingness visualizations
# 2) calculate population growth rate by hfca
#

# Variables to create -----------------------------------------------------
# 1) RDT stockout facility plausibility
# 2) medication stock-out plausibility score

# Create monthid variable -------------------------------------------------
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 1] <- 1
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 2] <- 2
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 3] <- 3
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 4] <- 4
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 5] <- 5
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 6] <- 6
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 7] <- 7
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 8] <- 8
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 9] <- 9
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 10] <- 10
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 11] <- 11
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 12] <- 12
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 1] <- 13
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 2] <- 14
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 3] <- 15
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 4] <- 16
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 5] <- 17
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 6] <- 18
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 7] <- 19
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 8] <- 20
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 9] <- 21
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 10] <- 22
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 11] <- 23
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 12] <- 24
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 1] <- 25
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 2] <- 26
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 3] <- 27
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 4] <- 28
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 5] <- 29
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 6] <- 30
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 7] <- 31
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 8] <- 32
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 9] <- 33
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 10] <- 34
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 11] <- 35
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 12] <- 36
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 1] <- 37
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 2] <- 38
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 3] <- 39
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 4] <- 40
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 5] <- 41
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 6] <- 42
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 7] <- 43
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 8] <- 44
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 9] <- 45
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 10] <- 46
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 11] <- 47
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 12] <- 48
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 1] <- 49
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 2] <- 50
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 3] <- 51
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 4] <- 52
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 5] <- 53
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 6] <- 54
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 7] <- 55
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 8] <- 56
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 9] <- 57
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 10] <- 58
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 11] <- 59
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 12] <- 60
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 1] <- 61
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 2] <- 62
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 3] <- 63
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 4] <- 64
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 5] <- 65
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 6] <- 66
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 7] <- 67
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 8] <- 68
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 9] <- 69
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 10] <- 70
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 11] <- 71
ga_14_19_slim$monthid[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 12] <- 72

ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 1] <- "2014/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 2] <- "2014/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 3] <- "2014/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 4] <- "2014/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 5] <- "2014/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 6] <- "2014/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 7] <- "2014/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 8] <- "2014/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 9] <- "2014/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 10] <- "2014/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 11] <- "2014/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2014 & ga_14_19_slim$Month == 12] <- "2014/12/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 1] <- "2015/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 2] <- "2015/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 3] <- "2015/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 4] <- "2015/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 5] <- "2015/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 6] <- "2015/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 7] <- "2015/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 8] <- "2015/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 9] <- "2015/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 10] <- "2015/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 11] <- "2015/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2015 & ga_14_19_slim$Month == 12] <- "2015/12/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 1] <- "2016/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 2] <- "2016/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 3] <- "2016/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 4] <- "2016/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 5] <- "2016/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 6] <- "2016/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 7] <- "2016/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 8] <- "2016/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 9] <- "2016/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 10] <- "2016/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 11] <- "2016/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2016 & ga_14_19_slim$Month == 12] <- "2016/12/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 1] <- "2017/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 2] <- "2017/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 3] <- "2017/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 4] <- "2017/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 5] <- "2017/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 6] <- "2017/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 7] <- "2017/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 8] <- "2017/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 9] <- "2017/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 10] <- "2017/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 11] <- "2017/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2017 & ga_14_19_slim$Month == 12] <- "2017/12/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 1] <- "2018/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 2] <- "2018/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 3] <- "2018/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 4] <- "2018/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 5] <- "2018/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 6] <- "2018/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 7] <- "2018/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 8] <- "2018/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 9] <- "2018/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 10] <- "2018/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 11] <- "2018/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2018 & ga_14_19_slim$Month == 12] <- "2018/12/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 1] <- "2019/01/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 2] <- "2019/02/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 3] <- "2019/03/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 4] <- "2019/04/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 5] <- "2019/05/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 6] <- "2019/06/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 7] <- "2019/07/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 8] <- "2019/08/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 9] <- "2019/09/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 10] <- "2019/10/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 11] <- "2019/11/01"
ga_14_19_slim$time[ga_14_19_slim$Year == 2019 & ga_14_19_slim$Month == 12] <- "2019/12/01"

# reorder variables -----------------------------
ga_14_19_slim_reorder <- ga_14_19_slim[, c(1, 2, 3, 16, 17, 4, 5, 6, 7, 8, 9, 10,
                                           11, 12, 13, 14, 15)]

# sort dataset by ouname_mspp, mz_hf_code, monthid ------------------------
# ga_master has n = 73 health facilities now, Phoebe mentioned there might be more
# n = 3245 obs
ga_master <- ga_14_19_slim_reorder[order(ga_14_19_slim_reorder$OUName_MSPP, 
                                         ga_14_19_slim_reorder$MZ_HF_Code, 
                                         ga_14_19_slim_reorder$monthid),]

# create binary intervention commune designation variable
# Itvn group (tMDA/IRS = Anse d'Hainault, Chambellan, Irois, Dame Marie, and
# Moron)
ga_master$itvn_grp[ga_master$OUName_MSPP == "Anse d'Hainault" | 
                       ga_master$OUName_MSPP == "Chambellan" |
                       ga_master$OUName_MSPP == "Irois" |
                       ga_master$OUName_MSPP == "Dame Marie" |
                       ga_master$OUName_MSPP == "Moron" 
] <- 1
ga_master$itvn_grp[ga_master$OUName_MSPP == "Abricot" | 
                       ga_master$OUName_MSPP == "Beaumont" |
                       ga_master$OUName_MSPP == "Jérémie" |
                       ga_master$OUName_MSPP == "Roseaux" |
                       ga_master$OUName_MSPP == "Moron" |
                       ga_master$OUName_MSPP == "Corail" |
                       ga_master$OUName_MSPP == "Pestel" 
] <- 0

# Examing how many HFs are missing entries and require padding
table(ga_master$MZ_HF_Code, useNA = "always")
# MZ_1010 MZ_1018 MZ_1033 MZ_1069 MZ_1072 MZ_1074 MZ_1077 MZ_1082 MZ_1089 MZ_1146 MZ_1151 MZ_1163 MZ_1187 MZ_1188 MZ_1189 MZ_1190   MZ_46   MZ_47   MZ_64  MZ_683  MZ_684  MZ_685 
# 12      25      13      24      12      22      12      24      12      22      22      21      11       1      12      12       4      72      38      63      70      59 
# MZ_686  MZ_687  MZ_688  MZ_689  MZ_690  MZ_691  MZ_692  MZ_693  MZ_694  MZ_695  MZ_696  MZ_697  MZ_698  MZ_699  MZ_700  MZ_701  MZ_702  MZ_703  MZ_705  MZ_706  MZ_707  MZ_708 
# 72      72      72      72      68      67      71      72      72      72      72      70      72      70      71      73      71      72      70      72      71      71 
# MZ_709  MZ_710  MZ_711  MZ_712  MZ_713  MZ_714  MZ_715  MZ_717  MZ_718  MZ_719  MZ_720  MZ_721  MZ_722  MZ_723  MZ_724  MZ_725  MZ_726  MZ_727  MZ_728  MZ_729  MZ_730  MZ_731 
# 72      72      72      71      72      68      66      72      56      33      72      72      69      70      68      72      71      71      66      54      33      29 
# MZ_78  MZ_882  MZ_900  MZ_905  MZ_922  MZ_952  MZ_994    <NA> 
#     41      72      44      41      43      44      26       0 

# What's going on with MZ_701? It has n = 73 obs
#Examine HF MZ_1151 ------------
MZ_701_data <- ga_master%>% filter(MZ_HF_Code == "MZ_701") 

# Remove one of the extra January 2019 entries. 
# Only a difference of 1 with case counts, remove the one with 287 tested total monthly
# n = 3840, n = 73 health facilities like in original LLIN analysis
ga_master_2<-ga_master[!(ga_master$MZ_HF_Code == "MZ_701" &
                             ga_master$TestedTotalmonthly==287),]


# Do I need this section? -----------------------------------------
# get interval
# ga_master$date %>% get_interval()
# ga_master_thincken <- ga_master %>% thicken(interval="week")
#------------------------------------------------------------------

# Pad entire GA data set for missingness figures
# There should be n = 5,256 observations after padding, but it looks like
# 5,266 obs exist after padding. Possible = 10 duplicates
ga_master$date <- as.Date(ga_master$time, "%Y/%m/%d") # change to appropriate date format for padding
ga_master_pad_n_73 <- 
    ga_master %>% pad(group = "MZ_HF_Code",  start_val = as.Date('2014-01-01'), end_val = as.Date('2019-12-01'))

# Show were duplicates exists
# show unique duplicates 
dups <- unique(ga_master_pad_n_73[duplicated(ga_master_pad_n_73),])

# Find duplicates (based on same first and last name)
dups2 <- ga_master_pad_n_73 %>% 
    group_by(MZ_HF_Code, date) %>% 
    filter(n()>1)

# Remove duplicates (keep only first instance of duplicated first and last name combinations)
# -------------------------------------------------
# This has the correct amount of obs. N = 5,256
# -------------------------------------------------
ga_no_dups <- ga_master_pad_n_73%>% 
    group_by(MZ_HF_Code, date) %>% 
    slice(1)

#------------------------------#
# Create case count categories
#------------------------------#
#Need to create new categorical/factor variable for NA entry, 0 entry and any numerical value >=1
ga_no_dups$casectcat[ga_no_dups$ConfirmedcasesMonthlytotal==0] <-"0"
ga_no_dups$casectcat[ga_no_dups$ConfirmedcasesMonthlytotal >=1] <-">=1"
ga_no_dups$casectcat[is.na(ga_no_dups$ConfirmedcasesMonthlytotal)] <-"NA"

# Fill in appropriate variables after padding
# ADM1Name, OUName_MSPP, monthid, Month, Year, Latitude_Final, Longitude_Final, Type_new_final,
# itvn group, 

# How many missing communes names?
# Abricot Anse d'Hainault        Beaumont          Bonbon      Chambellan          Corail      Dame Marie           Irois 
#     144             284             204             142             215             155             359             134 
# Jérémie           Moron          Pestel         Roseaux            <NA> 
#    1178             213             435             367            1426

# How many are missing Lat gps?
# table(ga_no_dups$Latitude_Final, useNA = "always")
# 18  18.37252  18.40626 18.436199  18.44134   18.4721   18.4771  18.47732 18.477791  18.47864 18.484261 18.489018 18.489861  18.48992 18.507799  18.50923  18.51679  18.51902 18.533918 
# 738        12        12         8        12        12        12         4        12        11        12        11        11        12        22        10        12        12        21 
# 18.536341   18.5375 18.541201 18.545321 18.545731  18.55076   18.5572 18.559641  18.55974  18.56016 18.560459 18.567221  18.57507  18.58721 18.595169  18.59535  18.59918 18.606871  18.62249 
# 12        12        12        12        12        12        12        12        12        12        12        12        12        12        12        12        12        12        24 
# 18.630671 18.635719 18.637489  18.64082  18.64576  18.64595  18.64744  18.64764   18.6486  18.66334   18.6651        19      <NA> 
#     12        22        12        12         1        12        12        12        12        12        12      2101      1828 
# Note: same n missing for longitude

# How many are missing for Type_new_final  and intervention group designation?
# Attempt to remedy this by revisiting SAS code
table(ga_no_dups$Type_new_final, useNA = "always") 
#             Private  Public    <NA> 
# 376      60     606    2788    1426 
table(ga_no_dups$itvn_grp, useNA = "always")
#    0    1 <NA> 
# 2696  992 1568 

# Fill in missing valuesv by group-------------------
ga_filled <- ga_no_dups %>%
    dplyr::group_by(MZ_HF_Code) %>%
    fill(OUName_MSPP, ADM1Name, Latitude_Final, Longitude_Final, itvn_grp, Type_new_final,
         .direction = "downup") %>%
    dplyr::ungroup()

# Verify filling of commune name after code
# table(ga_no_dups$OUName_MSPP, useNA = "always")
# Abricot Anse d'Hainault        Beaumont          Bonbon      Chambellan          Corail      Dame Marie           Irois 
#     144             288             366             144             216             210             360             144 
# Jérémie           Moron          Pestel         Roseaux            <NA> 
#    2016             216             720             432               0 

# table(ga_filled$Latitude_Final, useNA = "always") 
# n = 1224 obs missing (equates to 17 health facilities missing gps data)

# How many helath facilities in data set?
# unique(ga_filled$MZ_HF_Code, useNA = "always")
# [1] "MZ_1010" "MZ_1018" "MZ_1033" "MZ_1069" "MZ_1072" "MZ_1074" "MZ_1077" "MZ_1082" "MZ_1089" "MZ_1146" "MZ_1151" "MZ_1163" "MZ_1187" "MZ_1188"
# [15] "MZ_1189" "MZ_1190" "MZ_46"   "MZ_47"   "MZ_64"   "MZ_683"  "MZ_684"  "MZ_685"  "MZ_686"  "MZ_687"  "MZ_688"  "MZ_689"  "MZ_690"  "MZ_691" 
# [29] "MZ_692"  "MZ_693"  "MZ_694"  "MZ_695"  "MZ_696"  "MZ_697"  "MZ_698"  "MZ_699"  "MZ_700"  "MZ_701"  "MZ_702"  "MZ_703"  "MZ_705"  "MZ_706" 
# [43] "MZ_707"  "MZ_708"  "MZ_709"  "MZ_710"  "MZ_711"  "MZ_712"  "MZ_713"  "MZ_714"  "MZ_715"  "MZ_717"  "MZ_718"  "MZ_719"  "MZ_720"  "MZ_721" 
# [57] "MZ_722"  "MZ_723"  "MZ_724"  "MZ_725"  "MZ_726"  "MZ_727"  "MZ_728"  "MZ_729"  "MZ_730"  "MZ_731"  "MZ_78"   "MZ_882"  "MZ_900"  "MZ_905" 
# [71] "MZ_922"  "MZ_952"  "MZ_994" 
# n = 73
# subtract no gps (n=17)
# n = 56 health facilities

# Are there any itvn groups still missing?
table(ga_filled$itvn_grp, useNA = "always")
#    0    1 <NA> 
# 4104 1008  144

# create new ga_filled_2 dataset from ga_filled dataset
ga_filled_2 <- ga_filled
# Re-fill missing intervention groups
# Figure 4.12. Modeled-operational unit (OU) boundaries of 16 OUs in the five
# targeted pilot communes of:
# 1) Anse d’Hainault
# 2) Chambellan 
# 3) Dame-Marie
# 4) Moron
# 5) Les Irois, 
# where the rapid assessment was conducted. Red OUs had Rc 
# estimates in the top quartile across all of Grand’Anse, 
# gray OUs had Rc estimates in the bottom quartile, and the blue OU had 
# recent cases detected in the case-control study.
ga_filled_2$itvn_grp[ga_filled$OUName_MSPP == "Anse d'Hainault" | 
                         ga_filled$OUName_MSPP == "Chambellan" |
                         ga_filled$OUName_MSPP == "Irois" |
                         ga_filled$OUName_MSPP == "Dame Marie" |
                         ga_filled$OUName_MSPP == "Moron" 
] <- 1
ga_filled_2$itvn_grp[ga_filled$OUName_MSPP == "Abricot" | 
                         ga_filled$OUName_MSPP == "Beaumont" |
                         ga_filled$OUName_MSPP == "Jérémie" |
                         ga_filled$OUName_MSPP == "Roseaux" |
                         ga_filled$OUName_MSPP == "Bonbon" |
                         ga_filled$OUName_MSPP == "Corail" |
                         ga_filled$OUName_MSPP == "Pestel" 
] <- 0

# Verify appropriate filling of intervention group
# GTG = good to go
table(ga_filled_2$itvn_grp, useNA = "always") # no missing values
# 0    1 <NA> 
# 4032 1224    0 

# Are there any more Type_New_final observations missing? Yes, still missing values.
table(ga_filled_2$Type_new_final, useNA = "always") 
#                  Private  Public    <NA> 
#     1416     168     720    2952       0 

# Try Filling in missing values Type_new_final again by group-------------------
# This worked at remedying the NAs, but still have the blanks, not designated as NA
ga_filled_3 <- ga_filled_2 %>%
    dplyr::group_by(MZ_HF_Code) %>%
    fill(Type_new_final,.direction = "downup") %>%
    dplyr::ungroup()

# create new ga_filled_2 dataset from ga_filled dataset
ga_filled_4 <- ga_filled_3

# convert all blanks to NA
ga_filled_4 %>%
    replace_with_na_if(.predicate = is.character,
                       condition = ~.x %in% (" "))

# Examine at subset of facilities missing type_new_final variable
type_missing <- ga_filled_4[ which(!is.na(ga_filled_4$Type_new_final) & 
                                       ga_filled_4$Type_new_final != "Private" &
                                       ga_filled_4$Type_new_final != "Public") , ]

# show health facilities that are missing health facility type
# n = 1,584 obs are missing hf type variable
# equates to about n = 22 health facilities that are missing
# health faciltiy type variable
table(type_missing$MZ_HF_Code, useNA = "always") 
# MZ_1010 MZ_1018 MZ_1033 MZ_1069 MZ_1072 MZ_1074 MZ_1077 MZ_1082 MZ_1089 MZ_1146 MZ_1151 
# 72      72      72      72      72      72      72      72      72      72      72 
# MZ_1163 MZ_1187 MZ_1188 MZ_1189 MZ_1190   MZ_46  MZ_719  MZ_730  MZ_731   MZ_78  MZ_900 
# 72      72      72      72      72      72      72      72      72      72      72 
# <NA> 
#     0 
#--------------------------------------------------------------------------------
# Facility missing type, but not gps: 1077, 1082, 1146, 1163, 46, 719, 730, 731 
#--------------------------------------------------------------------------------

# Examine at subset of facilities missing gps variable
# n = 1,224 observations are misisng gps, 
# equates to n = 17 hfs, but verification required 
gps_missing <- ga_filled_4[ which(is.na(ga_filled_4$Latitude_Final)) , ]
# show health facilities that are missing latitude gps
table(gps_missing$MZ_HF_Code, useNA = "always") 
# MZ_1010 MZ_1018 MZ_1033 MZ_1069 MZ_1072 MZ_1089 MZ_1151 MZ_1187 MZ_1188 MZ_1189 MZ_1190 
# 72      72      72      72      72      72      72      72      72      72      72 
# MZ_64   MZ_78  MZ_900  MZ_905  MZ_922  MZ_952    <NA> 
#     72      72      72      72      72      72       0
#--------------------------------------------------------------------------------
# Facility missing gps but not type: 64, 905, 922, 952
#--------------------------------------------------------------------------------

# How many monthid's are missing? n = 1,426 are missing
table(ga_filled_4$monthid, useNA = "always")

# create new data set ga_filled_5 from ga_filled_4
ga_filled_5 <- ga_filled_4

#------------------------------------------------------------------
# Attempt to fill in missing values appropriately for monthid
# interpolate monthid variable - not working
#------------------------------------------------------------------
ga_filled_5 %>%
    group_by(MZ_HF_Code) %>%
    mutate_at(vars(matches("monthid")), na.approx, na.rm=FALSE)

# How many monthid's are missing after interpolation code? n = 1,426 are missing
table(ga_filled_5$monthid, useNA = "always")

# *--------------------------------------------------------------------------------*
# Verify monthid variables were filled in appropriately - NGTG - not good to go
# *--------------------------------------------------------------------------------*
table(ga_filled_5$monthid, useNA = "always")

# recode to fill in monthid variable (manually)
ga_filled_5$monthid[ga_filled_4$date ==  "2014/01/01"] <- 1
ga_filled_5$monthid[ga_filled_4$date ==  "2014/02/01"] <- 2
ga_filled_5$monthid[ga_filled_4$date ==  "2014/03/01"] <- 3
ga_filled_5$monthid[ga_filled_4$date ==  "2014/04/01"] <- 4
ga_filled_5$monthid[ga_filled_4$date ==  "2014/05/01"] <- 5
ga_filled_5$monthid[ga_filled_4$date ==  "2014/06/01"] <- 6
ga_filled_5$monthid[ga_filled_4$date ==  "2014/07/01"] <- 7
ga_filled_5$monthid[ga_filled_4$date ==  "2014/08/01"] <- 8
ga_filled_5$monthid[ga_filled_4$date ==  "2014/09/01"] <- 9
ga_filled_5$monthid[ga_filled_4$date ==  "2014/10/01"] <- 10
ga_filled_5$monthid[ga_filled_4$date ==  "2014/11/01"] <- 11
ga_filled_5$monthid[ga_filled_4$date ==  "2014/12/01"] <- 12
ga_filled_5$monthid[ga_filled_4$date ==  "2015/01/01"] <- 13
ga_filled_5$monthid[ga_filled_4$date ==  "2015/02/01"] <- 14
ga_filled_5$monthid[ga_filled_4$date ==  "2015/03/01"] <- 15
ga_filled_5$monthid[ga_filled_4$date ==  "2015/04/01"] <- 16
ga_filled_5$monthid[ga_filled_4$date ==  "2015/05/01"] <- 17
ga_filled_5$monthid[ga_filled_4$date ==  "2015/06/01"] <- 18
ga_filled_5$monthid[ga_filled_4$date ==  "2015/07/01"] <- 19
ga_filled_5$monthid[ga_filled_4$date ==  "2015/08/01"] <- 20
ga_filled_5$monthid[ga_filled_4$date ==  "2015/09/01"] <- 21
ga_filled_5$monthid[ga_filled_4$date ==  "2015/10/01"] <- 22
ga_filled_5$monthid[ga_filled_4$date ==  "2015/11/01"] <- 23
ga_filled_5$monthid[ga_filled_4$date ==  "2015/12/01"] <- 24
ga_filled_5$monthid[ga_filled_4$date ==  "2016/01/01"] <- 25
ga_filled_5$monthid[ga_filled_4$date ==  "2016/02/01"] <- 26
ga_filled_5$monthid[ga_filled_4$date ==  "2016/03/01"] <- 27
ga_filled_5$monthid[ga_filled_4$date ==  "2016/04/01"] <- 28
ga_filled_5$monthid[ga_filled_4$date ==  "2016/05/01"] <- 29
ga_filled_5$monthid[ga_filled_4$date ==  "2016/06/01"] <- 30
ga_filled_5$monthid[ga_filled_4$date ==  "2016/07/01"] <- 31
ga_filled_5$monthid[ga_filled_4$date ==  "2016/08/01"] <- 32
ga_filled_5$monthid[ga_filled_4$date ==  "2016/09/01"] <- 33
ga_filled_5$monthid[ga_filled_4$date ==  "2016/10/01"] <- 34
ga_filled_5$monthid[ga_filled_4$date ==  "2016/11/01"] <- 35
ga_filled_5$monthid[ga_filled_4$date ==  "2016/12/01"] <- 36
ga_filled_5$monthid[ga_filled_4$date ==  "2017/01/01"] <- 37
ga_filled_5$monthid[ga_filled_4$date ==  "2017/02/01"] <- 38
ga_filled_5$monthid[ga_filled_4$date ==  "2017/03/01"] <- 39
ga_filled_5$monthid[ga_filled_4$date ==  "2017/04/01"] <- 40
ga_filled_5$monthid[ga_filled_4$date ==  "2017/05/01"] <- 41
ga_filled_5$monthid[ga_filled_4$date ==  "2017/06/01"] <- 42
ga_filled_5$monthid[ga_filled_4$date ==  "2017/07/01"] <- 43
ga_filled_5$monthid[ga_filled_4$date ==  "2017/08/01"] <- 44
ga_filled_5$monthid[ga_filled_4$date ==  "2017/09/01"] <- 45
ga_filled_5$monthid[ga_filled_4$date ==  "2017/10/01"] <- 46
ga_filled_5$monthid[ga_filled_4$date ==  "2017/11/01"] <- 47
ga_filled_5$monthid[ga_filled_4$date ==  "2017/12/01"] <- 48
ga_filled_5$monthid[ga_filled_4$date ==  "2018/01/01"] <- 49
ga_filled_5$monthid[ga_filled_4$date ==  "2018/02/01"] <- 50
ga_filled_5$monthid[ga_filled_4$date ==  "2018/03/01"] <- 51
ga_filled_5$monthid[ga_filled_4$date ==  "2018/04/01"] <- 52
ga_filled_5$monthid[ga_filled_4$date ==  "2018/05/01"] <- 53
ga_filled_5$monthid[ga_filled_4$date ==  "2018/06/01"] <- 54
ga_filled_5$monthid[ga_filled_4$date ==  "2018/07/01"] <- 55
ga_filled_5$monthid[ga_filled_4$date ==  "2018/08/01"] <- 56
ga_filled_5$monthid[ga_filled_4$date ==  "2018/09/01"] <- 57
ga_filled_5$monthid[ga_filled_4$date ==  "2018/10/01"] <- 58
ga_filled_5$monthid[ga_filled_4$date ==  "2018/11/01"] <- 59
ga_filled_5$monthid[ga_filled_4$date ==  "2018/12/01"] <- 60
ga_filled_5$monthid[ga_filled_4$date ==  "2019/01/01"] <- 61
ga_filled_5$monthid[ga_filled_4$date ==  "2019/02/01"] <- 62
ga_filled_5$monthid[ga_filled_4$date ==  "2019/03/01"] <- 63
ga_filled_5$monthid[ga_filled_4$date ==  "2019/04/01"] <- 64
ga_filled_5$monthid[ga_filled_4$date ==  "2019/05/01"] <- 65
ga_filled_5$monthid[ga_filled_4$date ==  "2019/06/01"] <- 66
ga_filled_5$monthid[ga_filled_4$date ==  "2019/07/01"] <- 67
ga_filled_5$monthid[ga_filled_4$date ==  "2019/08/01"] <- 68
ga_filled_5$monthid[ga_filled_4$date ==  "2019/09/01"] <- 69
ga_filled_5$monthid[ga_filled_4$date ==  "2019/10/01"] <- 70
ga_filled_5$monthid[ga_filled_4$date ==  "2019/11/01"] <- 71
ga_filled_5$monthid[ga_filled_4$date ==  "2019/12/01"] <- 72


# How many monthid's are missing after interpolation code? none missing
table(ga_filled_5$monthid, useNA = "always")
# All n = 73 health facilities have have 72 monthids each

#---------------------------------------------------------------------------------------------------------------------------------
#Create Facet Wrap for proportions of case counts by health facility (monthid x-axis, case counts category on y-axis)
#Plot proportion of case counts by month for 2014-2020
#MODIFIED ON 7-4-20
#This time, DO NOT include grey outlines around the bar as Ruth suggested in Haiti_GA_its_9_27_19 LVDH RA WE.pptx on 9-27-19
#---------------------------------------------------------------------------------------------------------------------------------

# subset for viewing - only show MZ_hf_code, monthid, and case count category
myvars <- c("MZ_HF_Code", "monthid", "casectcat")
ga_filled_5_slim <- ga_filled_5[myvars]

# Convert ga_filled_7_slim to a dataframe, n = 5,256 obs
ga_filled_6_slim <- as.data.frame(ga_filled_5_slim)

# remove 2014 from dataset for visualization, n = 4,380 obs
ga_filled_7_slim<-ga_filled_6_slim[!(ga_filled_6_slim$monthid == 1:12),]

# View histogram for ga_filled_5 data set -----------------
hist(ga_filled_5$ConfirmedcasesMonthlytotal) # view histogram for all departments combined
summary(ga_filled_5$ConfirmedcasesMonthlytotal)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    2.00    9.97    8.00  461.00    1588
vc = ga_filled_5$ConfirmedcasesMonthlytotal
var(vc, na.rm = TRUE)
# [1] 654.7471 conditional mean not equal to conditional variance (?)
sd(ga_filled_5$ConfirmedcasesMonthlytotal, na.rm = TRUE)
# [1] 25.58803
# Filled Density Plot
density_plot <- density(ga_filled_5$ConfirmedcasesMonthlytotal, na.rm = TRUE)
plot(density_plot, main="Kernel Density of Confirmed Cases")
polygon(density_plot, col="lightblue", border="blue")

# Subset ga_filled_5 so that only contains n = 44 hf for analysis  --------
# n = 3,096
# n = 44 health facilities
ga_43 <- ga_filled_5[ which(ga_filled_5$MZ_HF_Code !=  "MZ_1010" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1018" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1033" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1069" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1072" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1074" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1077" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1082" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1089" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1146" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1151" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1163" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1187" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1188" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1189" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_1190" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_46" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_64" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_719" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_727" & # excluded due to inaccurate GPS (?)
                                ga_filled_5$MZ_HF_Code !=  "MZ_729" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_730" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_731" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_78" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_882" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_900" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_905" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_922" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_952" &
                                ga_filled_5$MZ_HF_Code !=  "MZ_994") , ]

# Verify -----------
# unique(ga_44$MZ_HF_Code, useNA = "always")
# [1] "MZ_47"  "MZ_683" "MZ_684" "MZ_685" "MZ_686" "MZ_687" "MZ_688" "MZ_689" "MZ_690" "MZ_691" "MZ_692" "MZ_693" "MZ_694" "MZ_695" "MZ_696" "MZ_697" "MZ_698" "MZ_699" "MZ_700"
# [20] "MZ_701" "MZ_702" "MZ_703" "MZ_705" "MZ_706" "MZ_707" "MZ_708" "MZ_709" "MZ_710" "MZ_711" "MZ_712" "MZ_713" "MZ_714" "MZ_715" "MZ_717" "MZ_718" "MZ_720" "MZ_721" "MZ_722"
# [39] "MZ_723" "MZ_724" "MZ_725" "MZ_726" "MZ_728" "MZ_729"

# How many obs are missing case count data?
table(ga_43$ConfirmedcasesMonthlytotal, useNA = "always")
# n = 109

# show observations that are missing case count data
# n = 109 obs
# NOTE: @ this point it's ok if month's 1-12 are missing data since 
# 2014 data will not be included in ITS analysis
ga_miss_case <- ga_43 %>% filter(is.na(ConfirmedcasesMonthlytotal))

# show slimmer ga_miss_case for easier visualization for interpolation
v2 <- c("MZ_HF_Code", "monthid", "ConfirmedcasesMonthlytotal")
ga_miss_case_slim <- ga_miss_case[v2]
View(ga_miss_case_slim)
# MZ_683 MZ_684 MZ_685 MZ_690 MZ_691 MZ_692 MZ_697 MZ_699 MZ_700 MZ_702 MZ_705 MZ_707 MZ_708 MZ_712 MZ_714 MZ_715 MZ_718 MZ_722 MZ_723 MZ_724 MZ_726 MZ_728 
# 10      2     13      4      7      1      2      2      1      2      2      1      1      1      4      6     22     10      2      5      1     10 
# n = 22 hfs require case count interpolation

# Perform interpolation of case count data for missing
# n = 3,096
ga_43_interp <- ga_43 %>%
    group_by(MZ_HF_Code) %>%
    mutate(ConfirmedcasesMonthlytotal_interp = na.approx(ConfirmedcasesMonthlytotal, na.rm=FALSE))  

# Verify interpolation worked correctly
v3 <- c("MZ_HF_Code", "monthid", "ConfirmedcasesMonthlytotal", "ConfirmedcasesMonthlytotal_interp")
ga_43_interp_slim <- ga_43_interp[v3]
View(ga_43_interp_slim)

# Any missing values after interpolation?
table(ga_43_interp_slim$ConfirmedcasesMonthlytotal_interp, useNA = "always") 
#n = 3, MZ_683 month 1 and 2 have missing values, but no worries b/c 
# yr. 2014 is not included in anlaysis
# post_interp_missing <- ga_43_interp_slim %>% filter(is.na(ConfirmedcasesMonthlytotal_interp))
# why is MZ_728 missing monthid 72 confirmed case count interp?

mz_728_hf <- ga_43_interp_slim %>% filter(MZ_HF_Code=="MZ_728")

table(ga_43_interp_slim$ConfirmedcasesMonthlytotal_interp, useNA = "always") 

# Round to nearest integer
ga_master_43 <- ga_43_interp
ga_master_43$malaria_cases <- ceiling(ga_43_interp_slim$ConfirmedcasesMonthlytotal_interp) 

# fill in zero cases for mz_728 monthid = 72
ga_master_43$malaria_cases[ga_master_43$MZ_HF_Code == "MZ_728" & ga_master_43$monthid == 72] <-0

# How many obs are missing month and year? 
table(ga_master_43$Month, useNA = "always") # n = 91 obs
table(ga_master_43$Year, useNA = "always") # n = 91 obs

# recode to fill in Month variable (manually)
ga_master_43$Month[ga_master_43$date ==  "2014/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2014/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2014/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2014/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2014/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2014/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2014/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2014/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2014/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2014/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2014/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2014/12/01"] <- 12
ga_master_43$Month[ga_master_43$date ==  "2015/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2015/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2015/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2015/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2015/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2015/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2015/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2015/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2015/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2015/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2015/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2015/12/01"] <- 12
ga_master_43$Month[ga_master_43$date ==  "2016/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2016/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2016/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2016/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2016/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2016/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2016/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2016/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2016/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2016/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2016/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2016/12/01"] <- 12
ga_master_43$Month[ga_master_43$date ==  "2017/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2017/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2017/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2017/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2017/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2017/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2017/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2017/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2017/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2017/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2017/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2017/12/01"] <- 12
ga_master_43$Month[ga_master_43$date ==  "2018/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2018/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2018/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2018/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2018/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2018/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2018/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2018/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2018/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2018/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2018/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2018/12/01"] <- 12
ga_master_43$Month[ga_master_43$date ==  "2019/01/01"] <- 1
ga_master_43$Month[ga_master_43$date ==  "2019/02/01"] <- 2
ga_master_43$Month[ga_master_43$date ==  "2019/03/01"] <- 3
ga_master_43$Month[ga_master_43$date ==  "2019/04/01"] <- 4
ga_master_43$Month[ga_master_43$date ==  "2019/05/01"] <- 5
ga_master_43$Month[ga_master_43$date ==  "2019/06/01"] <- 6
ga_master_43$Month[ga_master_43$date ==  "2019/07/01"] <- 7
ga_master_43$Month[ga_master_43$date ==  "2019/08/01"] <- 8
ga_master_43$Month[ga_master_43$date ==  "2019/09/01"] <- 9
ga_master_43$Month[ga_master_43$date ==  "2019/10/01"] <- 10
ga_master_43$Month[ga_master_43$date ==  "2019/11/01"] <- 11
ga_master_43$Month[ga_master_43$date ==  "2019/12/01"] <- 12

# Did Month filling work appropriately
table(ga_master_43$Month, useNA = "always") # correct amount for 43 facilities x 6 years
#   1    2    3    4    5    6    7    8    9   10   11   12 <NA> 
# 258  258  258  258  258  258  258  258  258  258  258  258    0 

# Fill in missing years
ga_master_43$Year[ga_master_43$date ==  "2014/01/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/02/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/03/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/04/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/05/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/06/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/07/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/08/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/09/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/10/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/11/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2014/12/01"] <- 2014
ga_master_43$Year[ga_master_43$date ==  "2015/01/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/02/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/03/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/04/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/05/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/06/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/07/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/08/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/09/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/10/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/11/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2015/12/01"] <- 2015
ga_master_43$Year[ga_master_43$date ==  "2016/01/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/02/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/03/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/04/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/05/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/06/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/07/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/08/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/09/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/10/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/11/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2016/12/01"] <- 2016
ga_master_43$Year[ga_master_43$date ==  "2017/01/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/02/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/03/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/04/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/05/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/06/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/07/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/08/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/09/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/10/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/11/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2017/12/01"] <- 2017
ga_master_43$Year[ga_master_43$date ==  "2018/01/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/02/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/03/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/04/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/05/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/06/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/07/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/08/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/09/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/10/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/11/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2018/12/01"] <- 2018
ga_master_43$Year[ga_master_43$date ==  "2019/01/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/02/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/03/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/04/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/05/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/06/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/07/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/08/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/09/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/10/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/11/01"] <- 2019
ga_master_43$Year[ga_master_43$date ==  "2019/12/01"] <- 2019

# Did Year filling work appropriately -------
table(ga_master_43$Year, useNA = "always") # correct amount for 43 facilities x 12 months
# 2014 2015 2016 2017 2018 2019 <NA> 
#  516  516  516  516  516  516    0 

# Create t since llin itvn
ga_master_43_2 <- ga_master_43
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 1] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 2] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 3] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 4] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 5] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 6] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 7] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 8] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 9] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 10] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 11] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 12] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 13] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 14] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 15] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 16] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 17] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 18] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 19] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 20] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 21] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 22] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 23] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 24] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 25] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 26] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 27] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 28] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 29] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 30] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 31] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 32] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 33] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 34] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 35] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 36] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 37] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 38] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 39] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 40] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 41] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 42] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 43] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 44] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 45] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 46] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 47] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 48] <- 0
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 49] <- 1
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 50] <- 2
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 51] <- 3
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 52] <- 4
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 53] <- 5
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 54] <- 6
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 55] <- 7
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 56] <- 8
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 57] <- 9
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 58] <- 10
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 59] <- 11
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 60] <- 12
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 61] <- 13
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 62] <- 14
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 63] <- 15
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 64] <- 16
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 65] <- 17
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 66] <- 18
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 67] <- 19
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 68] <- 20
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 69] <- 21
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 70] <- 22
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 71] <- 23
ga_master_43_2$t_since_llin[ga_master_43_2$monthid == 72] <- 24

# ga_master_43_2$t_since_llin <- ceiling(ga_43_interp_slim$ConfirmedcasesMonthlytotal_interp) 


###############
##### 
# Messed up this section on 7-6-20, remedied on 7-8-20


# create lagged case count variable ----------------
ga_master_43_3 <- 
    ga_master_43_2 %>%
    group_by(MZ_HF_Code) %>%
    mutate(casect_lag = dplyr::lag(malaria_cases, n = 1, default = NA))

# create square root of lagged case ct variable ----------------
# ga_master_43_4$ga_master_43_2[ga_master_43_2$monthid == 1] <- 
ga_master_43_4 <- ga_master_43_3 # create new data frame
ga_master_43_4$sqrt_lag_casect <- sqrt(ga_master_43_3$casect_lag)
    
# round to nearest integer?    
# likely not needed for autocorrelation adjustment
# ga_master_43_2$t_since_llin <- ceiling(ga_43_interp_slim$ConfirmedcasesMonthlytotal_interp) 

# make previous population data set slimmer
myvars <- c("MZ_hf_code", "MONTHID", "med_pop")
med_pop <- haiti_prelim_2015_2018_9_24_19[myvars]
View(med_pop)

# change variable names in med_pop data set for merge
med_pop_2 <- med_pop %>% 
    rename(monthid = MONTHID,
        MZ_HF_Code = MZ_hf_code)

# sort data frames by MZ_HF_Code and monthid
ga_master_43_4_order <- ga_master_43_4[order(ga_master_43_4$MZ_HF_Code, 
                                             ga_master_43_4$monthid),]
med_pop_2_order <- med_pop_2[order(med_pop_2$MZ_HF_Code, 
                                   med_pop_2$monthid),]

# how many obs have pop data in med_pop_2_order
table(med_pop_2_order$med_pop, useNA = "always")

# merge datasets
# n = 3,096 obs
ga_master_merge <- merge(ga_master_43_4_order,med_pop_2_order,by=c("MZ_HF_Code","monthid"), all.x = TRUE)

# calculate 1 moth lagged med_pop variable
# n = 3,096
ga_master_merge_2 <- 
    ga_master_merge %>%
    group_by(MZ_HF_Code) %>%
    mutate(med_pop_lag = dplyr::lag(med_pop, n = 1, default = NA))

# slim data set for transposing
myvars2 <- c("MZ_HF_Code", "monthid", "med_pop")
ga_master_merge_2_slim <- ga_master_merge_2[myvars2]
View(ga_master_merge_2_slim)

# melted <- melt(ga_master_merge_2_slim, id=c("MZ_HF_Code","monthid"))
# View(melted)

##--TEST reshape from wide to long # master_wide <- 
ga_master_wide <-  as.data.frame(ga_master_merge_2_slim %>% pivot_wider(names_from = "monthid",  values_from = "med_pop")) # "MZ_HF_Code",
                                       

# export to calculate population growth rate in excel
# TASK: Do this in R!!! Next and ensure proper growth rate applied
# write.csv(ga_master_merge_2, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/Growth rate application dataset/ga_master_merge_2.csv")

# fill in med_pop using growth rate 1.001^x
# IF MONTHID=4 THEN Med_pop = Med_pop_2014 *(1.001**3);
# ga_14_19_slim$time[ga_master_merge$date == 2015 & ga_master_merge$Month == 2] <- "2015/02/01"
# 
# ga_master_merge$med_pop_end_2018 <- [ga-master_merge_2$MZ_HF_Code == "MZ_
# 
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/01/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/02/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/03/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/04/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/05/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/06/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/07/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/08/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/09/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/10/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/11/01"] <- 2019
# ga_master_merge$med_pop[ga_master_merge$date ==  "2019/12/01"] <- 2019

#################################################
# Redo population growth rate for 2019
################################################

# bring in dataset for its analysis with 2019 population carried forward from last 2018 entry. will redo later today for meeting
master_ga_2019_v1 <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/Growth rate application dataset/ga_master_merge_2_last_pop_carried_forward_2019.csv")

# Drop 2014 data
# n = 2580, GTG
ga_2015_2019 <- master_ga_2019_v1[ which(master_ga_2019_v1$Year !=  2014)]
ga_2015_2019 <- master_ga_2019_v1 %>% filter(Year == 2015 | Year == 2016| Year == 2017| Year == 2018| Year == 2019) 

# create pre itvn period and post intervention pd variable for tmda_irs

ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 1] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 2] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 3] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 4] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 5] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 6] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 7] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 8] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 9] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 10] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 11] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 12] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 13] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 14] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 15] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 16] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 17] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 18] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 19] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 20] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 21] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 22] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 23] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 24] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 25] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 26] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 27] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 28] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 29] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 30] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 31] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 32] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 33] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 34] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 35] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 36] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 37] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 38] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 39] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 40] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 41] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 42] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 43] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 44] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 45] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 46] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 47] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 48] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 49] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 50] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 51] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 52] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 53] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 54] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 55] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 56] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 57] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 58] <- 0
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 59] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 60] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 61] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 62] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 63] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 64] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 65] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 66] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 67] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 68] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 69] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 70] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 71] <- 1
ga_2015_2019$tmda_irs[ga_2015_2019$monthid == 72] <- 1



# Export the dataset to perform analysis in SATA
write.csv(ga_2015_2019, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/ga_2015_2019_ITS.csv")

#####################################################################################################################
# Started off here on 7-8-20 took its analysis data set from preliminary its analysis presentation with Ruth and Thom
#####################################################################################################################
# 7-8-20 Bring data set back in to continue with data management/addition of rainfall data and modification of tmda_irs slope term and level change term
ga_2015_2019_w_rf <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/ga_2015_2019_ITS.csv")
# Bring in rainfall data
rf_2014_2019 <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/GIS Files/Rainfall/2014-2019/CHIRPS_rf_hf_point_2014_2019.csv")
# create pre itvn period and post intervention pd variable for tmda_irs

ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 1] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 2] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 3] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 4] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 5] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 6] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 7] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 8] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 9] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 10] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 11] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 12] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 13] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 14] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 15] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 16] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 17] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 18] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 19] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 20] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 21] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 22] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 23] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 24] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 25] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 26] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 27] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 28] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 29] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 30] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 31] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 32] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 33] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 34] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 35] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 36] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 37] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 38] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 39] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 40] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 41] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 42] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 43] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 44] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 45] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 46] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 47] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 48] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 49] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 50] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 51] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 52] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 53] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 54] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 55] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 56] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 57] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 58] <- 0
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 59] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 60] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 61] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 62] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 63] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 64] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 65] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 66] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 67] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 68] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 69] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 70] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 71] <- 1
ga_2015_2019_w_rf$tmda_irs[ga_2015_2019_w_rf$monthid == 72] <- 1

# Create tmda & irs slope term/variable -----------------
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 1] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 2] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 3] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 4] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 5] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 6] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 7] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 8] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 9] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 10] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 11] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 12] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 13] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 14] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 15] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 16] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 17] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 18] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 19] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 20] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 21] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 22] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 23] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 24] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 25] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 26] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 27] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 28] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 29] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 30] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 31] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 32] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 33] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 34] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 35] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 36] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 37] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 38] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 39] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 40] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 41] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 42] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 43] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 44] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 45] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 46] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 47] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 48] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 49] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 50] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 51] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 52] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 53] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 54] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 55] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 56] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 57] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 58] <- 0
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 59] <- 1
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 60] <- 2
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 61] <- 3
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 62] <- 4
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 63] <- 5
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 64] <- 6
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 65] <- 7
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 66] <- 8
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 67] <- 9
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 68] <- 10
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 69] <- 11
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 70] <- 12
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 71] <- 13
ga_2015_2019_w_rf$t_since_mda_irs[ga_2015_2019_w_rf$monthid == 72] <- 14

# Create monthid variable in rainfall data for merge -------------------------------------------------
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 1] <- 1
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 2] <- 2
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 3] <- 3
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 4] <- 4
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 5] <- 5
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 6] <- 6
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 7] <- 7
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 8] <- 8
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 9] <- 9
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 10] <- 10
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 11] <- 11
rf_2014_2019$monthid[rf_2014_2019$year == 2014 & rf_2014_2019$month == 12] <- 12
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 1] <- 13
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 2] <- 14
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 3] <- 15
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 4] <- 16
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 5] <- 17
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 6] <- 18
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 7] <- 19
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 8] <- 20
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 9] <- 21
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 10] <- 22
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 11] <- 23
rf_2014_2019$monthid[rf_2014_2019$year == 2015 & rf_2014_2019$month == 12] <- 24
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 1] <- 25
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 2] <- 26
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 3] <- 27
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 4] <- 28
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 5] <- 29
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 6] <- 30
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 7] <- 31
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 8] <- 32
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 9] <- 33
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 10] <- 34
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 11] <- 35
rf_2014_2019$monthid[rf_2014_2019$year == 2016 & rf_2014_2019$month == 12] <- 36
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 1] <- 37
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 2] <- 38
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 3] <- 39
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 4] <- 40
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 5] <- 41
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 6] <- 42
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 7] <- 43
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 8] <- 44
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 9] <- 45
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 10] <- 46
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 11] <- 47
rf_2014_2019$monthid[rf_2014_2019$year == 2017 & rf_2014_2019$month == 12] <- 48
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 1] <- 49
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 2] <- 50
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 3] <- 51
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 4] <- 52
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 5] <- 53
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 6] <- 54
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 7] <- 55
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 8] <- 56
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 9] <- 57
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 10] <- 58
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 11] <- 59
rf_2014_2019$monthid[rf_2014_2019$year == 2018 & rf_2014_2019$month == 12] <- 60
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 1] <- 61
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 2] <- 62
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 3] <- 63
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 4] <- 64
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 5] <- 65
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 6] <- 66
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 7] <- 67
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 8] <- 68
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 9] <- 69
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 10] <- 70
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 11] <- 71
rf_2014_2019$monthid[rf_2014_2019$year == 2019 & rf_2014_2019$month == 12] <- 72

# lag rainfall by 2 months
rf_dataset <- 
    rf_2014_2019 %>%
    group_by(MZ_hf_code) %>%
    mutate(lag_2_rf = dplyr::lag(rf, n = 2, default = NA))

# sort rainfall dataset by mz_hf_code the monthid
rf_ordered <- rf_dataset[order(rf_dataset$MZ_hf_code, 
                               rf_dataset$monthid),]

# change variable names in rf_ordered data set for merge
rf_ready <- rf_ordered %>% 
    rename(MZ_HF_Code = MZ_hf_code)

# remove 2014 rainfall obs
rf_ready_2015_2019 <- rf_ready[!(rf_ready$monthid == 1:12),]

# remove unnecessary facilities
rf_slim <- rf_ready_2015_2019[ which(rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1010" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1018" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1033" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1069" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1072" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1074" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1077" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1082" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1089" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1146" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1151" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1163" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1187" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1188" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1189" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_1190" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_46" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_64" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_719" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_727" & # excluded due to inaccurate GPS (?)
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_729" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_730" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_731" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_78" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_882" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_900" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_905" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_922" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_952" &
                                       rf_ready_2015_2019$MZ_HF_Code !=  "MZ_994") , ]

# merge datasets
# n = 2,580
ga_master_rf <- merge(ga_2015_2019_w_rf,rf_slim,by=c("MZ_HF_Code","monthid"), all.x = TRUE)

# Create t since llin itvn
ga_master_rf$t_since_llin[ga_master_rf$monthid == 1] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 2] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 3] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 4] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 5] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 6] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 7] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 8] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 9] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 10] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 11] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 12] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 13] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 14] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 15] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 16] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 17] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 18] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 19] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 20] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 21] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 22] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 23] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 24] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 25] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 26] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 27] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 28] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 29] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 30] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 31] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 32] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 33] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 34] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 35] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 36] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 37] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 38] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 39] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 40] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 41] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 42] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 43] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 44] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 45] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 46] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 47] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 48] <- 0
ga_master_rf$t_since_llin[ga_master_rf$monthid == 49] <- 1
ga_master_rf$t_since_llin[ga_master_rf$monthid == 50] <- 2
ga_master_rf$t_since_llin[ga_master_rf$monthid == 51] <- 3
ga_master_rf$t_since_llin[ga_master_rf$monthid == 52] <- 4
ga_master_rf$t_since_llin[ga_master_rf$monthid == 53] <- 5
ga_master_rf$t_since_llin[ga_master_rf$monthid == 54] <- 6
ga_master_rf$t_since_llin[ga_master_rf$monthid == 55] <- 7
ga_master_rf$t_since_llin[ga_master_rf$monthid == 56] <- 8
ga_master_rf$t_since_llin[ga_master_rf$monthid == 57] <- 9
ga_master_rf$t_since_llin[ga_master_rf$monthid == 58] <- 10
ga_master_rf$t_since_llin[ga_master_rf$monthid == 59] <- 11
ga_master_rf$t_since_llin[ga_master_rf$monthid == 60] <- 12
ga_master_rf$t_since_llin[ga_master_rf$monthid == 61] <- 13
ga_master_rf$t_since_llin[ga_master_rf$monthid == 62] <- 14
ga_master_rf$t_since_llin[ga_master_rf$monthid == 63] <- 15
ga_master_rf$t_since_llin[ga_master_rf$monthid == 64] <- 16
ga_master_rf$t_since_llin[ga_master_rf$monthid == 65] <- 17
ga_master_rf$t_since_llin[ga_master_rf$monthid == 66] <- 18
ga_master_rf$t_since_llin[ga_master_rf$monthid == 67] <- 19
ga_master_rf$t_since_llin[ga_master_rf$monthid == 68] <- 20
ga_master_rf$t_since_llin[ga_master_rf$monthid == 69] <- 21
ga_master_rf$t_since_llin[ga_master_rf$monthid == 70] <- 22
ga_master_rf$t_since_llin[ga_master_rf$monthid == 71] <- 23
ga_master_rf$t_since_llin[ga_master_rf$monthid == 72] <- 24


# Create binary variable for llin_itvn
ga_master_rf$llin[ga_master_rf$monthid == 1] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 2] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 3] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 4] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 5] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 6] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 7] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 8] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 9] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 10] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 11] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 12] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 13] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 14] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 15] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 16] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 17] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 18] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 19] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 20] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 21] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 22] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 23] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 24] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 25] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 26] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 27] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 28] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 29] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 30] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 31] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 32] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 33] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 34] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 35] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 36] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 37] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 38] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 39] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 40] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 41] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 42] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 43] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 44] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 45] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 46] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 47] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 48] <- 0
ga_master_rf$llin[ga_master_rf$monthid == 49] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 50] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 51] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 52] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 53] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 54] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 55] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 56] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 57] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 58] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 59] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 60] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 61] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 62] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 63] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 64] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 65] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 66] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 67] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 68] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 69] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 70] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 71] <- 1
ga_master_rf$llin[ga_master_rf$monthid == 72] <- 1

# Bring in old LLIN analysis dataset to obtain RDT stockout variable
rdt_stockout <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Final Dataset/haiti_prelim_2015_2018_9_24_19.csv")
# slim down
rdt_vars <- c("MZ_hf_code", "MONTHID", "RDT_stockout_new", "rdt_high_grp", "rdt_low_grp", "rdt_med_grp")
rdt_stockout_slim <- rdt_stockout[rdt_vars]

#rename variables in rdt stockout datset for merge
rdt_stout_slim <- rdt_stockout_slim %>% 
    rename(monthid = MONTHID,
           MZ_HF_Code = MZ_hf_code)

# slim ga_master its slim data set prior to merge with rdt stockout data
myvars_its <- c("MZ_HF_Code", "monthid", "ADM1Name", "OUName_MSPP", "Month", 
                "Year", "Type_new_final", "malaria_cases", "casectcat",
                "incidence", "tmda_irs", "t_since_mda_irs", "itvn_grp",
                "sqrt_lag_casect", "lag_2_rf", "t_since_llin", "llin")
ga_master_its_slim <- ga_master_rf[myvars_its]

# merge stockout data with ga_master_its_slim
# merge datasets
# n = 2,580
ga_master_v3 <- merge(ga_master_its_slim, rdt_stout_slim, by=c("MZ_HF_Code","monthid"), all.x = TRUE)

# export to csv to manually interpolate rdt variable by group
write.csv(ga_master_v3, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/ga_master_v3_2.csv")

# bring in ga_master_v4_2 and then merge with med_pop
ga_master_v4_2 <- read.csv("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/ga_master_v4_2.csv")

# slim down ____ and merge med pop with ga_master_v4_2
med_pop_vars <- c("MZ_HF_Code", "monthid", "med_pop")
med_pop_data <- ga_master_rf[med_pop_vars]

# merge med pop data with ga_master_v4_2.csv
ga_master_5 <- merge(ga_master_v4_2, med_pop_data, by=c("MZ_HF_Code","monthid"), all.x = TRUE)

# export to stata for analysis
write.csv(ga_master_5, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/ga_master_5.csv")


#------------------------------------------------------------------
# Plot rainfall and case counts | No greyed out Nov-Dec 2017 area |
# Try facet wrap by year facet_wrap( ~ Year)                      |
#------------------------------------------------------------------
library(ggplot2)
All_MonCaseCount$itvn_grp_fac <- as.factor(All_MonCaseCount$itvn_grp)

# sum case counts (linearly interpolated) by month
All_MonCaseCount <- ga_master_5 %>% 
    group_by(monthid, Year, itvn_grp) %>% 
    summarise(SumMonthConfCaseCount = sum(malaria_cases, na.rm = TRUE))
All_MonCaseCount

write.csv(All_MonCaseCount, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/All_MonCaseCount.csv")



plot_all_casecounts_over_time <- ggplot() + 
    #geom_area(data=ga_master_5, size = 1.25, aes(x = MONTHID, y = rf* 2000 / 420, fill = "#a6cee3"), alpha = .5) +
    geom_line(data=All_MonCaseCount, size = 1.2, aes(x = monthid, y = SumMonthConfCaseCount, group=itvn_grp_fac, colour = itvn_grp_fac)) #+  
    #scale_fill_manual(values = c("#a6cee3"), labels = c("mean total rfe")) + # ensures correct blue shade is filled in for rainfall
    scale_color_manual(values=c("#756bb1"), labels = c("confirmed malaria cases")) +   # ensures correct purple line color is applied to case count line
    theme_classic() #+
    ggtitle("Total monthly confirmed malaria cases \n n =43 health facilities 2015 - 2019") +
    theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5), text=element_text(family="sans")) +
    labs(fill = "HFCA") +
    scale_x_continuous(
        expand = c(0,0),  #eliminate visual gaps (white space along x and y axis) in figure
        breaks = c(13:72),
        labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", 
                   "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))  + 
    ylab("Confirmed malaria case counts") +                                                               
    theme(axis.title.y = element_text(angle = 90, size = 20), text=element_text(family="sans")) +
    xlab("Time (Months)") +
    theme(axis.title.x = element_text(angle = 0, size = 20), text=element_text(family="sans"))  +
    #geom_vline(xintercept = 47, linetype='dashed', color = "#969696") + # add vertical lines for month 47 and 58, and 59 (Oct (Targeted MDA & IRS) /Nov 2018 - CHW placement for community case management)
    #annotate ("text", x = 43, y= 1700, label ="LLIN distribution\nNov 2017",
    #        size = 5, colour = "#252525", fontface = "bold") +
    scale_y_continuous( expand = c(0,0), # eliminate visual gaps in figures
                        ylab("Confirmed malaria cases") # ,
                        #sec.axis = sec_axis(~ . * 420 / 2000, name = "Mean total rfe (mm)"), limits = c(0,2000)) +
    theme(legend.position = c(0.12,.875), legend.text = element_text(size = 12), legend.title = element_blank(), legend.spacing.y = unit(0, "mm"), 
          legend.box.background = element_rect(fill='white'), 
          legend.background = element_blank(),
          strip.text = element_text(size = 20, face = "bold" ),
          strip.background =element_rect(fill="#d9d9d9")) +
    guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2)) # +
    #facet_wrap( ~ Year, scales = 'free_x')

    plot_all_casecounts_over_time  
ggsave(filename ="plot_all_casecounts_rf_lines_44_facetwrap.png", width =13.35, height=7.5, units ='in', dpi =320)


################
# ITS Analysis
################
# load the packages
library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(MASS)

# compute the standardized rates
ga_2015_2019$incidence <- with(ga_2015_2019, malaria_cases/med_pop*1000)
# start the plot, excluding the points and the x-axis
plot(ga_2015_2019$incidence,type="n",ylim=c(00,1280),xlab="Year", ylab="Incidence per 1000",
     bty="l",xaxt="n")
# shade the post intervention period grey
rect(36,0,60,300,col=grey(0.9),border=F)
# plot the observed rate for pre-intervention period
points(data$rate[data$smokban==0],cex=0.7)
#specify the x-axis (i.e. time units)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2002:2006)
# add a title
title("Sicily, 2002-2006")

#Poisson with the standardised population as an offset
model1 <- glm.nb(malaria_cases ~ offset(log(med_pop)) + monthid + monthid*tmda_irs + itvn_grp + itvn_grp*monthid + itvn_grp*tmda_irs*monthid + Month,  data=ga_2015_2019) # family=poisson,
summary(model1)
summary(model1)$dispersion
round(ci.lin(model1,Exp=T),3)

(est <- cbind(Estimate = coef(model1), confint(model1)))
exp(est) # exponentiate for IRR


# try with link = log
# glm.nb(formula = daysabs ~ math + prog, data = dat, init.theta = 1.032713156, 
#        link = log)
model1 <- glm.nb(malaria_cases ~ offset(log(med_pop)) + monthid + monthid*tmda_irs + itvn_grp + itvn_grp*monthid + itvn_grp*tmda_irs*monthid + Month,  data=ga_2015_2019) # family=poisson,



    

                                       
#----------------------------------------------
# LEFT OFF HERE ON 7-5-20 
# PLAN:
# 1) Remove 2014 from the figure below - done in ga_filled_7_slim dataset
# ga_filled_6_slim still contains 2014 observations
# 2) Modify scale_x_continuous line of code
#----------------------------------------------

# Set working drive for figures ---------------------------------------------------------------------------
setwd("/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Figures/Case ct by month by hf")

# Make faceted figure for case count category by month by health facility ---------------------------------
ggplot(ga_filled_7_slim, aes(x = monthid, fill = casectcat)) + 
    geom_bar(width=1, position = "fill") + 
    scale_x_continuous(expand = c(0,0),
                       limits=c(12,73),
                       breaks=c(18, 30, 42, 54, 66), 
                       labels=c("2015","2016", "2017", "2018", "2019")) +
    scale_fill_manual(values = c( "#67a9cf", "#a8ddb5", "#ef8a62"), labels = c(">=1", "0", "Missing")) +
    theme_bw()+
    theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1),
          axis.text.y = element_blank(),
          axis.ticks  = element_blank()) +
    labs(fill = "Case Count\nCategory", title = "Grand'Anse", subtitle = "Case count category by month by health facility,\n2015 - 2019", y=" ", x="Month") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
          plot.tag.position = c(.86, .96)) + 
    #labs(tag = "N = 73\nhealth facilities", size = 10) +
    facet_wrap( ~ MZ_HF_Code)

ggsave(filename ="haiti_case_ct_by_month_by_hf_2015_2019_no_n_hf_facet_wrap_new.png", width =13.35, height=7.5, units ='in', dpi =320)



# Remove health facilities that were removed from original ITS analysis
# plus a few others like 1187, 1188, 1189, 1190
# n = 3,081 obs
ga_master_2 <- ga_master %>%
    filter(!ga_master$MZ_HF_Code %in%
               c("MZ_1010", "MZ_1018", "MZ_1033", "MZ_1069", "MZ_1072", "MZ_1074", "MZ_1077", "MZ_1082", "MZ_1089", "MZ_1146",
                 "MZ_1151", "MZ_1163", "MZ_46", "MZ_64", "MZ_719", "MZ_729", "MZ_730", "MZ_731", "MZ_78", "MZ_882",
                 "MZ_900", "MZ_905", "MZ_922", "MZ_952", "MZ_994", "MZ_1187", "MZ_1188", "MZ_1189", "MZ_1190"
               ))

# Examing how many HFs are missing entries and require padding
# table(ga_master_2$MZ_HF_Code, useNA = "always")
# MZ_47 MZ_683 MZ_684 MZ_685 MZ_686 MZ_687 MZ_688 MZ_689 MZ_690 MZ_691 MZ_692 MZ_693 MZ_694 MZ_695 MZ_696 MZ_697 MZ_698 MZ_699 MZ_700 MZ_701 MZ_702 MZ_703 MZ_705 MZ_706 MZ_707 MZ_708 MZ_709 MZ_710 MZ_711 MZ_712 MZ_713 MZ_714 
# 72     63     70     59     72     72     72     72     68     67     71     72     72     72     72     70     72     70     71     73     71     72     70     72     71     71     72     72     72     71     72     68 
# MZ_715 MZ_717 MZ_718 MZ_720 MZ_721 MZ_722 MZ_723 MZ_724 MZ_725 MZ_726 MZ_727 MZ_728   <NA> 
#     66     72     56     72     72     69     70     68     72     71     71     66      0 

# pad data / thicken using padr  ------------------------------------------
# (currently, there are n 3245 obs. There should be 72 x 73 HFs = 3,168 obs in GA) 
ga_master_3$date <- as.Date(ga_master_3$time, "%Y/%m/%d") # change to appropriate date format for padding
ga_master_pad <- 
    ga_master_3 %>%
    pad(group = "MZ_HF_Code", by = "date")

# export csv to check padding manually -------
# Ensure no missing values at monthid = 1 or monthid = 72 of time series
write.csv(ga_master_pad, "/Users/willeaton/Box/Modeling for Malaria Zero/ITS Analysis/Data/Intermediate Files/Padded inspection dataset/ga_2019_ts_padded.csv")


# 9) 1 mo lag of sq root of # of confirmed malaria cases
# 10) # of facilities reporting data for the month - later
# 11) incidence - later
# 12) log of case counts
# 13) zonal statistics for rainfall - later

# Create lagged environ variables --------------------------
# 4) lagged rainfall variable (anomaly)
# 5) lagged evi variable (anomaly)
# 6) lagged lst variable (anomaly)

# Maps to create --------------------------------------------------------------
# 1) Haiti study region with health facilities included in LLIN ITS
# 1) Haiti study region with health facilities included in tMDA/IRS ITS
# 1) Haiti study region with health facilities included in tMDA/IRS
#    intervention group
# 1) Haiti study region with health facilities included in tMDA/IRS 
#    control group


# Figures to create -----------------------------------------------------
# 1) Replicate J. Landier et al., 2018 Figure 3 for time series 
# 2) Figure 1. Faceted maps (tmap) to show HF's included in analysis

# Tables to create
# 1) Replicate J. Landier et al., Table 3


# Wants-------------------------------------------------------------------
# 1) OPD data
# 2) creation of more variables - environ anom.