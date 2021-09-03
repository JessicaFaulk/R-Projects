#Date: Sept 1, 2021
#Author: Jessica Faulk

#load libraries
library(dplyr)
library(pivottabler)
library(tidyr)
library(forcats)
library(plotly)
library(lubridate)
library(tidycensus)

# get and add you API key here
#census_api_key('ADD API KEY HERE', install=TRUE)

#load the variables using the tidycensus package for the 5 year ACS data from 2019
v19 <- load_variables(2019, "acs5", cache = TRUE)

#load the PUMS variable data for ACS 5 year in 2019
pums_vars_2019 <- pums_variables %>% 
  filter(year == 2019, survey == "acs5")
pums_vars_2019 %>% 
  distinct(var_code, var_label, data_type, level)



# Grouped Language Report

# load the pums data for TEXAS ACS 5 year data for Household language spoken (HHL) 
 #and for how well English is spoken (ENG). Recode=TRUE used to get the appropriate names
tx_pums <- get_pums(
  variables = c("HHL", "ENG"),
  state = "tx",
  survey = "acs5",
  year = 2019,
  recode = TRUE,
  
)

#take the Texas PUMS data and reshape it. Get counts.
tx_HHL_ENG<-tx_pums %>% 
  count(HHL_label, ENG, wt = PWGTP) %>%
  pivot_wider(names_from=c(ENG), values_from=n, values_fill=0)

#combine columns and rename, arrange by the total population and drop the HHL labels
tx_HHL_ENG<-tx_HHL_ENG %>% mutate(total_over5= `1`+`2`+`3`+`4`, less_than_well=`2`+`3`+`4`) %>%
  rename(very_well=`1`) %>%
  select(-c(`2`,`3`,`4`,`b`)) %>%
  arrange(desc(total_over5)) %>%
  drop_na(HHL_label)
View(tx_HHL_ENG)
tx_HHL_ENG_df<-as.data.frame (tx_HHL_ENG)

write.csv(tx_HHL_ENG_df,"~data/Census Data - HHL/tx5yr_HHL_ENG_df.csv", row.names = TRUE)

### Detailed Language report

# load the pums data for TEXAS ACS 5 year data for Household language spoken (HHL) 
#and for how well English is spoken (ENG). Recode=TRUE used to get the appropriate names
tx_pums <- get_pums(
  variables = c("HHL", "ENG"),
  state = "tx",
  survey = "acs5",
  year = 2019,
  recode = TRUE,
  
)


#take the Texas PUMS data and reshape it. Get counts.
tx_LANP_ENG<-tx_pums %>% 
  count(LANP, ENG, wt = PWGTP) %>%
  pivot_wider(names_from=c(ENG), values_from=n, values_fill=0)

#combine columns and rename, arrange by the total population and drop the HHL labels
tx_LANP_ENG<-tx_LANP_ENG %>% mutate(total_over5= `1`+`2`+`3`+`4`, less_than_well=`2`+`3`+`4`) %>%
  rename(very_well=`1`) %>%
  select(-c(`2`,`3`,`4`,`b`)) %>%
  arrange(desc(total_over5)) %>%
  drop_na(LANP_label)
View(tx_LANP_ENG)
tx_HHL_ENG_df<-as.data.frame (tx_LANP_ENG)
write.csv(tx_HHL_ENG_df,"~data/Census Data - HHL/tx5yr_HHL_ENG_detailed_df.csv", row.names = TRUE)
