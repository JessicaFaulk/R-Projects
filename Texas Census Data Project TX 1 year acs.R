library(dplyr)
library(pivottabler)
library(tidyr)
library(forcats)
library(plotly)
library(lubridate)
library(tidycensus)

# get and add you API key here
census_api_key('add API key here', install=TRUE)

v19 <- load_variables(2019, "acs1", cache = TRUE)

pums_vars_2019 <- pums_variables %>% 
  filter(year == 2019, survey == "acs1")
pums_vars_2019 %>% 
  distinct(var_code, var_label, data_type, level)

tx_pums <- get_pums(
  variables = c("HHLANP", "ENG"),
  state = "tx",
  survey = "acs1",
  year = 2019,
  recode = TRUE,
  
)

tx_HHL_ENG<-tx_pums %>% 
  count(HHLANP_label, ENG, wt = PWGTP) %>%
  pivot_wider(names_from=c(ENG), values_from=n, values_fill=0)

tx_HHL_ENG<-tx_HHL_ENG %>% mutate(total_over5= `1`+`2`+`3`+`4`, less_than_well=`2`+`3`+`4`) %>%
  rename(very_well=`1`) %>%
  select(-c(`2`,`3`,`4`,`b`)) %>%
  arrange(desc(total_over5)) %>%
  drop_na(HHLANP_label)
tx_HHL_ENG_df<-as.data.frame (tx_HHL_ENG)
write.csv(tx_HHL_ENG_df,"~/Language Services Report FY 2021/data/Census Data - HHL/tx_HHL_ENG_df.csv", row.names = TRUE)
