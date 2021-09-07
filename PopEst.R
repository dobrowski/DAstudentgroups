
# This script identifies the population of the student group in 2019 and the estimated 
# number need to avoid DA this year if the group size remains the same. 


### Libraries ------

library(MCOE)
library(tidyverse)


con <- MCOE::mcoe_sql_con()



chronic <- tbl(con, "DASH_CHRONIC") %>%
    filter(#county_code == "27",
        districtname == "Salinas Union High" ,
        YEAR == max(YEAR),
        rtype == "D"
       # studentgroup == "ALL"
        #        CountyName == "Monterey",
        #        CharterSchool == "All",
        #           DASS == "All"
    ) %>% 
    collect()

### End ------
