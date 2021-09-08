
# This script identifies the population of the student group in 2019 and the estimated 
# number need to avoid DA this year if the group size remains the same. 


### Libraries ------

library(MCOE)
library(tidyverse)
library(glue)


con <- MCOE::mcoe_sql_con()

thresh <- tribble(
    ~ "ind", ~"threshold", ~"direction",
    "chronic", .2, "lower",
    "grad", .68, "higher",
    "cci", .099, "higher",
    "susp", .09, "lower",
    "elpi",.35, "higher"
)



cdscoder <- 27661590000000

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

chronic.sent <- chronic %>%
    mutate(sentence = paste0(studentgroup," =~ ", floor( currdenom*.20) ," / ", currdenom))



sentence <- function(indi, groups){
    tbl(con, "DASH_ALL") %>%
        filter(#county_code == "27",
            #  districtname == "Salinas U High" ,
            CDScode == cdscoder,
           # YEAR == max(YEAR),
            rtype == "D",
            ind == indi,
            # studentgroup == "ALL"
            countyname == "Monterey",
        ) %>% 
        collect() %>%
        filter(studentgroup %in% groups) %>%
        left_join(thresh) %>%
        mutate(sentence_short = paste0(studentgroup," =~ ",
                                if_else(direction == "lower", floor( currdenom*threshold), ceiling( currdenom*threshold) ) ,
                                 " / ", currdenom),
               sentence_full = paste0(studentgroup," should have ", direction, " than ",
                                      if_else(direction == "lower", floor( currdenom*threshold), ceiling( currdenom*threshold) ) ,
                                      " students qualify based on the count in 2019 of ", currdenom, " to not be in Red.")
                   ) %>%
        filter(year == max(year)) %>%
        select(ind, threshold ,sentence_short, sentence_full)

    
}


temp <- sentence("susp",c("EL","SWD", "HOM", "ALL")) 



### End ------
