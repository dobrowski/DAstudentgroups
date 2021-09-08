

# This script generates graphs for historic numbers for the subgroups and all students . 


### Libraries ------

library(MCOE)
library(tidyverse)
library(ggthemes)

options(scipen=999)

con <- MCOE::mcoe_sql_con()

            
cdscoder <- 27661590000000

graphit <- function(indi, groups){
    tbl(con, "DASH_ALL") %>%
        filter(#county_code == "27",
          #  districtname == "Salinas U High" ,
            CDScode == cdscoder,
            #      YEAR == max(YEAR),
            rtype == "D",
            ind == indi,
            # studentgroup == "ALL"
            countyname == "Monterey",
        ) %>% 
        collect() %>%
        filter(studentgroup %in% groups) %>%
        ggplot(aes(y = currstatus/100, group = studentgroup, x = year, color = studentgroup)) +
        geom_line() +
        geom_point() +
        mcoe_theme +
        scale_color_few() + 
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste0(indi, " Historical Rates "),
             #            subtitle = sub.title,
             caption = "CA Dashboard",
             color = "")
    
}


graphit("chronic",c("ALL", groups))


graphit("grad",c("ALL" ,groups))


graphit("susp",c("ALL",groups))




tbl(con, "DASH_ALL") %>%
    filter(#county_code == "27",
        #  districtname == "Salinas U High" ,
        #  cds = cdscode,
        #      YEAR == max(YEAR),
        rtype == "D",
        ind == indi,
        # studentgroup == "ALL"
        countyname == "Monterey",
    ) %>% 
    head(20) %>%
    collect() 
