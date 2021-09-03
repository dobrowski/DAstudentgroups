
library(tidyverse)
library(here)
library(readxl)



da2017 <- read_excel(here("data", "assistance-status.xls" ), range = "A5:V941") %>% mutate(Year = 2017)

da2018 <- read_excel(here("data", "assistancestatus18.xlsx" ), range = "A3:Z996")  %>% mutate(Year = 2018)

da2019 <- read_excel(here("data", "assistancestatus19-rev.xlsx" ), sheet = "District and COE 2019" , range = "A6:AA1004")  %>% mutate(Year = 2019)

codes <- read_excel(here("data", "assistancestatus19-rev.xlsx" ), sheet = "Value" , range = "A4:B15") %>%
    mutate(Value = str_sub(Value,1,1))

da.all <- da2019 %>% bind_rows(da2018) %>% bind_rows(da2017)


da.mry.grp <- da.all %>% 
    filter(Countyname == "Monterey") %>% 
    select(CDS, LEAname, Year, AApriorities:WHpriorities) %>% 
    pivot_longer(cols = AApriorities:WHpriorities ) %>%
    mutate(value = na_if(value,"*")) %>%
    na.omit() %>%
    mutate(name = str_remove(name, "priorities") ) %>% 
    left_join(codes, c("value" = "Value"))
