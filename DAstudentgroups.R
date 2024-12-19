
# This script pulls from the state data files and says which student groups lead a district 
# to be eligible for Differentiated Assistance, and which priority areas and subsequent indicators 
# lead to that. 

### Libraries -------

library(tidyverse)
library(here)
library(readxl)
library(googlesheets4)
library(janitor)

### Import Files -------

da2017 <- read_excel(here("data", "assistance-status.xls" ), range = "A5:V941", ) %>%
    mutate(Year = 2017) %>% clean_names()

da2018 <- read_excel(here("data", "assistancestatus18.xlsx" ), range = "A3:Z996")  %>%
    mutate(Year = 2018) %>% clean_names(parsing_option = 0)

da2019 <- read_excel(here("data", "assistancestatus19-rev.xlsx" ), sheet = "District and COE 2019" , range = "A6:AA1004")  %>%
    mutate(Year = 2019) %>% clean_names(parsing_option = 0)

da2022 <- read_excel(here("data", "assistancestatus22.xlsx" ), sheet = "District and COE 2022" , range = "A6:AA999")  %>%
    mutate(Year = 2022) %>% clean_names(parsing_option = 0)


da2023 <- read_excel(here("data", "assistancestatus23.xlsx" ), sheet = "District and COE 2023" , range = "A6:AD996")  %>%
    mutate(Year = 2023) %>% clean_names(parsing_option = 0)


da2024 <- read_excel(here("data", "assistancestatus24.xlsx" ), sheet = "District and COE 2024" , range = "A6:AD10043")  %>%
    mutate(Year = 2024) %>% clean_names(parsing_option = 0)





# dash.mry.da.2 <- read_excel(here("data","assistancestatus24.xlsx"),
#                             range = "A6:AD999",
#                             sheet = "District and COE 2024") %>%
#     filter(Countyname == "Monterey") %>%
#     pivot_longer(cols = ends_with("priorities")) %>%
#     mutate(indicator2 = case_when(value == "A" ~ "Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), and 6 (Suspensions)",
#                                   value == "B" ~ "Met Criteria in Priority Areas 4 (Academic Indicators) and 5 (Chronic Absenteeism and/or Graduation)",
#                                   value == "D" ~ "Met Criteria in Priority Areas 4 (Academic Indicators) and 6 (Suspensions)",
#                                   value == "C" ~ "Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation) and 6 (Suspensions)",
#                                   value =="E" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators) and 8 (College/Career)",
#                                   value =="F" ~ 	"Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation) and 8 (College/Career)",
#                                   value =="G" ~ 	"Met Criteria in Priority Areas 6 (Suspensions) and 8 (College/Career)",
#                                   value =="H" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), and 8 (College/Career)",
#                                   value =="I" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 6 (Suspensions), and 8 (College/Career)",
#                                   value =="J" ~ 	"Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation), 6 (Suspensions), and 8 (College/Career)",
#                                   value =="K" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), 6 (Suspensions), and 8 (College/Career)"
#     ),
#     studentgroup.long = case_when(name == "AApriorities"	~ "Black/African American",
#                                   name == "AIpriorities" ~	"American Indian or Alaska Native American",
#                                   name == "ASpriorities" ~	"Asian American",
#                                   name == "ELpriorities" ~	"English Learner",
#                                   name == "LTELpriorities" ~	"Long Term English Learner",
#                                   name == "FIpriorities" ~	"Filipino",
#                                   name == "FOSpriorities"	 ~ "Foster Youth",
#                                   name == "HIpriorities" ~	"Hispanic",
#                                   name == "HOMpriorities"	~ "Homeless",
#                                   name == "PIpriorities" ~	"Pacific Islander",
#                                   name == "SEDpriorities" ~	"Socioeconomically Disadvantaged",
#                                   name == "SWDpriorities" ~	"Students with Disabilities",
#                                   name == "TOMpriorities" ~	"Two or More Races",
#                                   name == "WHpriorities" ~	"White"
#     ),
#     cds = CDS
#     )





codes <- read_excel(here("data", "assistancestatus19-rev.xlsx" ), sheet = "Value" , range = "A4:B15") %>%
    mutate(Value = str_sub(Value,1,1))

da.all <- da2024 %>%
    bind_rows(da2023) %>%
    bind_rows(da2022) %>%
    bind_rows(da2019) %>%
    bind_rows(da2018) %>%
    bind_rows(da2017) %>%
    transmute(cds, leaname, year, gsoffered,
           aa = coalesce(aapriorities, aa_priorities),
           ai = coalesce(aipriorities, ai_priorities),
           as = coalesce(aspriorities, as_priorities),
           el = coalesce(elpriorities, el_priorities),
           fi = coalesce(fipriorities, fi_priorities),
           fos = coalesce(fospriorities, fos_priorities),
           hi = coalesce(hipriorities, hi_priorities),
           hom = coalesce(hompriorities, hom_priorities),
           mr = coalesce(tompriorities, mr_priorities),
           pi = coalesce(pipriorities, pi_priorities),
           sed = coalesce(sedpriorities, sed_priorities),
           swd = coalesce(swdpriorities, swd_priorities),
           wh = coalesce(whpriorities, wh_priorities)
           
           ) %>%
    pivot_longer(cols = c(aa:wh)) %>%
    mutate(value = na_if(value, "*")) %>%
    pivot_wider(names_from = year)

da.mry.2.5 <- da.all %>%
    select(-`2017`) %>%
#    filter(str_detect(leaname, "Soledad")) %>%
    mutate(num.years = rowSums(!is.na(.))-3) %>%
    filter(num.years >=2) %>%
    group_by(cds) %>%
    mutate(rows.max = max(row_number())) %>%
    ungroup() %>%
    filter(str_starts( cds,"27" ) ) %>%
    filter(rows.max >=2)

student.group.potential.3years <- da.all %>%
    select(-`2017`, -`2018`,-`2019`,) %>%
    #    filter(str_detect(leaname, "Soledad")) %>%
    mutate(num.years = rowSums(!is.na(.))-3) %>%
    filter(num.years >=2) %>%
    group_by(cds) %>%
    mutate(rows.max = max(row_number())) %>%
    ungroup() %>%
    filter(str_starts( cds,"27" ) )# %>%
 #   filter(rows.max >=2)








#### Spreadsheet of all DA over time -------


names<- da.all%>% select(cds,leaname) %>% distinct() %>% na.omit()



da.mry <- da2017 %>%
    bind_rows(da2018) %>%
    bind_rows(da2019) %>%
    bind_rows(da2022) %>%
    bind_rows(da2023) %>%
    transmute(cds, leaname, year,
              aa = coalesce(aapriorities, aa_priorities),
              ai = coalesce(aipriorities, ai_priorities),
              as = coalesce(aspriorities, as_priorities),
              el = coalesce(elpriorities, el_priorities),
              fi = coalesce(fipriorities, fi_priorities),
              fos = coalesce(fospriorities, fos_priorities),
              hi = coalesce(hipriorities, hi_priorities),
              hom = coalesce(hompriorities, hom_priorities),
              mr = coalesce(tompriorities, mr_priorities),
              pi = coalesce(pipriorities, pi_priorities),
              sed = coalesce(sedpriorities, sed_priorities),
              swd = coalesce(swdpriorities, swd_priorities),
              wh = coalesce(whpriorities, wh_priorities)
              
    ) %>%
    pivot_longer(cols = c(aa:wh)) %>%
    mutate(value = na_if(value, "*")) %>%
    filter(str_starts(cds,"27") ) %>%
    mutate(reason = case_match(value,
                               "A" ~ "Met in Priority Areas 4, 5, and 6",
                               "B" ~ "Met in Priority Areas 4 and 5",
                               "C" ~ "Met in Priority Areas 5 and 6",
                               "D" ~ "Met in Priority Areas 4 and 6",
                               "E" ~ 	"Met Criteria in Priority Areas 4 and 8",
                               "F" ~ 	"Met Criteria in Priority Areas 5 and 8",
                               "G" ~ 	"Met Criteria in Priority Areas 6 and 8",
                               "H" ~ 	"Met Criteria in Priority Areas 4, 5, and 8",
                               "I" ~ 	"Met Criteria in Priority Areas 4, 6, and 8",
                               "J" ~ 	"Met Criteria in Priority Areas 5, 6, and 8",
                               "K" ~ 	"Met Criteria in Priority Areas 4, 5, 6, and 8"
                               )
    ) %>%
    pivot_wider(id_cols = c(cds,name),names_from = year, values_from = reason) %>%
    mutate(num.years = rowSums(!is.na(.))-2) %>%
    filter(num.years >0) %>%
    left_join(names) %>%
 #   select(-value) %>%
    arrange(leaname, name)

write_sheet(da.mry, 
            "https://docs.google.com/spreadsheets/d/11p4b2ymZWkCvTbJnomrAM935fBarpuhKMLXLCyBgf1U/edit?gid=678188896#gid=678188896",
            sheet = "Sheet2")








####### older work -------




priors <- tribble(~ "Priority Area", ~ "Criteria", ~"Indicators",
                 "P4","Red on ELA and Math, Red on one and Orange on other for ELA Math, Red on ELPI", c("ela", "math", "elpi"),
                 "P5","Red on graduation or Red on chronic absenteeism", c("grad","chronic"),
                 "P6", "Red on Suspension", "susp",
                 "P8","Red on College/Career", "cci"
                 )


### Identify schools and priority areas ------

da.mry.grp <- da.all %>% 
    filter(Countyname == "Monterey") %>% 
    select(CDS, LEAname, Year, AApriorities:WHpriorities) %>% 
    pivot_longer(cols = AApriorities:WHpriorities ) %>%
    mutate(value = na_if(value,"*")) %>%
    na.omit() %>%
    mutate(name = str_remove(name, "priorities") ) %>% 
    left_join(codes, c("value" = "Value"))

write_rds(da.mry.grp, here("data", "da-mry-grp.rds"))

da.mry.grp.split <- da.mry.grp %>%
    mutate(P4 = if_else(str_detect(Description,"4") , TRUE,FALSE),
           P5 = if_else(str_detect(Description,"5") , TRUE,FALSE),
           P6 = if_else(str_detect(Description,"6") , TRUE,FALSE),
           P8 = if_else(str_detect(Description,"8") , TRUE,FALSE))  %>%
    pivot_longer(cols = P4:P8,
                 names_to = "Priority Area",
                 values_to = "Met",
                 names_repair = "unique") %>%
    filter(Met == TRUE) %>%
    left_join(priors)



# Determine list of indicators of interest

inders <- da.mry.grp.split %>%
    filter(CDS == cdscoder) %>%
    select(Indicators) %>%
    distinct() %>%
    unlist()

# Determine list of student groups of interest

grouper <- da.mry.grp %>%
    filter(CDS == cdscoder) %>%
    select(name) %>%
    distinct() %>%
    unlist()

### End --------



temp <- tbl(con, "DASH_ALL") %>%
    filter(#county_code == "27",
        #  districtname == "Salinas U High" ,
  #      CDScode == dist,
        #      YEAR == max(YEAR),
        rtype == "D",
  #      ind == indi,
        # studentgroup == "ALL"
        countyname == "Monterey",
    ) %>% 
    collect()


temp3 <- temp %>%
  select(districtname, DOCType) %>%
  distinct()
