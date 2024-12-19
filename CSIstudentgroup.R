
# This script pulls from the state data files and says which student groups lead a district 
# to be eligible for Differentiated Assistance, and which priority areas and subsequent indicators 
# lead to that. 

### Libraries -------

library(tidyverse)
library(here)
library(readxl)
library(MCOE)


options(scipen=999)

con <- MCOE::mcoe_sql_con()

sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"

codebook <- read_sheet(sheet_id,
                       col_types = "ccccccD")



### Import Files -------

csi <- read_excel(here("data", "essaassistance19.xlsx" ), range = "A3:I9973") %>%
    filter(countyname == "Monterey",
           AssistanceStatus2018 != "General Assistance" | AssistanceStatus2019 != "General Assistance" ) 


csi.grad <- csi %>%
    filter(str_detect( AssistanceStatus2019,"Grad"))


csi.low <- csi %>%
    filter(str_detect( AssistanceStatus2019,"Low"))


# grad <- tbl(con, "GRAD_FOUR") %>%
#      filter(CountyCode == "27",
#             AggregateLevel == "S",
#             ReportingCategory == "TA",
#             CharterSchool == "All",
#             DASS == "All") %>%
#     collect() %>%
#     mutate(cds = paste0(CountyCode,DistrictCode,SchoolCode)) %>%
#     filter(cds %in% csi.grad$cds)
    


grad.dash <- tbl(con, "DASH_GRAD") %>%
     filter(countyname == "Monterey",
    #     CountyCode == "27",
    #        AggregateLevel == "S",
            StudentGroup == "ALL",
    #        CharterSchool == "All",
    #        DASS == "All"
) %>%
    collect() %>%
    filter(cds %in% csi.grad$cds,
           reportingyear >= 2019)


grad.dash.summary <- grad.dash %>%
    group_by(schoolname) %>%
    summarise(mean(currstatus))

#  Learning for Life is above,  NMC Indepednet Study depends on rounding rules. 




codes <- read_excel(here("data", "assistancestatus19-rev.xlsx" ), sheet = "Value" , range = "A4:B15") %>%
    mutate(Value = str_sub(Value,1,1))



### Identify schools and priority areas ------




pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")

cd.students <- codebook %>% 
    filter(table == "DASH_SUSP",
           field_name == "studentgroup") %>%
    select(studentgroup = variable, studentgroup_long = definition)



csi.school <- csi.grad$cds[3]

csi.school <- csi.low$cds[6]

all.colors <- tbl(con, "DASH_ALL") %>%
    filter(#county_code == "27",
        #  districtname == "Salinas U High" ,
        CDScode == csi.school,
        
        rtype == "S",
        #         ind == indi,
        # studentgroup == "ALL"
        countyname == "Monterey",
    ) %>% 
    collect() %>%
    mutate(combo.name = paste0(districtname," - ",schoolname),
           bestname = if_else(rtype =="D",districtname,schoolname)) %>%
    #   filter(year == max(year)) %>%
    left_join(cd.students) %>%
    filter(color.factor != 0)

all.colors %>%
    filter(year == max(year)) %>%
    ggplot( aes(ind, y = fct_rev(studentgroup_long))) + 
    geom_tile(aes(fill = color.factor)) +
    lemon::facet_rep_wrap(~bestname, repeat.tick.labels = TRUE) + # If not using lemon, jsut use facet_wrap
    scale_fill_manual(values = pal) +
    theme_minimal()  +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         title = paste0("Dashboard Colors by Student Group in 2019" ),
         subtitle = "The latest available year",
         caption = paste0("https://www.caschooldashboard.org/reports/",csi.school,"/2019")
    )

# Schools with all red indicators;
# • Schools with all red but one indicator of any other color;
# • Schools with all red and orange indicators; and
# • Schools with five or more indicators where the majority are red.







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


