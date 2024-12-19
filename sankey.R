## Sankey 




# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(dplyr) 

df <- tribble(
    ~x, ~node, ~next_x, ~next_node,
    2017, "Salinas Union",2018,"Salinas Union",
    2018, "Salinas Union",2019,"Salinas Union",
    2019, "Salinas Union",2020,"Salinas Union",
    2017, "South Monterey",2018,"South Monterey",
    2018, "South Monterey",2019,"South Monterey",
    2019, "South Monterey",2020,"South Monterey",
    2019, "Santa Rita",2020,"Santa Rita",
    # 2018, "MCOE",2019,"MCOE",
    # 2019, "MCOE",2020,"MCOE",
    2018, "Soledad",2019,"Soledad",
    2019, "Soledad",2020,"Soledad",
    2017, "Salinas City",2018,"Salinas City",
    2018, "Salinas City",2019,"Salinas City",
    2018, "San Antonio",2019,"San Antonio",
    2018, "Monterey Peninsula",2019,"Monterey Peninsula",
    2019, "Monterey Peninsula",2020,"Monterey Peninsula",   
    2017, "Greenfield",2018,"Greenfield",
    2018, "Greenfield",2019,"Greenfield",
    2017, "King City",2018,"King City",
    2018, "Gonzales",2019,"Gonzales",
    #  2020, "",2020,"",
    
) %>%
    arrange(node) 





df.students <- tribble(
    ~x, ~node, ~next_x, ~next_node, ~count,
    2017, "Salinas Union",2018,"Salinas Union",4,
    2018, "Salinas Union",2019,"Salinas Union",3,
    2019, "Salinas Union",2020,"Salinas Union",3,
    2017, "South Monterey",2018,"South Monterey",1,
    2018, "South Monterey",2019,"South Monterey",4,
    2019, "South Monterey",2020,"South Monterey",1,
    2019, "Santa Rita",2020,"Santa Rita",1,
    # 2018, "MCOE",2019,"MCOE",4,
    # 2019, "MCOE",2020,"MCOE",4,
    2018, "Soledad",2019,"Soledad",2,
    2019, "Soledad",2020,"Soledad",1,
    2017, "Salinas City",2018,"Salinas City",1,
    2018, "Salinas City",2019,"Salinas City",2,
    2018, "San Antonio",2019,"San Antonio",3,
    2018, "Monterey Peninsula",2019,"Monterey Peninsula",1,
    2019, "Monterey Peninsula",2020,"Monterey Peninsula",2,   
    2017, "Greenfield",2018,"Greenfield",1,
    2018, "Greenfield",2019,"Greenfield",1,
    2017, "King City",2018,"King City",1,
    2018, "Gonzales",2019,"Gonzales",3,
    #  2020, "",2020,"",
    
) #%>%
 #   uncount(count) %>%
#    arrange(node) 


df.students.labs <- df.students %>%
    mutate(labby = paste0(node,", ",count))



ggplot(df.students.labs, aes(x = x, 
               next_x = next_x, 
               node = node,
               label = labby, 
               next_node = next_node,
               fill = factor(node))) +
    geom_sankey()+
    theme_sankey(base_size = 16) +
    geom_sankey(flow.alpha = 0.75, node.color = 1) +
    geom_sankey_label(size = 3.5, color = 1, fill = "white", hjust="inward") +
    scale_fill_viridis_d(option = "A", alpha = 0.95) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(2017,2018,2019))+
    labs(title = "Districts in DA Overtime" ,
         subtitle = "Labels include number of student groups"
         ,
         x = "") 


ggsave("DA Sankey.png", width = 7, height = 5)



ggsave("DA Sankey groups.png", width = 7, height = 5)
