library(readxl)
library(tidyverse)
library(ggplot2)

data <- read_xlsx("data/EmissionTest_Details_Jan-Dec2019.xlsx")

data <- data %>% 
    mutate(Test_Result = ifelse(Test_Result == 1, "PASS", Test_Result)) %>%
    mutate(Test_Result = ifelse(Test_Result == "null" & Engine_Name == "Petrol" & (CO < 4),
                                "PASS", Test_Result)) %>% 
    filter(!is.na(Test_Result)) %>% 
    filter(Test_Result != 'null')

data_grouped <- data %>% 
    group_by(region_name, Test_Result) %>% 
    tally(name = "veh_num")

data_grouped %>% 
    ggplot(aes(reorder(x = region_name, -veh_num), y = veh_num, 
               fill = Test_Result)) +
    geom_bar(stat = "identity", position=position_dodge2(reverse = TRUE)) +
    labs(x = "", 
         y = "Number of vehicles") +
    theme_linedraw() +
    theme(legend.position = "none") +
    coord_flip() +
    ylim(0, 40000)

ggsave("output/emission_test.jpg", width = 20, height = 15, units = "cm")