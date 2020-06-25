library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# 1. EMISSION TEST BY REGION ----------------------------------------------

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
    ggplot(aes(reorder(x = region_name, -veh_num), y = veh_num, fill = Test_Result)) +
    geom_bar(stat = "identity", width = 0.3) +
    # geom_bar(stat = "identity", position=position_dodge2(reverse = TRUE)) +
    labs(x = "", 
         y = "Number of vehicular emission test in 2019",
         fill = "Test result") +
    theme_linedraw() +
    theme(legend.position = c(0.9, 0.9)) +
    coord_flip() +
    ylim(0, 40000)

ggsave("output/emission_test.jpg", width = 20, height = 15, units = "cm")

# 2. VEHICLE POPULATION ---------------------------------------------------
veh_pop <- read_csv("data/MV_year.csv")

veh_pop <- veh_pop %>% 
    mutate(Year = paste("31-12", Year, sep = "-")) %>% 
    mutate(Year = as.POSIXct(Year, format = "%d-%m-%Y"))

veh_pop %>% 
    ggplot(aes(x = Year, y = MV)) +
    geom_line(color = "#D6EAF8", size = 1.5) +
    geom_point(color = "#2874A6", size = 3) +
    ylim(5000, 120000) +
    labs(x = "year",
         y = "Number of vehicles") +
    theme_linedraw() +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 15))
    
ggsave("output/vehicle_pop.jpg", width = 25, height = 15, units = "cm")

# 3. PETROL DIESEL VEHICLES -----------------------------------------------

path_1 <- '/Users/ugyennorbu/Documents/OFFICE DOCUMENTS/VEHICLE DATA/unsorted data/VehiclesDetails_31March2020_ORG.xls'

path_2 <- '/Users/ugyennorbu/Documents/OFFICE DOCUMENTS/VEHICLE DATA/unsorted data/VehiclesDetails_05May2020_PVT.xlsx'

org_veh <- read_xls(path_1)
org_veh <- org_veh %>% 
    select("Vehicle_Type_Name", "Vehicle_Company_Name", "Engine_Name", "Registration_Date")

pvt_veh <- read_xlsx(path_2)
pvt_veh <- pvt_veh %>% 
    select("Vehicle_Type_Name", "Vehicle_Company_Name", "Engine_Name", "Registration_Date")

all_veh <- rbind(org_veh, pvt_veh)
all_veh <- all_veh %>%
    mutate(Registration_Date = as.Date(Registration_Date, "%d/%m/%Y"))

veh_grouped <- all_veh %>% 
    group_by(Vehicle_Type_Name, Engine_Name) %>% 
    tally() %>%
    na.omit()

veh_grouped %>% 
    ggplot(aes(x = reorder(Vehicle_Type_Name, -n), y = n)) +
    geom_bar(stat = 'identity', aes(fill = Engine_Name)) +
    coord_flip() +
    theme_linedraw() +
    scale_fill_manual(values = c("#2874A6", "#F39C12", "#F4D03F", "#85C1E9")) +
    labs(x = "Vehicle type",
         y = "Engine Type",
         fill = "Engine type") +
    theme(legend.background = element_rect(linetype = 0),
          legend.position = c(0.9, 0.87))
ggsave("output/veh_by_engine_type.jpeg", dpi = 300, width = 25, height = 15, units = 'cm')

# Plot the trend of vehicle type registration with time

veh_all_by_year <- all_veh %>%
    filter(Registration_Date >= as.Date("1977-06-26")) %>% 
    group_by(year(Registration_Date), Engine_Name) %>%
    tally() %>% 
    na.omit()
colnames(veh_all_by_year) <- c("Year", "Engine_name", "n")

pet_diesel_only <- veh_all_by_year %>% 
    filter(Engine_name != "Electric" & Engine_name != "Hybrid")

rest_veh <- veh_all_by_year %>% 
    filter(Engine_name == "Electric" | Engine_name == "Hybrid")


veh_all_by_year %>% 
    ggplot(aes(x = Year, y = n)) +
    geom_area(data = pet_diesel_only, aes(fill = Engine_name, color = Engine_name), alpha = 0.2) +
    geom_line(data = rest_veh, size = 1) +
    theme_linedraw() +
    ylim(0, 10000) +
    labs(x = "Year", 
         y = "Number of vehicles registered",
         fill = "Engine name",
         color = "Engine name") +
    theme(legend.position = "top")
    
ggsave("output/petrol_diesel_veh.jpeg", dpi = 300, width = 25, height = 12, units = 'cm')


# 4. TESTING TARGET -------------------------------------------------------

target_data <- read_xlsx("data/veh_emission_test_2016-19.xlsx")

target_data <- target_data %>% 
    mutate(Year = as.Date(Year, "%d-%m-%Y")) %>% 
    pivot_longer(-Year, names_to = "attribute", values_to = "veh_num")

target_data %>% 
    ggplot(aes(x = Year, y = veh_num, color = attribute)) +
    geom_line() +
    ylim(0, 80000) +
    theme_linedraw()+
    labs(x = "Year",
         y = "Number of vehicles",
         color = "") +
    theme(legend.position = "top")

ggsave("output/test_target.jpg", width = 25, height = 15, units = "cm")
