# Load required packages
library(readr)
library(dplyr)

data <- read_csv(choose.files(), col_types = ("ccccccnnnnc")) %>% #read in the extracted data, assigning correct data types
  #convert the bilateral PCHI column to a numeric type
  mutate(`Bilateral PCHI Inc ANSD` = as.numeric(case_when( 
    `Bilateral PCHI Inc ANSD` == "Yes" ~ 1,
    TRUE ~ 0
  )))

data_manipulated <- data %>%
  filter(IsLatestSession == "Yes") %>% # restrict the data down to just the latest sessions (removes earlier audiology sessions)
  filter(`Current Site Code` != "GOS") %>% # Remove Great Ormond Street data (small number children's hospital)
  group_by(`Current Site`, `Current Site Code`, `Screening Year`, Protocol) %>% # group the data
  # Summarise the data to sums of each column by each group, ignoring NA values. Drops unused columns and grouping.
  summarise(`Screens Completed` = sum(`Screens Completed`, na.rm = T), 
            `Screens Immediate Refer` = sum(`Screens Immediate Refer`, na.rm = T),
            `PCHI Following Immediate Referral` = sum(`PCHI Following Immediate Referral`, na.rm = T),
            `NYD Following Immediate Referral` = sum(`NYD Following Immediate Referral`, na.rm = T),
            `Bilateral PCHI Inc ANSD` = sum(`Bilateral PCHI Inc ANSD`, na.rm = T),
            .groups = "drop")

# save the data to csv
write.csv(data_manipulated, file = "Grouped_Summarised_Data.csv", row.names = F)
