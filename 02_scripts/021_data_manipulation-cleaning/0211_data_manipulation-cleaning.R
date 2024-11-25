require(data.table)
require(tidyverse)
require(magrittr)
require(here)
require(R.utils)

# 1 Extracting, Trasnforming and Saving Data

## 1.1 Load data
acidentes_US <- fread("C:/Users/joaov/Downloads/US_Accidents_March23.zip")
# acidentes_US %>% glimpse() # View data info
# acidentes_US %>% class() # View data info

# 1.2.1 Remove lines with NA values
# acidentes_US <- acidentes_US[apply(!is.na(acidentes_US), 1, any)]

# 1.2.2 Filtering raw dataset
acidentes_US_date <- acidentes_US[, .(ID, Severity, Start_Time, End_Time, Civil_Twilight)]
# acidentes_US_date %>% class() # View data info

# 1.2.3 Extracting date info separetely from datetime column
acidentes_US_date[, Date := as.Date(Start_Time, format = "%Y-%m-%d")] # Extract only date from datetime
acidentes_US_date[, Hour := as.numeric(format(Start_Time, format = "%H"))]  # Extract only hour from datetime
acidentes_US_date[, Year := year(Date)]  # Extract year from date
acidentes_US_date[, Month := month(Date)]  # Extract month from date
acidentes_US_date[, Day := day(Date)]  # Extract day from date
acidentes_US_date[, Weekday := weekdays(Date, abbreviate = TRUE)]  # Extract weekday from date
weekday_order <- c("seg", "ter", "qua", "qui", "sex", "sáb", "dom") # Creates a vector with orderd weekdays
acidentes_US_date[, Weekday := factor(acidentes_US_date$Weekday, levels = weekday_order)] # transforma em fator e ordena os dias da semana
acidentes_US_date[, Time_Range := fifelse(Hour == 0, "00:00:00",
                                          fifelse(Hour == 1, "01:00:00",
                                                  fifelse(Hour == 2, "02:00:00",
                                                          fifelse(Hour == 3, "03:00:00",
                                                                  fifelse(Hour == 4, "04:00:00",
                                                                          fifelse(Hour == 5, "05:00:00",
                                                                                  fifelse(Hour == 6, "06:00:00",
                                                                                          fifelse(Hour == 7, "07:00:00",
                                                                                                  fifelse(Hour == 8, "08:00:00",
                                                                                                          fifelse(Hour == 9, "09:00:00",
                                                                                                                  fifelse(Hour == 10, "10:00:00",
                                                                                                                          fifelse(Hour == 11, "11:00:00",
                                                                                                                                  fifelse(Hour == 12, "12:00:00",
                                                                                                                                          fifelse(Hour == 13, "13:00:00",
                                                                                                                                                  fifelse(Hour == 14, "14:00:00",
                                                                                                                                                          fifelse(Hour == 15, "15:00:00",
                                                                                                                                                                  fifelse(Hour == 16, "16:00:00",
                                                                                                                                                                          fifelse(Hour == 17, "17:00:00",
                                                                                                                                                                                  fifelse(Hour == 18, "18:00:00", 
                                                                                                                                                                                          fifelse(Hour == 19, "19:00:00",
                                                                                                                                                                                                  fifelse(Hour == 20, "20:00:00",
                                                                                                                                                                                                          fifelse(Hour == 21, "21:00:00",
                                                                                                                                                                                                                  fifelse(Hour == 22, "22:00:00",
                                                                                                                                                                                                                          fifelse(Hour == 23, "23:00:00", NA))))))))))))))))))))))))] # Transform time/hour in categorical variable
time_range_order <- c("23:00:00", "22:00:00", "21:00:00", "20:00:00", "19:00:00", "18:00:00", "17:00:00", "16:00:00", "15:00:00", "14:00:00", "13:00:00", "12:00:00", "11:00:00", "10:00:00", "09:00:00", "08:00:00", "07:00:00", "06:00:00", "05:00:00", "04:00:00", "03:00:00", "02:00:00", "01:00:00", "00:00:00")
acidentes_US_date[, Time_Range := factor(acidentes_US_date$Time_Range, levels = time_range_order)] # transforma em fator e ordena as faixas horarias
acidentes_US_date[, Duration := round(as.numeric(difftime(End_Time, Start_Time, units = "mins")), 0)]
# acidentes_US_date[, Start_Time := NULL] # Removing Start_Time column
# acidentes_US_date[, Hour := NULL] # Removing Hour column
# acidentes_US_date[, End_Time := NULL] # Removing Hour column
# acidentes_US_date %>% View() # Visualizing data


# 1.3 Saving data
fwrite(acidentes_US_date, here("C:/Users/joaov/Downloads/CE302-Data-Analysis-And-Communication/01_data/012_clean", "0120_clean-data.csv"))