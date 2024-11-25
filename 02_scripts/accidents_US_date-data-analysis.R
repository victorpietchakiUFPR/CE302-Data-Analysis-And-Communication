library(data.table)
library(tidyverse)
library(magrittr)
library(here)
library(R.utils)
library(plotly)
library(scales)

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
# acidentes_US_date %>% glimpse() # Visualizing data info
# acidentes_US_date %>% View() # Visualizing data
# is.na(acidentes_US_date) %>% sum() # Checking NA values
fwrite(acidentes_US_date, here("C:/Users/joaov/Downloads/CE302-Data-Analysis-And-Communication/01_data/012_clean/0120_clean-data.csv"))



# 2 Aggregating Data

# 2.2.0 Creating a table with accidents count per year
acidentes_US_date20 <- acidentes_US_date[, .N, by = .(Year)]
acidentes_US_date20 <- setorder(acidentes_US_date20, Year) # order data set increasingly by year
acidentes_US_date20 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.2.1 Creating a table with accidents count per year and Civil Twilight
acidentes_US_date21 <- acidentes_US_date[, .N, by = .(Year, Civil_Twilight)]
acidentes_US_date21 <- setorder(acidentes_US_date21, Year, Civil_Twilight) # order data set increasingly by year
acidentes_US_date21 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.3.0 Creating a table with accidents count per year and month
acidentes_US_date30 <- acidentes_US_date[, .N, by = .(Year, Month)]
acidentes_US_date30[, Month := sprintf("%02d", Month)]
acidentes_US_date30 <- acidentes_US_date30[, Year_Month := paste0(Year, "-", Month)]
acidentes_US_date30 <- setorder(acidentes_US_date30, Year, Month) # order data set increasingly by year and month
acidentes_US_date30 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.3.1 Creating a table with accidents count per year and month and Civil Twilight
acidentes_US_date31 <- acidentes_US_date[, .N, by = .(Year, Month, Civil_Twilight)]
acidentes_US_date31[, Month := sprintf("%02d", Month)]
acidentes_US_date31 <- acidentes_US_date31[, Year_Month := paste0(Year, "-", Month)]
acidentes_US_date31 <- setorder(acidentes_US_date31, Year, Month, Civil_Twilight) # order data set increasingly by year and month
acidentes_US_date31 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.4.0 Creating a table with accidents count per month
acidentes_US_date40 <- acidentes_US_date[, .N, by = .(Month)]
acidentes_US_date40[, Month := sprintf("%02d", Month)]
acidentes_US_date40 <- setorder(acidentes_US_date40, Month) # order data set increasingly by month
acidentes_US_date40 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.4.1 Creating a table with accidents count per month and Civil Twilight
acidentes_US_date41 <- acidentes_US_date[, .N, by = .(Month, Civil_Twilight)]
acidentes_US_date41[, Month := sprintf("%02d", Month)]
acidentes_US_date41 <- setorder(acidentes_US_date41, Month, Civil_Twilight) # order data set increasingly by month
acidentes_US_date41 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.5.0 Creating a table with accidents count per day
acidentes_US_date50 <- acidentes_US_date[, .N, by = .(Date)]
acidentes_US_date50 <- setorder(acidentes_US_date50, Date) # order data set increasingly by date
acidentes_US_date50 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.5.1 Creating a table with accidents count per day and Civil Twilight
acidentes_US_date51 <- acidentes_US_date[, .N, by = .(Date, Civil_Twilight)]
acidentes_US_date51 <- setorder(acidentes_US_date51, Date, Civil_Twilight) # order data set increasingly by date
acidentes_US_date51 %<>%  as.data.frame() # Transforming data.table into data.frame

# 2.6 Creating a table with accidents count per weekday and time range
acidentes_US_date6 <- acidentes_US_date[, .N, by = .(Weekday, Time_Range)]
acidentes_US_date6 <- setorder(acidentes_US_date6, Weekday, -Time_Range) # order data set increasingly by weekday and time range
acidentes_US_date6 %<>%  as.data.frame() # Transforming data.table into data.frame

# # # Visualizing data
# acidentes_US_date2 %>% View()
# acidentes_US_date3 %>% View()
# acidentes_US_date31 %>% View()
# acidentes_US_date4 %>% View()
# acidentes_US_date5 %>% View()



# 3 Building Plots

# 3.2.0 Ploting accidents count per year with barplot
acidentes_ano_barplot0 <- acidentes_US_date20 %>% 
  ggplot() + 
  aes(x = Year, y = N) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Ano", x = "Ano", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
acidentes_ano_barplot0 %>% ggplotly()

# 3.2.1 Ploting accidents count per year with barplot
acidentes_ano_barplot1 <- acidentes_US_date21 %>% 
  ggplot() + 
  aes(x = Year, y = N) + 
  geom_bar(aes(fill = Civil_Twilight), stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  labs(title = "Qtd. de Acidentes por Ano", x = "Ano", y = "Qtd. Acidentes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
acidentes_ano_barplot1 %>% ggplotly()

# 3.3.0 Ploting accidents count per year and month with barplot
acidentes_ano_mes_barplot0 <- acidentes_US_date30 %>% 
  ggplot() + 
  geom_bar(aes(x = Year_Month, y = N), stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Ano e Mes", x = "Ano-Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))
acidentes_ano_mes_barplot0 %>% ggplotly()

# 3.3.1 Ploting accidents count per year and month with barplot
acidentes_ano_mes_barplot1 <- acidentes_US_date31 %>% 
  ggplot() + 
  geom_bar(aes(x = Year_Month, y = N, fill = Civil_Twilight), stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) + 
  labs(title = "Qtd. de Acidentes por Ano e Mes", x = "Ano-Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))
acidentes_ano_mes_barplot1 %>% ggplotly()

# 3.4.0 Ploting accidents count per month with barplot
acidentes_mes_barplot0 <- acidentes_US_date40 %>% 
  ggplot() + 
  aes(x = Month, y = N) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
acidentes_mes_barplot0 %>% ggplotly()

# 3.4.1 Ploting accidents count per month with barplot
acidentes_mes_barplot1 <- acidentes_US_date41 %>% 
  ggplot() + 
  aes(x = Month, y = N) + 
  geom_bar(stat = "identity", aes(fill = Civil_Twilight)) +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
acidentes_mes_barplot1 %>% ggplotly()

# 3.5.0 Ploting accidents count per date with line graphic
acidentes_data_timeseries0 <- acidentes_US_date50 %>% 
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_point(size = 0.7) + 
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries0 %>% ggplotly()

# 3.5.1 Ploting accidents count per date with line graphic
acidentes_data_timeseries1 <- acidentes_US_date51 %>% 
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_point(aes(color = Civil_Twilight), size = 0.7) + 
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_color_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries1 %>% ggplotly()

# 3.6 Ploting accidents count per weekday and time range
acidentes_data_2table <- acidentes_US_date6 %>% 
  ggplot() + 
  aes(x = Weekday, y = Time_Range, fill = N) +
  geom_tile() + 
  scale_fill_gradient(low = "black", high = "white") +
  labs(title = "Agenda Diaria de Acidentes", x = "Dia da Semana", y = "Horario") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") +
  theme(
    axis.title.x.top = element_text(),
    axis.text.x.top = element_text(),
    axis.line.x.top = element_line(),
    axis.ticks.x.top = element_line()
    
  )
acidentes_data_2table