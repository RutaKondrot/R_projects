library(rvest)
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(ggplot2)

url <- read_html("https://en.wikipedia.org/wiki/FC_Bayern_Munich") #Wikipedia's article about FC Bayern Munich

all_tables <- url %>% html_table(fill = TRUE) #all tables from the article

required_table <- all_tables[[20]] #"Coaches since 1963" table 
required_table <- required_table[-1,]

required_table <- required_table %>% clean_names()

# Changing empty values "–" to "0"
for(i in 1:length(required_table)) {
  for(j in 1:length(required_table[[i]])) {
    required_table[[i]][j] <- required_table[[i]][j] %>%
      stringr::str_replace("–", "0") 
  }
}

# Changing date format to "Y-m-d"
required_table$period <- parse_date_time(x = required_table$period, 
                                      orders = "d B Y") %>% str_remove("UTC") 

required_table$period_2 <- parse_date_time(x = required_table$period_2, 
                                      orders = "d B Y") %>% str_remove("UTC") 

# Finding total sum of domestic titles (summing up BL, DP, LP and	SC titles)
domestic_titles <- data.frame(as.numeric(required_table$domestic), as.numeric(required_table$domestic_2), 
                              as.numeric(required_table$domestic_3), as.numeric(required_table$domestic_4))

total_domestic_titles <- data.frame(rowSums(domestic_titles, na.rm = TRUE))

# Finding total sum of european titles (summing up CL,	EL,	SC and	WC titles)
european_titles <- data.frame(as.numeric(required_table$european), as.numeric(required_table$european_2), 
                              as.numeric(required_table$european_3), as.numeric(required_table$european_4))

total_european_titles <- data.frame(rowSums(european_titles, na.rm = TRUE))

# Finding total sum of worldwide titles (summing up ICC	and CWC titles)
worldwide_titles <- data.frame(as.numeric(required_table$worldwide), as.numeric(required_table$worldwide_2))

total_worldwide_titles <- data.frame(rowSums(worldwide_titles, na.rm = TRUE))

# Merging "from" and "until" into one date column "period"
period_new <- paste("From", required_table$period, "until", required_table$period_2)

coaches <- data.frame(required_table$coach, period_new, required_table$major_titles, total_domestic_titles, 
                      total_european_titles, total_worldwide_titles) 

# Changing names of the columns
colnames(coaches) <- c("Coach", "Period", "Total number of titles", "Domestic titles", "European titles", "Worldwide titles")

# Ordering the table by "Total number of titles" from the largest to the smallest value
coaches <- coaches[order(as.integer(coaches$`Total number of titles`),decreasing = TRUE), ]

coaches$Period[3] <- "From 2019-11-03 until present" #changing "NA" to "present"

coaches$Coach[2] <- "Pep Guardiola" #removing additional "[172][173]"

coaches$Coach <- coaches$Coach %>% str_remove_all("[:punct:]caretaker[:punct:]") %>% trimws("r") #removing "(caretaker) "

### 1. Table of coaches of FC Bayern Munich since 1963 (with some duplicate values in "Coach" column))
View(coaches) 

# Saving created data frame as a .csv file
write.csv(coaches, "C:\\Users\\el ruchenzo\\OneDrive\\Documents\\coaches.csv", row.names = FALSE)


# Creating a data frame that will be transformed into a new data frame without duplicates and dates ("Period" column)
coaches1 <- data.frame(coaches$Coach, as.numeric(unlist(coaches$`Total number of titles`)), as.numeric(unlist(coaches$`Domestic titles`)), 
                       as.numeric(unlist(coaches$`European titles`)), as.numeric(unlist(coaches$`Worldwide titles`)))

# Changing names of the columns
colnames(coaches1) <- c("Coach", "Total number of titles", "Domestic titles", "European titles", "Worldwide titles")

# Finding duplicates in column "Coach", leaving only 1 value of "Coach" and summarising values of other columns
coaches1 %>% 
  group_by(Coach) %>%
  filter(n()>1) %>%
  summarise_all(sum) -> coaches_dub

# Creating a data frame with only unique values in "Coach" column (removing all duplicates)
coaches_no_dub <- filter(coaches1, Coach != "Franz Beckenbauer" & Coach != "Giovanni Trapattoni" & Coach != "Jupp Heynckes" 
                & Coach != "Ottmar Hitzfeld" & Coach != "Udo Lattek")

# Combining "coaches_dub" and "coaches_no_dub" into one
coaches_clean <- data.frame(rbind(coaches_dub, coaches_no_dub))

# Changing names of the columns
colnames(coaches_clean) <- c("Coach", "Total number of titles", "Domestic titles", "European titles", "Worldwide titles")

### 2. Table of coaches of FC Bayern Munich since 1963 (without duplicate values in "Coach" column and without "Period" column))
View(coaches_clean)

# Saving created data frame as a .csv file
write.csv(coaches_clean, "C:\\Users\\el ruchenzo\\OneDrive\\Documents\\coaches_clean.csv", row.names = FALSE)


# Transforming a data frame into the form that will be suitable for drawing a stacked bar chart (adding a new "Type of title" column)
coaches_plot <- data.frame(rbind(cbind(coaches_clean$Coach, coaches_clean$`Domestic titles`, rep("Domestic titles", 23)),
                                 cbind(coaches_clean$Coach, coaches_clean$`European titles`, rep("European titles", 23)),
                                 cbind(coaches_clean$Coach, coaches_clean$`Worldwide titles`, rep("Worldwide titles", 23))))

# Changing names of the columns
colnames(coaches_plot) <- c("Coach", "Number of titles", "Type of title")

# Changing zero values to NA
coaches_plot[coaches_plot == 0] <- NA

# Removing NA values from the data frame
coaches_plot <- na.omit(coaches_plot)

# Changing types of variables 
Coach <- as.factor(coaches_plot[,1])
Number_of_titles <- as.numeric(coaches_plot[,2])
Type_of_title <- as.factor(coaches_plot[,3])

coaches_plot_clean <- data.frame(Coach, Number_of_titles, Type_of_title)

### 3. Table of coaches of FC Bayern Munich since 1963 that is suitable for drawing stacked bar chart
View(coaches_plot_clean)

# Saving created data frame as a .csv file
write.csv(coaches_plot_clean, "C:\\Users\\el ruchenzo\\OneDrive\\Documents\\coaches_plot_clean.csv", row.names = FALSE)

### 4. The stacked bar chart of transformed table of coaches of FC Bayern Munich since 1963
chart <- ggplot(coaches_plot_clean, aes(x = factor(Coach), y = Number_of_titles, fill = Type_of_title, label = Number_of_titles)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) + 
  geom_text(aes(label = Number_of_titles), position = position_stack(vjust = 0.5),size = 4) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Sum of titles", limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  coord_flip() +
  scale_fill_brewer(palette="Reds", direction = -1) + 
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) + 
  ggtitle("Coaches of FC Bayern Munich since 1963") + 
  labs(caption = "Data Source: Wikipedia.\n\nNotes: A stacked bar chart showing the domestic, europeand and worldwide titles won by FC Bayern \nMunich head coaches from 1963 to the present. \nDomestic titles (BL - Bundesliga, DP - DFB-Pokal, LP - DFB-Ligapokal, SC - Super Cup), European ti-\ntles (CL - Champions League/European Cup, EL - Europa League/UEFA Cup, SC - UEFA Super Cup, \nWC - UEFA Cup Winners' Cup), Worldwide titles (ICC - Intercontinental Cup, CWC - FIFA Club World \nCup).")

# Saving created chart as a .png file
ggsave_custom(chart, 
              "C:\\Users\\el ruchenzo\\OneDrive\\Documents\\chart.png",
              dpi = "auto",
              width = 7, height = 14, unit = "cm")
