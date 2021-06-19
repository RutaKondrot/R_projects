library(tidyverse)
library(pdftools)

# Uploading and processing a pdf file
pdfpath <- "~\\June.pdf"
PDFInfo <- pdftools::pdf_info(pdfpath)
pdf <- pdftools::pdf_text(pdfpath) %>% str_split("\r\n") 

# Creating a date variable whose value must be manually adjusted each time the code is run
date <- data.frame(cbind("Date", "2021-06-30"))

# Header removal from all pages in the pdf file
for(i in 1:2) { 
  pdf[[i]] <- pdf[[i]][-c(1:2)]
}

# Footer removal from all pages in the pdf file
for(i in 2) {
  a1 <- length(pdf[[1]]) #length of pdf i page
  a2 <- length(pdf[[2]])
  b1 <- a1-3 #length of pdf i page without a footer
  b2 <- a2-3
  pdf[[1]] <- pdf[[1]][-b1:-a1] #footer removal
  pdf[[2]] <- pdf[[2]][-b2:-a2]
} 

# Collecting text to 1 string (removal of whitespaces) and breaking obtained text into separate lines
pdf_squish <- str_squish(pdf) 
pdf_split <- strsplit(pdf_squish, split= "\\,\\s\\\"") 

# Cleaning the received data: removing unnecessary whitespaces and symbols
for(i in 1:length(pdf_split)) {
  pdf_split[[i]][1] <- pdf_split[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*") #removing c(\" from the first line of each pdf page
}

for(i in 1:length(pdf_split)) {
  for(j in 1:length(pdf_split[[i]])) {
    pdf_split[[i]][j] <- pdf_split[[i]][j] %>%
      stringr::str_extract(".*(?=\")") #removing \") from the end of each line
  }
}

# Removal of special characters (unicodes)
pdf_clean1 <- gsub("^\\s*|\\s*<U\\+\\w+>", "", pdf_split[[1]])
pdf_clean2 <- gsub("^\\s*|\\s*<U\\+\\w+>", "", pdf_split[[2]])


# Separation of the obtained data into text and number lists.
# Removal of unnecessary rows from these lists and changing variables names. 
# Combination of text and number lists into a single data frame.
# Finally, the structure of the received table is changed manually (row by row).

##########
# 1 PAGE #
##########

### 1 TABLE: Prices of quality coffee beans according to roasting type

text1a = list()
titles1 <- pdf_clean1 %>% str_replace_all("No data", "") %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% str_remove_all("[:digit:]+.*")
titles1a <- titles1[1:21]
titles1a_df <- data.frame(titles1a) 
names1a_ex <- titles1a_df
text1a <- dplyr::bind_rows(names1a_ex) %>% rename("Title" = "titles1a") 
print(text1a)

digits1a = list()
numbers1 <- pdf_clean1 %>% str_extract("[:digit:]+.*") %>% str_replace_all("No|data", "") %>% str_replace_all("\\.", "")
numbers1a <- numbers1[1:21]
numbers1a_df <- data.frame(numbers1a)
numbers1a_ex <- numbers1a_df
digits1a <- dplyr::bind_rows(numbers1a_ex) %>% rename("Price of quality coffee beans (EUR)" = "numbers1a") 
print(digits1a)

digits1a %>%
  separate("Price of quality coffee beans (EUR)", c("10 kg from","10 kg to", "20 kg from","20 kg to", 
                                                    "30 kg from", "30 kg to", "40 kg from", "40 kg to"), sep="\\s") -> digits1a

DF1a <- cbind(text1a, digits1a) 
#View(DF1a)

DF1a <- DF1a[-c(1, 2, 3, 4, 10, 16),] 

row1 <- rbind(cbind("10 kg of Coffee Bros light roast coffee beans from", DF1a$`10 kg from`[1]),
              cbind("10 kg of Coffee Bros light roast coffee beans to", DF1a$`10 kg to`[1]),
              cbind("20 kg of Coffee Bros light roast coffee beans from", DF1a$`20 kg from`[1]),
              cbind("20 kg of Coffee Bros light roast coffee beans to", DF1a$`20 kg to`[1]),
              cbind("30 kg of Coffee Bros light roast coffee beans from", DF1a$`30 kg from`[1]),
              cbind("30 kg of Coffee Bros light roast coffee beans to", DF1a$`30 kg to`[1]),
              cbind("40 kg of Coffee Bros light roast coffee beans from", DF1a$`40 kg from`[1]),
              cbind("40 kg of Coffee Bros light roast coffee beans to", DF1a$`40 kg to`[1]))

row2 <- rbind(cbind("10 kg of Lifeboost light roast coffee beans from", DF1a$`10 kg from`[2]),
              cbind("10 kg of Lifeboost light roast coffee beans to", DF1a$`10 kg to`[2]),
              cbind("20 kg of Lifeboost light roast coffee beans from", DF1a$`20 kg from`[2]),
              cbind("20 kg of Lifeboost light roast coffee beans to", DF1a$`20 kg to`[2]),
              cbind("30 kg of Lifeboost light roast coffee beans from", DF1a$`30 kg from`[2]),
              cbind("30 kg of Lifeboost light roast coffee beans to", DF1a$`30 kg to`[2]),
              cbind("40 kg of Lifeboost light roast coffee beans from", DF1a$`40 kg from`[2]),
              cbind("40 kg of Lifeboost light roast coffee beans to", DF1a$`40 kg to`[2]))

row3 <- rbind(cbind("10 kg of Dune Zip Zinger light roast coffee beans from", DF1a$`10 kg from`[3]),
              cbind("10 kg of Dune Zip Zinger light roast coffee beans to", DF1a$`10 kg to`[3]),
              cbind("20 kg of Dune Zip Zinger light roast coffee beans from", DF1a$`20 kg from`[3]),
              cbind("20 kg of Dune Zip Zinger light roast coffee beans to", DF1a$`20 kg to`[3]),
              cbind("30 kg of Dune Zip Zinger light roast coffee beans from", DF1a$`30 kg from`[3]),
              cbind("30 kg of Dune Zip Zinger light roast coffee beans to", DF1a$`30 kg to`[3]),
              cbind("40 kg of Dune Zip Zinger light roast coffee beans from", DF1a$`40 kg from`[3]),
              cbind("40 kg of Dune Zip Zinger light roast coffee beans to", DF1a$`40 kg to`[3]))


row4 <- rbind(cbind("10 kg of Tandem Sun Lamp light roast coffee beans from", DF1a$`10 kg from`[4]),
              cbind("10 kg of Tandem Sun Lamp light roast coffee beans to", DF1a$`10 kg to`[4]),
              cbind("20 kg of Tandem Sun Lamp light roast coffee beans from", DF1a$`20 kg from`[4]),
              cbind("20 kg of Tandem Sun Lamp light roast coffee beans to", DF1a$`20 kg to`[4]),
              cbind("30 kg of Tandem Sun Lamp light roast coffee beans from", DF1a$`30 kg from`[4]),
              cbind("30 kg of Tandem Sun Lamp light roast coffee beans to", DF1a$`30 kg to`[4]),
              cbind("40 kg of Tandem Sun Lamp light roast coffee beans from", DF1a$`40 kg from`[4]),
              cbind("40 kg of Tandem Sun Lamp light roast coffee beans to", DF1a$`40 kg to`[4]))

row5 <- rbind(cbind("10 kg of Verve Kenya Karimikui light roast coffee beans from", DF1a$`10 kg from`[5]),
              cbind("10 kg of Verve Kenya Karimikui light roast coffee beans to", DF1a$`10 kg to`[5]),
              cbind("20 kg of Verve Kenya Karimikui light roast coffee beans from", DF1a$`20 kg from`[5]),
              cbind("20 kg of Verve Kenya Karimikui light roast coffee beans to", DF1a$`20 kg to`[5]),
              cbind("30 kg of Verve Kenya Karimikui light roast coffee beans from", DF1a$`30 kg from`[5]),
              cbind("30 kg of Verve Kenya Karimikui light roast coffee beans to", DF1a$`30 kg to`[5]),
              cbind("40 kg of Verve Kenya Karimikui light roast coffee beans from", DF1a$`40 kg from`[5]),
              cbind("40 kg of Verve Kenya Karimikui light roast coffee beans to", DF1a$`40 kg to`[5]))

light_roast1 <- rbind(row1, row2, row3, row4, row5)

row6 <- rbind(cbind("10 kg of Koa medium roast coffee beans from", DF1a$`10 kg from`[6]),
              cbind("10 kg of Koa medium roast coffee beans to", DF1a$`10 kg to`[6]),
              cbind("20 kg of Koa medium roast coffee beans from", DF1a$`20 kg from`[6]),
              cbind("20 kg of Koa medium roast coffee beans to", DF1a$`20 kg to`[6]),
              cbind("30 kg of Koa medium roast coffee beans from", DF1a$`30 kg from`[6]),
              cbind("30 kg of Koa medium roast coffee beans to", DF1a$`30 kg to`[6]),
              cbind("40 kg of Koa medium roast coffee beans from", DF1a$`40 kg from`[6]),
              cbind("40 kg of Koa medium roast coffee beans to", DF1a$`40 kg to`[6]))

row7 <- rbind(cbind("10 kg of Volcanica medium roast coffee beans from", DF1a$`10 kg from`[7]),
              cbind("10 kg of Volcanica medium roast coffee beans to", DF1a$`10 kg to`[7]),
              cbind("20 kg of Volcanica medium roast coffee beans from", DF1a$`20 kg from`[7]),
              cbind("20 kg of Volcanica medium roast coffee beans to", DF1a$`20 kg to`[7]),
              cbind("30 kg of Volcanica medium roast coffee beans from", DF1a$`30 kg from`[7]),
              cbind("30 kg of Volcanica medium roast coffee beans to", DF1a$`30 kg to`[7]),
              cbind("40 kg of Volcanica medium roast coffee beans from", DF1a$`40 kg from`[7]),
              cbind("40 kg of Volcanica medium roast coffee beans to", DF1a$`40 kg to`[7]))

row8 <- rbind(cbind("10 kg of Kicking Horse medium roast coffee beans from", DF1a$`10 kg from`[8]),
              cbind("10 kg of Kicking Horse medium roast coffee beans to", DF1a$`10 kg to`[8]),
              cbind("20 kg of Kicking Horse medium roast coffee beans from", DF1a$`20 kg from`[8]),
              cbind("20 kg of Kicking Horse medium roast coffee beans to", DF1a$`20 kg to`[8]),
              cbind("30 kg of Kicking Horse medium roast coffee beans from", DF1a$`30 kg from`[8]),
              cbind("30 kg of Kicking Horse medium roast coffee beans to", DF1a$`30 kg to`[8]),
              cbind("40 kg of Kicking Horse medium roast coffee beans from", DF1a$`40 kg from`[8]),
              cbind("40 kg of Kicking Horse medium roast coffee beans to", DF1a$`40 kg to`[8]))

row9 <- rbind(cbind("10 kg of Caribou medium roast coffee beans from", DF1a$`10 kg from`[9]),
              cbind("10 kg of Caribou medium roast coffee beans to", DF1a$`10 kg to`[9]),
              cbind("20 kg of Caribou medium roast coffee beans from", DF1a$`20 kg from`[9]),
              cbind("20 kg of Caribou medium roast coffee beans to", DF1a$`20 kg to`[9]),
              cbind("30 kg of Caribou medium roast coffee beans from", DF1a$`30 kg from`[9]),
              cbind("30 kg of Caribou medium roast coffee beans to", DF1a$`30 kg to`[9]),
              cbind("40 kg of Caribou medium roast coffee beans from", DF1a$`40 kg from`[9]),
              cbind("40 kg of Caribou medium roast coffee beans to", DF1a$`40 kg to`[9]))

row10 <- rbind(cbind("10 kg of Peet's Coffee Big Bang medium roast coffee beans from", DF1a$`10 kg from`[10]),
               cbind("10 kg of Peet's Coffee Big Bang medium roast coffee beans to", DF1a$`10 kg to`[10]),
               cbind("20 kg of Peet's Coffee Big Bang medium roast coffee beans from", DF1a$`20 kg from`[10]),
               cbind("20 kg of Peet's Coffee Big Bang medium roast coffee beans to", DF1a$`20 kg to`[10]),
               cbind("30 kg of Peet's Coffee Big Bang medium roast coffee beans from", DF1a$`30 kg from`[10]),
               cbind("30 kg of Peet's Coffee Big Bang medium roast coffee beans to", DF1a$`30 kg to`[10]),
               cbind("40 kg of Peet's Coffee Big Bang medium roast coffee beans from", DF1a$`40 kg from`[10]),
               cbind("40 kg of Peet's Coffee Big Bang medium roast coffee beans to", DF1a$`40 kg to`[10]))

medium_roast1 <- rbind(row6, row7, row8, row9, row10)

row11 <- rbind(cbind("10 kg of Sumatra Mandheling dark roast coffee beans from", DF1a$`10 kg from`[11]),
               cbind("10 kg of Sumatra Mandheling dark roast coffee beans to", DF1a$`10 kg to`[11]),
               cbind("20 kg of Sumatra Mandheling dark roast coffee beans from", DF1a$`20 kg from`[11]),
               cbind("20 kg of Sumatra Mandheling dark roast coffee beans to", DF1a$`20 kg to`[11]),
               cbind("30 kg of Sumatra Mandheling dark roast coffee beans from", DF1a$`30 kg from`[11]),
               cbind("30 kg of Sumatra Mandheling dark roast coffee beans to", DF1a$`30 kg to`[11]),
               cbind("40 kg of Sumatra Mandheling dark roast coffee beans from", DF1a$`40 kg from`[11]),
               cbind("40 kg of Sumatra Mandheling dark roast coffee beans to", DF1a$`40 kg to`[11]))

row12 <- rbind(cbind("10 kg of Lion Coffee dark roast coffee beans from", DF1a$`10 kg from`[12]),
               cbind("10 kg of Lion Coffee dark roast coffee beans to", DF1a$`10 kg to`[12]),
               cbind("20 kg of Lion Coffee dark roast coffee beans from", DF1a$`20 kg from`[12]),
               cbind("20 kg of Lion Coffee dark roast coffee beans to", DF1a$`20 kg to`[12]),
               cbind("30 kg of Lion Coffee dark roast coffee beans from", DF1a$`30 kg from`[12]),
               cbind("30 kg of Lion Coffee dark roast coffee beans to", DF1a$`30 kg to`[12]),
               cbind("40 kg of Lion Coffee dark roast coffee beans from", DF1a$`40 kg from`[12]),
               cbind("40 kg of Lion Coffee dark roast coffee beans to", DF1a$`40 kg to`[12]))

row13 <- rbind(cbind("10 kg of Death Wish dark roast coffee beans from", DF1a$`10 kg from`[13]),
               cbind("10 kg of Death Wish dark roast coffee beans to", DF1a$`10 kg to`[13]),
               cbind("20 kg of Death Wish dark roast coffee beans from", DF1a$`20 kg from`[13]),
               cbind("20 kg of Death Wish dark roast coffee beans to", DF1a$`20 kg to`[13]),
               cbind("30 kg of Death Wish dark roast coffee beans from", DF1a$`30 kg from`[13]),
               cbind("30 kg of Death Wish dark roast coffee beans to", DF1a$`30 kg to`[13]),
               cbind("40 kg of Death Wish dark roast coffee beans from", DF1a$`40 kg from`[13]),
               cbind("40 kg of Death Wish dark roast coffee beans to", DF1a$`40 kg to`[13]))

row14 <- rbind(cbind("10 kg of Don Pablo dark roast coffee beans from", DF1a$`10 kg from`[14]),
               cbind("10 kg of Don Pablo dark roast coffee beans to", DF1a$`10 kg to`[14]),
               cbind("20 kg of Don Pablo dark roast coffee beans from", DF1a$`20 kg from`[14]),
               cbind("20 kg of Don Pablo dark roast coffee beans to", DF1a$`20 kg to`[14]),
               cbind("30 kg of Don Pablo dark roast coffee beans from", DF1a$`30 kg from`[14]),
               cbind("30 kg of Don Pablo dark roast coffee beans to", DF1a$`30 kg to`[14]),
               cbind("40 kg of Don Pablo dark roast coffee beans from", DF1a$`40 kg from`[14]),
               cbind("40 kg of Don Pablo dark roast coffee beans to", DF1a$`40 kg to`[14]))


row15 <- rbind(cbind("10 kg of Amazon Fresh dark roast coffee beans from", DF1a$`10 kg from`[15]),
               cbind("10 kg of Amazon Fresh dark roast coffee beans to", DF1a$`10 kg to`[15]),
               cbind("20 kg of Amazon Fresh dark roast coffee beans from", DF1a$`20 kg from`[15]),
               cbind("20 kg of Amazon Fresh dark roast coffee beans to", DF1a$`20 kg to`[15]),
               cbind("30 kg of Amazon Fresh dark roast coffee beans from", DF1a$`30 kg from`[15]),
               cbind("30 kg of Amazon Fresh dark roast coffee beans to", DF1a$`30 kg to`[15]),
               cbind("40 kg of Amazon Fresh dark roast coffee beans from", DF1a$`40 kg from`[15]),
               cbind("40 kg of Amazon Fresh dark roast coffee beans to", DF1a$`40 kg to`[15]))

dark_roast1 <- rbind(row11, row12, row13, row14, row15)

types_of_roasting <- data.frame(rbind(light_roast1, medium_roast1, dark_roast1))
#View(types_of_roasting)

# Assigning time series properties to available data (adding a "Date" variable to constructed tables).
# Creation of primary csv files (codes of them are left in the comments nearby '#').

types_of_roasting_ts <- data.frame(rbind(date, types_of_roasting))
types_of_roasting_t_ts <- setNames(data.frame(t(types_of_roasting_ts[,-1])), types_of_roasting_ts[,1])
#write.csv(types_of_roasting_t_ts, "~\\Prices_of_quality_coffee_beans_according_to_types_of_roasting.csv", row.names = FALSE)

# Adding new time series data to previously stored data in csv files.

datafile1a <- read.csv("~\\Prices_of_quality_coffee_beans_according_to_types_of_roasting.csv")
#View(datafile1a)
colnames(datafile1a) <- colnames(types_of_roasting_t_ts)
data_to_file1a <- rbind(datafile1a, types_of_roasting_t_ts)
write.csv(data_to_file1a, "~\\Prices_of_quality_coffee_beans_according_to_types_of_roasting.csv", row.names = FALSE)


### 2 TABLE: Prices of quality coffee beans according to beans species

text1b = list()
titles1b <- titles1[22:36]
titles1b_df <- data.frame(titles1b) 
names1b_ex <- titles1b_df
text1b <- dplyr::bind_rows(names1b_ex) %>% rename("Title" = "titles1b") 
print(text1b)

digits1a = list()
numbers1 <- pdf_clean1 %>% str_extract("[:digit:]+.*") %>% str_replace_all("No|data", "") %>% str_replace_all("\\.", "")
numbers1b <- numbers1[22:36]
numbers1b_df <- data.frame(numbers1b)
numbers1b_ex <- numbers1b_df
digits1b <- dplyr::bind_rows(numbers1b_ex) %>% rename("Price of quality coffee beans (EUR)" = "numbers1b") 
print(digits1b)

digits1b %>%
  separate("Price of quality coffee beans (EUR)", c("10 kg from","10 kg to", "20 kg from","20 kg to", 
                                                    "30 kg from", "30 kg to", "40 kg from", "40 kg to"), sep="\\s") -> digits1b

DF1b <- cbind(text1b, digits1b) 
#View(DF1b)

DF1b <- DF1b[-c(1, 2, 3, 4, 10),] 

row16 <- rbind(cbind("10 kg of Coffee Bros arabica coffee beans from", DF1b$`10 kg from`[1]),
               cbind("10 kg of Coffee Bros arabica coffee beans to", DF1b$`10 kg to`[1]),
               cbind("20 kg of Coffee Bros arabica coffee beans from", DF1b$`20 kg from`[1]),
               cbind("20 kg of Coffee Bros arabica coffee beans to", DF1b$`20 kg to`[1]),
               cbind("30 kg of Coffee Bros arabica coffee beans from", DF1b$`30 kg from`[1]),
               cbind("30 kg of Coffee Bros arabica coffee beans to", DF1b$`30 kg to`[1]),
               cbind("40 kg of Coffee Bros arabica coffee beans from", DF1b$`40 kg from`[1]),
               cbind("40 kg of Coffee Bros arabica coffee beans to", DF1b$`40 kg to`[1]))

row17 <- rbind(cbind("10 kg of Lifeboost arabica coffee beans from", DF1b$`10 kg from`[2]),
               cbind("10 kg of Lifeboost arabica coffee beans to", DF1b$`10 kg to`[2]),
               cbind("20 kg of Lifeboost arabica coffee beans from", DF1b$`20 kg from`[2]),
               cbind("20 kg of Lifeboost arabica coffee beans to", DF1b$`20 kg to`[2]),
               cbind("30 kg of Lifeboost arabica coffee beans from", DF1b$`30 kg from`[2]),
               cbind("30 kg of Lifeboost arabica coffee beans to", DF1b$`30 kg to`[2]),
               cbind("40 kg of Lifeboost arabica coffee beans from", DF1b$`40 kg from`[2]),
               cbind("40 kg of Lifeboost arabica coffee beans to", DF1b$`40 kg to`[2]))

row18 <- rbind(cbind("10 kg of Dune Zip Zinger arabica coffee beans from", DF1b$`10 kg from`[3]),
               cbind("10 kg of Dune Zip Zinger arabica coffee beans to", DF1b$`10 kg to`[3]),
               cbind("20 kg of Dune Zip Zinger arabica coffee beans from", DF1b$`20 kg from`[3]),
               cbind("20 kg of Dune Zip Zinger arabica coffee beans to", DF1b$`20 kg to`[3]),
               cbind("30 kg of Dune Zip Zinger arabica coffee beans from", DF1b$`30 kg from`[3]),
               cbind("30 kg of Dune Zip Zinger arabica coffee beans to", DF1b$`30 kg to`[3]),
               cbind("40 kg of Dune Zip Zinger arabica coffee beans from", DF1b$`40 kg from`[3]),
               cbind("40 kg of Dune Zip Zinger arabica coffee beans to", DF1b$`40 kg to`[3]))

row19 <- rbind(cbind("10 kg of Tandem Sun Lamp arabica coffee beans from", DF1b$`10 kg from`[4]),
               cbind("10 kg of Tandem Sun Lamp arabica coffee beans to", DF1b$`10 kg to`[4]),
               cbind("20 kg of Tandem Sun Lamp arabica coffee beans from", DF1b$`20 kg from`[4]),
               cbind("20 kg of Tandem Sun Lamp arabica coffee beans to", DF1b$`20 kg to`[4]),
               cbind("30 kg of Tandem Sun Lamp arabica coffee beans from", DF1b$`30 kg from`[4]),
               cbind("30 kg of Tandem Sun Lamp arabica coffee beans to", DF1b$`30 kg to`[4]),
               cbind("40 kg of Tandem Sun Lamp arabica coffee beans from", DF1b$`40 kg from`[4]),
               cbind("40 kg of Tandem Sun Lamp arabica coffee beans to", DF1b$`40 kg to`[4]))

row20 <- rbind(cbind("10 kg of Verve Kenya Karimikui arabica coffee beans from", DF1b$`10 kg from`[5]),
               cbind("10 kg of Verve Kenya Karimikui arabica coffee beans to", DF1b$`10 kg to`[5]),
               cbind("20 kg of Verve Kenya Karimikui arabica coffee beans from", DF1b$`20 kg from`[5]),
               cbind("20 kg of Verve Kenya Karimikui arabica coffee beans to", DF1b$`20 kg to`[5]),
               cbind("30 kg of Verve Kenya Karimikui arabica coffee beans from", DF1b$`30 kg from`[5]),
               cbind("30 kg of Verve Kenya Karimikui arabica coffee beans to", DF1b$`30 kg to`[5]),
               cbind("40 kg of Verve Kenya Karimikui arabica coffee beans from", DF1b$`40 kg from`[5]),
               cbind("40 kg of Verve Kenya Karimikui arabica coffee beans to", DF1b$`40 kg to`[5]))

arabica <- rbind(row16, row17, row18, row19, row20)

row21 <- rbind(cbind("10 kg of Koa robusta coffee beans from", DF1b$`10 kg from`[6]),
               cbind("10 kg of Koa robusta coffee beans to", DF1b$`10 kg to`[6]),
               cbind("20 kg of Koa robusta coffee beans from", DF1b$`20 kg from`[6]),
               cbind("20 kg of Koa robusta coffee beans to", DF1b$`20 kg to`[6]),
               cbind("30 kg of Koa robusta coffee beans from", DF1b$`30 kg from`[6]),
               cbind("30 kg of Koa robusta coffee beans to", DF1b$`30 kg to`[6]),
               cbind("40 kg of Koa robusta coffee beans from", DF1b$`40 kg from`[6]),
               cbind("40 kg of Koa robusta coffee beans to", DF1b$`40 kg to`[6]))

row22 <- rbind(cbind("10 kg of Volcanica robusta coffee beans from", DF1b$`10 kg from`[7]),
               cbind("10 kg of Volcanica robusta coffee beans to", DF1b$`10 kg to`[7]),
               cbind("20 kg of Volcanica robusta coffee beans from", DF1b$`20 kg from`[7]),
               cbind("20 kg of Volcanica robusta coffee beans to", DF1b$`20 kg to`[7]),
               cbind("30 kg of Volcanica robusta coffee beans from", DF1b$`30 kg from`[7]),
               cbind("30 kg of Volcanica robusta coffee beans to", DF1b$`30 kg to`[7]),
               cbind("40 kg of Volcanica robusta coffee beans from", DF1b$`40 kg from`[7]),
               cbind("40 kg of Volcanica robusta coffee beans to", DF1b$`40 kg to`[7]))

row23 <- rbind(cbind("10 kg of Kicking Horse robusta coffee beans from", DF1b$`10 kg from`[8]),
               cbind("10 kg of Kicking Horse robusta coffee beans to", DF1b$`10 kg to`[8]),
               cbind("20 kg of Kicking Horse robusta coffee beans from", DF1b$`20 kg from`[8]),
               cbind("20 kg of Kicking Horse robusta coffee beans to", DF1b$`20 kg to`[8]),
               cbind("30 kg of Kicking Horse robusta coffee beans from", DF1b$`30 kg from`[8]),
               cbind("30 kg of Kicking Horse robusta coffee beans to", DF1b$`30 kg to`[8]),
               cbind("40 kg of Kicking Horse robusta coffee beans from", DF1b$`40 kg from`[8]),
               cbind("40 kg of Kicking Horse robusta coffee beans to", DF1b$`40 kg to`[8]))

row24 <- rbind(cbind("10 kg of Caribou robusta coffee beans from", DF1b$`10 kg from`[9]),
               cbind("10 kg of Caribou robusta coffee beans to", DF1b$`10 kg to`[9]),
               cbind("20 kg of Caribou robusta coffee beans from", DF1b$`20 kg from`[9]),
               cbind("20 kg of Caribou robusta coffee beans to", DF1b$`20 kg to`[9]),
               cbind("30 kg of Caribou robusta coffee beans from", DF1b$`30 kg from`[9]),
               cbind("30 kg of Caribou robusta coffee beans to", DF1b$`30 kg to`[9]),
               cbind("40 kg of Caribou robusta coffee beans from", DF1b$`40 kg from`[9]),
               cbind("40 kg of Caribou robusta coffee beans to", DF1b$`40 kg to`[9]))

row25 <- rbind(cbind("10 kg of Peet's Coffee Big Bang robusta coffee beans from", DF1b$`10 kg from`[10]),
               cbind("10 kg of Peet's Coffee Big Bang robusta coffee beans to", DF1b$`10 kg to`[10]),
               cbind("20 kg of Peet's Coffee Big Bang robusta coffee beans from", DF1b$`20 kg from`[10]),
               cbind("20 kg of Peet's Coffee Big Bang robusta coffee beans to", DF1b$`20 kg to`[10]),
               cbind("30 kg of Peet's Coffee Big Bang robusta coffee beans from", DF1b$`30 kg from`[10]),
               cbind("30 kg of Peet's Coffee Big Bang robusta coffee beans to", DF1b$`30 kg to`[10]),
               cbind("40 kg of Peet's Coffee Big Bang robusta coffee beans from", DF1b$`40 kg from`[10]),
               cbind("40 kg of Peet's Coffee Big Bang robusta coffee beans to", DF1b$`40 kg to`[10]))

robusta <- rbind(row21, row22, row23, row24, row25)

species_of_beans <- data.frame(rbind(arabica, robusta))
#View(species_of_beans)

species_of_beans_ts <- data.frame(rbind(date, species_of_beans))
species_of_beans_t_ts <- setNames(data.frame(t(species_of_beans_ts[,-1])), species_of_beans_ts[,1])
#write.csv(species_of_beans_t_ts, "~\\Prices_of_quality_coffee_beans_according_to_species_of_beans.csv", row.names = FALSE)

datafile1b <- read.csv("~\\Prices_of_quality_coffee_beans_according_to_species_of_beans.csv")
#View(datafile1b)
colnames(datafile1b) <- colnames(species_of_beans_t_ts)
data_to_file1b <- rbind(datafile1b, species_of_beans_t_ts)
write.csv(data_to_file1b, "~\\Prices_of_quality_coffee_beans_according_to_species_of_beans.csv", row.names = FALSE)


##########
# 2 PAGE #
##########

### 3 TABLE: Prices of kilogram of quality coffee beans according to region

text2 = list()
titles2 <- pdf_clean2 %>% str_replace_all("No data", "") %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% str_remove_all("[:digit:]+.*")
titles2_df <- data.frame(titles2) 
names2_ex <- titles2_df
text2 <- dplyr::bind_rows(names2_ex) %>% rename("Title" = "titles2") 
print(text2)

digits2 = list()
numbers2 <- pdf_clean2 %>% str_extract("[:digit:]+.*") %>% str_replace_all("No|data", "") %>% str_replace_all("\\.", "")
numbers2_df <- data.frame(numbers2)
numbers2_ex <- numbers2_df
digits2 <- dplyr::bind_rows(numbers2_ex) %>% rename("Price of quality coffee beans (EUR/kg)" = "numbers2") 
print(digits2)

digits2 %>%
  separate("Price of quality coffee beans (EUR/kg)", c("Africa from","Africa to", "Central and South America from",
                                                       "Central and South America to"), sep="\\s") -> digits2

DF2 <- cbind(text2, digits2) 
#View(DF2)

DF2 <- DF2[-c(1, 2, 3, 4, 10, 16),] 

row26 <- rbind(cbind("Coffee Bros light roast coffee beans from Africa from", DF2$`Africa from`[1]),
               cbind("Coffee Bros light roast coffee beans from Africa to", DF2$`Africa to`[1]),
               cbind("Coffee Bros light roast coffee beans from Central and South America from", DF2$`Central and South America from`[1]),
               cbind("Coffee Bros light roast coffee beans from Central and South America to", DF2$`Central and South America to`[1]))

row27 <- rbind(cbind("Lifeboost light roast coffee beans from Africa from", DF2$`Africa from`[2]),
               cbind("Lifeboost light roast coffee beans from Africa to", DF2$`Africa to`[2]),
               cbind("Lifeboost light roast coffee beans from Central and South America from", DF2$`Central and South America from`[2]),
               cbind("Lifeboost light roast coffee beans from Central and South America to", DF2$`Central and South America to`[2]))

row28 <- rbind(cbind("Dune Zip Zinger light roast coffee beans from Africa from", DF2$`Africa from`[3]),
               cbind("Dune Zip Zinger light roast coffee beans from Africa to", DF2$`Africa to`[3]),
               cbind("Dune Zip Zinger light roast coffee beans from Central and South America from", DF2$`Central and South America from`[3]),
               cbind("Dune Zip Zinger light roast coffee beans from Central and South America to", DF2$`Central and South America to`[3]))

row29 <- rbind(cbind("Tandem Sun Lamp light roast coffee beans from Africa from", DF2$`Africa from`[4]),
               cbind("Tandem Sun Lamp light roast coffee beans from Africa to", DF2$`Africa to`[4]),
               cbind("Tandem Sun Lamp light roast coffee beans from Central and South America from", DF2$`Central and South America from`[4]),
               cbind("Tandem Sun Lamp light roast coffee beans from Central and South America to", DF2$`Central and South America to`[4]))

row30 <- rbind(cbind("Verve Kenya Karimikui light roast coffee beans from Africa from", DF2$`Africa from`[5]),
               cbind("Verve Kenya Karimikui light roast coffee beans from Africa to", DF2$`Africa to`[5]),
               cbind("Verve Kenya Karimikui light roast coffee beans from Central and South America from", DF2$`Central and South America from`[5]),
               cbind("Verve Kenya Karimikui light roast coffee beans from Central and South America to", DF2$`Central and South America to`[5]))

light_roast2 <- rbind(row26, row27, row28, row29, row30)

row31 <- rbind(cbind("Koa medium roast coffee beans from Africa from", DF2$`Africa from`[6]),
               cbind("Koa medium roast coffee beans from Africa to", DF2$`Africa to`[6]),
               cbind("Koa medium roast coffee beans from Central and South America from", DF2$`Central and South America from`[6]),
               cbind("Koa medium roast coffee beans from Central and South America to", DF2$`Central and South America to`[6]))

row32 <- rbind(cbind("Volcanica medium roast coffee beans from Africa from", DF2$`Africa from`[7]),
               cbind("Volcanica medium roast coffee beans from Africa to", DF2$`Africa to`[7]),
               cbind("Volcanica medium roast coffee beans from Central and South America from", DF2$`Central and South America from`[7]),
               cbind("Volcanica medium roast coffee beans from Central and South America to", DF2$`Central and South America to`[7]))

row33 <- rbind(cbind("Kicking Horse medium roast coffee beans from Africa from", DF2$`Africa from`[8]),
               cbind("Kicking Horse medium roast coffee beans from Africa to", DF2$`Africa to`[8]),
               cbind("Kicking Horse medium roast coffee beans from Central and South America from", DF2$`Central and South America from`[8]),
               cbind("Kicking Horse medium roast coffee beans from Central and South America to", DF2$`Central and South America to`[8]))

row34 <- rbind(cbind("Caribou medium roast coffee beans from Africa from", DF2$`Africa from`[9]),
               cbind("Caribou medium roast coffee beans from Africa to", DF2$`Africa to`[9]),
               cbind("Caribou medium roast coffee beans from Central and South America from", DF2$`Central and South America from`[9]),
               cbind("Caribou medium roast coffee beans from Central and South America to", DF2$`Central and South America to`[9]))

row35 <- rbind(cbind("Peet's Coffee Big Bang medium roast coffee beans from Africa from", DF2$`Africa from`[10]),
               cbind("Peet's Coffee Big Bang medium roast coffee beans from Africa to", DF2$`Africa to`[10]),
               cbind("Peet's Coffee Big Bang medium roast coffee beans from Central and South America from", DF2$`Central and South America from`[10]),
               cbind("Peet's Coffee Big Bang medium roast coffee beans from Central and South America to", DF2$`Central and South America to`[10]))

medium_roast2 <- rbind(row31, row32, row33, row34, row35)

row36 <- rbind(cbind("Sumatra Mandheling dark roast coffee beans from Africa from", DF2$`Africa from`[11]),
               cbind("Sumatra Mandheling dark roast coffee beans from Africa to", DF2$`Africa to`[11]),
               cbind("Sumatra Mandheling dark roast coffee beans from Central and South America from", DF2$`Central and South America from`[11]),
               cbind("Sumatra Mandheling dark roast coffee beans from Central and South America to", DF2$`Central and South America to`[11]))

row37 <- rbind(cbind("Lion Coffee dark roast coffee beans from Africa from", DF2$`Africa from`[12]),
               cbind("Lion Coffee dark roast coffee beans from Africa to", DF2$`Africa to`[12]),
               cbind("Lion Coffee dark roast coffee beans from Central and South America from", DF2$`Central and South America from`[12]),
               cbind("Lion Coffee dark roast coffee beans from Central and South America to", DF2$`Central and South America to`[12]))

row38 <- rbind(cbind("Death Wish dark roast coffee beans from Africa from", DF2$`Africa from`[13]),
               cbind("Death Wish dark roast coffee beans from Africa to", DF2$`Africa to`[13]),
               cbind("Death Wish dark roast coffee beans from Central and South America from", DF2$`Central and South America from`[13]),
               cbind("Death Wish dark roast coffee beans from Central and South America to", DF2$`Central and South America to`[13]))

row39 <- rbind(cbind("Don Pablo dark roast coffee beans from Africa from", DF2$`Africa from`[14]),
               cbind("Don Pablo dark roast coffee beans from Africa to", DF2$`Africa to`[14]),
               cbind("Don Pablo dark roast coffee beans from Central and South America from", DF2$`Central and South America from`[14]),
               cbind("Don Pablo dark roast coffee beans from Central and South America to", DF2$`Central and South America to`[14]))

row40 <- rbind(cbind("Amazon Fresh dark roast coffee beans from Africa from", DF2$`Africa from`[15]),
               cbind("Amazon Fresh dark roast coffee beans from Africa to", DF2$`Africa to`[15]),
               cbind("Amazon Fresh dark roast coffee beans from Central and South America from", DF2$`Central and South America from`[15]),
               cbind("Amazon Fresh dark roast coffee beans from Central and South America to", DF2$`Central and South America to`[15]))

dark_roast2 <- rbind(row36, row37, row38, row39, row40)

two_regions_of_beans <- data.frame(rbind(light_roast2, medium_roast2, dark_roast2))
#View(two_regions_of_beans)

two_regions_of_beans_ts <- data.frame(rbind(date, two_regions_of_beans))
two_regions_of_beans_t_ts <- setNames(data.frame(t(two_regions_of_beans_ts[,-1])), two_regions_of_beans_ts[,1])
#write.csv(two_regions_of_beans_t_ts, "~\\Prices_of_quality_coffee_beans_according_to_regions_of_beans.csv", row.names = FALSE)

datafile2 <- read.csv("~\\Prices_of_quality_coffee_beans_according_to_regions_of_beans.csv")
#View(datafile2)
colnames(datafile2) <- colnames(two_regions_of_beans_t_ts)
data_to_file2 <- rbind(datafile2, two_regions_of_beans_t_ts)
write.csv(data_to_file2, "~\\Prices_of_quality_coffee_beans_according_to_regions_of_beans.csv", row.names = FALSE)