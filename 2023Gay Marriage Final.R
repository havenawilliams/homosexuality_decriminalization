#This is the replication file for the confirmatory analysis of gay marriage laws analysis by rulesforpower
#Importing/ Creating Data----------------------------------------------------------------------------------
library(tidyverse)
library(countrycode)

setwd("G:/My Drive/R/ON RFP Writings/02 Theory Code Files/Gay Marriage Research Paper Files")

years <- c(-99, 1791:2020)
years <- data.frame(years)

countrynames <- read.csv('countrynames_gm.csv', fileEncoding="UTF-8-BOM")

gm_data <- read.csv('gm_data_fixed2.csv', fileEncoding="UTF-8-BOM")

p5 <- read.csv('p5_condensed.csv', fileEncoding="UTF-8-BOM")

p5_wb <- countrycode(sourcevar = p5$country, "country.name", "wb")

p5_complete <- data.frame(p5, p5_wb)

p5_complete$merge_key <- paste(p5_complete$p5_wb, "_", p5_complete$year, sep = "")

gm_religion <- read.csv('gm_religion.csv', fileEncoding="UTF-8-BOM")

worldbank_indicators <- read.csv('worldbank_internet.csv', fileEncoding="UTF-8-BOM")
#note, internet is already as a percent

#Merge----------------------------------------------------------------------------------------------------
#It's not elegant, but I didn't want to take the time to learn a more efficient way

df <- merge(years, countrynames, all.x = TRUE, all.y = TRUE)

df2 <- df %>%
  unite(merge_key, Code, years, sep = "_", remove = FALSE)

df3 <- left_join(df2, gm_data, by = 'merge_key', keep = FALSE)

df4 <- full_join(df3, p5_complete, by = 'merge_key', keep = FALSE)

df5 <- full_join(df4, gm_religion, by = 'merge_key', keep = FALSE)
df5 <- subset(df5, select = -c(country.x, wb_code.x, country.y, wb_code.y))

master_dataset <- full_join(df5, worldbank_indicators, by = 'merge_key', keep = FALSE)

write.csv(master_dataset, file = "2023master_dataset.csv")

#---------------------------------------------------------------------------------------------------
#Getting rid of useless columns, rows
master_dataset <- subset(master_dataset, select = -c(merge_key.x, country.x, wb_code.x, ))
view(master_dataset)

#Filling General Data
master_fill <- master_dataset %>%
  group_by(Code) %>%
  fill(c(legal, gay_marriage, civil_unions, protections), .direction = c("down")) %>%
  fill(c(legal_year, illegal_year, year_passed_gm, gm_method, cu_method, year_passed_cu,
         punishment, physical_punishment, prison_punishment, capital_punishment), .direction = c("downup"))

# #Filling in legal_year
# master_fill$legal[master_fill$years<master_fill$legal_year] <- 0
# master_fill$legal[master_fill$years>=master_fill$legal_year] <- 1
# master_fill$legal[master_fill$legal_year == -99] <- 0
# 
# #Filling in gm_year
# master_fill$gay_marriage[master_fill$year_passed_gm>master_fill$year] <- 0
# master_fill$gay_marriage[master_fill$year_passed_gm<=master_fill$year] <-1
# master_fill$gay_marriage[master_fill$years == -99] <- 0
# 
# #Filling in cu_year
# master_fill$civil_unions[master_fill$year_passed_cu>master_fill$year] <- 0
# master_fill$civil_unions[master_fill$year_passed_cu<=master_fill$year] <-1
# master_fill$civil_unions[master_fill$years == -99] <- 0
# 
# #Filling in punishment according to legal year
# 
# #master_fill <- master_fill %>%
# #master_fill$punishment = ifelse(master_fill$legal_year > master_fill$illegal_year, fill(c(master_fill$punishment), .direction = c("down")), 0)

#Replace NAs

view(master_fill)

master_fill$legal <- master_fill$legal %>% replace_na(0)
master_fill$protections <- master_fill$protections %>% replace_na(0)
master_fill$gay_marriage <- master_fill$gay_marriage %>% replace_na(0)
master_fill$civil_unions <- master_fill$civil_unions %>% replace_na(0)

view(master_fill) #DONE

#Descriptive Statistics-----------------------------------------------------------------------------
#GDP

head(master_fill)
view(master_fill)

master_fill %>% filter(year == 2018) %>% summary()
master_fill <- master_fill[!is.na(master_fill$polity),]


#Distribution of polity scores
ggplot(master_fill, aes(polity)) + geom_bar() + coord_cartesian(xlim = c(-10, 10))

#Distribution of same sex
ggplot(subset(master_fill, years== 2018), aes(gay_marriage)) +
  geom_bar()

#State of Gay Marriage in 2018
master_fill_2018 <- master_fill %>% filter(years == 2018)
view(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)

#Internet above 80, all governments
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(internet_pop >= 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage) #add in NAs
table(master_fill_2018$punishment) #Mexico is NA is no

#Internet below 80, all governments
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(internet_pop < 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)

#State of democracy
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity >=6)
nrow(master_fill_2018)
view(master_fill_2018)#Indonesia manually counted as illegal and low, Mexico as low and legal
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity < 6) #manually add Indonesia and Mexico to low, no punish
nrow(master_fill_2018)

#FINAL BREAKDOWN AT 80 PERCENT INTERNET
#Democracy, high internet
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity >=6) %>% filter(internet_pop >= 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)
view(master_fill_2018)

#Democracy, low internet
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity >=6) %>% filter(internet_pop < 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)

#Non democracy, high internet
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity < 6) %>% filter(internet_pop > 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)
view(master_fill_2018)

#Non democracy, low internet
master_fill_2018 <- master_fill %>% filter(years == 2018) %>% filter(polity < 6) %>% filter(internet_pop < 80)
nrow(master_fill_2018)
table(master_fill_2018$gay_marriage)
table(master_fill_2018$punishment)

#Population

#Polity

#Tests----------------------------------------------------------------------------------------------

