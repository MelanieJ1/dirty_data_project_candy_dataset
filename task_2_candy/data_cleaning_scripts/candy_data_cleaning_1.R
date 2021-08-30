######  1.4 Task 4 - Halloween Candy Data

#### Loading in packages

library(tidyverse)
library(dplyr)
library(janitor)
library(tidyr)
library(here)
library(readxl)


## Loading in candy data.

candy_2015 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
View(candy_2015)

candy_2016 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")

candy_2017 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")


######################################## Candy 2015 data

glimpse(candy_2015)

glimpse(candy_2016)

## 
head(candy_2015)



## candy_2015, separating timestamp to get year column

candy_2015_year <- separate(candy_2015, Timestamp, into = "year", sep="-")


  
candy_2015_age <- rename(candy_2015_year, age = "How old are you?")





### selecting columns from 2015


candy_2015_table <- candy_2015_age %>%
  select(1, 2, 3, 4:115)



candy_2015_table_cols <- subset(candy_2015_table, select = -c(16, 18, 23, 26, 27,
                                                              28, 33, 34, 38, 41, 45, 56, 57, 63,
                                                              71, 72, 82, 88, 90, 93:95, 97:113))



### pivot columns in candy_2015_table_cols


candy_2015_pivot <- candy_2015_table_cols %>%
  pivot_longer( cols = -c("year", "age", "Are you going actually going trick or treating yourself?"), 
                names_to = "candy", values_to = "rating")

head(candy_2015_pivot)



## adjusting column names
 candy_2015_treat <- rename(candy_2015_pivot,
                            "going_trick_or_treating" =  "Are you going actually going trick or treating yourself?")



 ## Removing brackets fom candy column in 2015 data
 
candy_2015_treat_a <- candy_2015_treat$candy %>%
  str_remove_all(fixed("[")) %>%
  str_remove_all(fixed("]"))

candy_2015_treat_b <- candy_2015_treat %>%
  mutate(candy = candy_2015_treat_a)


## Adding gender and country columns for 2015 data.

candy_2015_col_gender <- candy_2015_treat_b %>%
  mutate(gender = NA, country = NA)


## Changing age column from character to numeric.

candy_2015_col_gender <- transform(candy_2015_col_gender,
                             age = as.numeric(as.factor(age)))


## Changing NAs in age column to 0.

candy_2015_clean <- candy_2015_col_gender %>%
  mutate(age = coalesce(age, 0))

candy_2015_clean %>%
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))





########################################## Candy 2016 data

## Separating Timestamp column to get year.

candy_2016_year <- separate(candy_2016, Timestamp, into = "year", sep="-")



## Changing column name to age.


candy_2016_age <- rename(candy_2016_year, age = "How old are you?")


### selecting columns from 2016 data.


candy_2016_table <- candy_2016_age %>%
  select(1:5, 7:106, 123)

candy_2016_cols <- subset(candy_2016_table, select = -c(11, 14, 20, 21, 25, 26,
                                                        30, 31, 37, 42, 45, 48, 67, 68, 
                                                        77, 78, 89, 100:104, 106))



## Pivot_longer candy type and ratings columns.

candy_2016_pivot <- candy_2016_cols %>%
  pivot_longer( cols = -c("year", "age", "Are you going actually going trick or treating yourself?","Your gender:", "Which country do you live in?"), names_to = "candy", values_to = "rating")


candy_2016_treat <- rename(candy_2016_pivot,"going_trick_or_treating" =  "Are you going actually going trick or treating yourself?")



## Removing brackets for candy column.

candy_2016_treat_a <- candy_2016_treat$candy %>%
  str_remove_all(fixed("[")) %>%
  str_remove_all(fixed("]"))

candy_2016_treat_b <- candy_2016_treat %>%
  mutate(candy = candy_2016_treat_a)


## Changing column name to gender.

candy_2016_gender_col <- rename(candy_2016_treat_b, "gender" = "Your gender:")


## Changing column name to country.

candy_2016_country <- rename(candy_2016_gender_col, "country" = "Which country do you live in?")


## Checking unique value inputs for columns.

unique(candy_2016_country$country)
unique(candy_2016_country$rating)
unique(candy_2016_country$going_trick_or_treating)
unique(candy_2016_country$gender)




## Changing age column from character to numeric

candy_2016_country <- transform(candy_2016_country,
                                   age = as.numeric(as.factor(age)))
glimpse(candy_2016_country)



## Changing NAs in age column to 0.

candy_2016_clean <- candy_2016_country %>%
  mutate(age = coalesce(age, 0))

candy_2016_clean %>%
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))







######################################## Candy 2017 data.

## selecting columns from 2017 data.

candy_2017_table <- candy_2017 %>%
  select(2:5, 7:109)

candy_2017_cols <- subset(candy_2017_table, select = -c(10, 13, 19, 20, 24, 25, 29, 30, 36, 41, 44, 47,
                                                        65:68, 77, 79, 84, 90, 102, 103, 105, 106))
                          



## Renaming column to country

candy_2017_country <- rename(candy_2017_cols, "country" = "Q4: COUNTRY")


## Renaming column to age.

candy_2017_age <- rename(candy_2017_country, "age" = "Q3: AGE")


## Renaming column to gender.

candy_2017_gender <- rename(candy_2017_age, "gender" = "Q2: GENDER")



## Renaming column to going_trick_or_treating.

candy_2017_treat <- rename(candy_2017_gender, "going_trick_or_treating" = "Q1: GOING OUT?")



## Pivoting candy columns.

candy_2017_pivot <- candy_2017_treat %>%
  pivot_longer(cols = -c("country", "age", "going_trick_or_treating","gender"), 
               names_to = "candy", values_to = "rating")
                          


## Adding year column to 2017 data

candy_2017_year_col <- candy_2017_pivot %>%
  mutate(year = 2017)



### Remove "Q6 |" from candy column

candy_2017_year_brackets <- candy_2017_year_col$candy %>%
  str_remove_all(fixed("Q6 |"))

candy_2017_no_q <- candy_2017_year_col %>%
  mutate(candy = candy_2017_year_brackets)


glimpse(candy_2017_no_q)

## Changing age column from character to numeric

candy_2017_no_q <- transform(candy_2017_no_q,
                                   age = as.numeric(as.factor(age)))



## Changing NAs in age column to 0.
candy_2017_clean <- candy_2017_no_q %>%
  mutate(age = coalesce(age, 0))

candy_2017_clean %>%
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))



unique(candy_2017_year_col$country)
unique(candy_2017_year_col$gender)
unique(candy_2017_year_col$going_trick_or_treating)


################################ Combining selected data from each year using rbind().


candy_data_total <- rbind(candy_2015_clean, candy_2016_clean, candy_2017_clean)

glimpse(candy_data_total)

candy_data_total %>%
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))



## Adding rating_id column to each rating.

candy_data_total <- tibble::rowid_to_column(candy_data_total, "rating_id")
glimpse(candy_data_total)

candy_data_total <- transform(candy_data_total,
                                rating_id = as.numeric(as.factor(rating_id)))

glimpse(candy_data_total)


## Converting incorrect country values to NA.

unique(candy_data_total$country)


candy_data_total[candy_data_total == "Fear and Loathing"] <- NA
candy_data_total[candy_data_total == "I don't know anymore"] <- NA
candy_data_total[candy_data_total == "subscribe to dm4uz3 on youtube"] <- NA
candy_data_total[candy_data_total == "1"] <- NA
candy_data_total[candy_data_total == "Narnia"] <- NA
candy_data_total[candy_data_total == "Atlantis"] <- NA
candy_data_total[candy_data_total == "A"] <- NA
candy_data_total[candy_data_total == "32"] <- NA
candy_data_total[candy_data_total == "45"] <- NA
candy_data_total[candy_data_total == "insanity lately"] <- NA
candy_data_total[candy_data_total == "cascadia"] <- NA
candy_data_total[candy_data_total == "46"] <- NA
candy_data_total[candy_data_total == "Earth"] <- NA
candy_data_total[candy_data_total == "Europe"] <- NA
candy_data_total[candy_data_total == "35"] <- NA
candy_data_total[candy_data_total == "Denial"] <- NA
candy_data_total[candy_data_total == "Not the USA or Canada"] <- NA


candy_data_total[candy_data_total == "The republic of Cascadia"] <- NA
candy_data_total[candy_data_total == "30.0"] <- NA
candy_data_total[candy_data_total == "see above"] <- NA
candy_data_total[candy_data_total == "45.0"] <- NA
candy_data_total[candy_data_total == "god's country"] <- NA
candy_data_total[candy_data_total == "44.0"] <- NA
candy_data_total[candy_data_total == "54.0"] <- NA
candy_data_total[candy_data_total == "Somewhere"] <- NA
candy_data_total[candy_data_total == "one of the best ones"] <- NA
candy_data_total[candy_data_total == "there isn't one for old men"] <- NA
candy_data_total[candy_data_total == "Cascadia"] <- NA
candy_data_total[candy_data_total == "47.0"] <- NA
candy_data_total[candy_data_total == "51.0"] <- NA
candy_data_total[candy_data_total == "this one"] <- NA
candy_data_total[candy_data_total == "Neverland"] <- NA
candy_data_total[candy_data_total == "A tropical island south of the equator"] <- NA
candy_data_total[candy_data_total == "soviet canuckistan"] <- NA

unique(candy_data_total$country)


candy_data_2 <- candy_data_total
candy_data_2[candy_data_2 == "usa"] <- "United States"
candy_data_2[candy_data_2 == "UK"] <- "United Kingdom"
candy_data_2[candy_data_2 == "United States of America"] <-"United States"
candy_data_2[candy_data_2 == "uSA"] <- "United States"
candy_data_2[candy_data_2 == "united states"] <- "United States"

candy_data_2[ candy_data_2 == "canada"] <- "Canada"
candy_data_2[candy_data_2 == "us"] <- "United States" 
candy_data_2[candy_data_2 == "france"] <- "France"
candy_data_2[candy_data_2 == "USSA"] <- "United States"
candy_data_2[candy_data_2 == "U.S.A."] <- "United States" 
candy_data_2[candy_data_2 =="england"] <- "United Kingdom"
candy_data_2[candy_data_2 == "uk"] <- "United Kingdom"
candy_data_2[candy_data_2 == "Murica"] <- "United States"
candy_data_2[candy_data_2 =="USA!"] <- "United States"
candy_data_2[candy_data_2 == "USA (I think but it's an election year so who can really tell)"] <- "United States"
candy_data_2[candy_data_2 == "Usa"] <- "United States"


candy_data_2[candy_data_2 =="U.S."] <- "United States"

candy_data_2[candy_data_2 =="Us"] <- "United States" 
candy_data_2[candy_data_2 =="America"] <- "United States"
candy_data_2[candy_data_2 =="Units States"] <- "United States" 
candy_data_2[candy_data_2 == "belgium"] <- "Belgium" 
candy_data_2[candy_data_2 == "croatia"] <- "Croatia" 
candy_data_2[candy_data_2 == "United states"] <- "United States"
candy_data_2[candy_data_2 == "England"] <- "United Kingdom" 
candy_data_2[candy_data_2 =="USA USA USA"] <- "United States"
candy_data_2[candy_data_2 == "the best one - usa"] <- "United States"
candy_data_2[candy_data_2 =="USA! USA! USA!"] <- "United States"
candy_data_2[candy_data_2 == "españa"] <- "Spain" 
candy_data_2[candy_data_2 =="u.s."] <- "United States"
candy_data_2[candy_data_2 =="United Kindom"] <- "United Kingdom" 
candy_data_2[candy_data_2 =="hungary"] <- "Hungary"
candy_data_2[candy_data_2 =="united states of america"] <- "United States"
candy_data_2[candy_data_2 =="Brasil"] <- "Brazil"
candy_data_2[candy_data_2 == "USA!!!!!!"] <- "United States" 
candy_data_2[candy_data_2 == "EUA"] <- "United States"
candy_data_2[candy_data_2 == "USA! USA!"] <- "United States" 
candy_data_2[candy_data_2 == "United Sates"] <- "United States"
                                        
candy_data_2[candy_data_2 =="Sub-Canadian North America... 'Merica"] <-"United States"
candy_data_2[candy_data_2 == "Trumpistan"] <- "United States"
candy_data_2[candy_data_2 == "U.s."] <- "United States"
candy_data_2[candy_data_2 == "Merica"] <- "United States" 
candy_data_2[candy_data_2 == "germany"] <- "Germany"
                                        
candy_data_2[candy_data_2 == "UNited States"] <-"United States" 
candy_data_2[candy_data_2 == "kenya"] <-"Kenya"
candy_data_2[candy_data_2 == "Netherlands"] <- "The Netherlands" 
candy_data_2[candy_data_2 == "United Stetes"] <- "United States"
candy_data_2[candy_data_2 =="america"] <- "United States"
candy_data_2[candy_data_2 == "USA USA USA USA"] <- "United States" 
candy_data_2[candy_data_2 == "United  States of America"] <- "United States" 
candy_data_2[candy_data_2 == "netherlands"] <- "The Netherlands"
candy_data_2[candy_data_2 == "netherlands"] <-"The Netherlands"
candy_data_2[candy_data_2 =="United State"] <- "United States"
candy_data_2[candy_data_2 == "United staes"] <-"United States"
candy_data_2[candy_data_2 =="u.s.a."] <- "United States"
candy_data_2[candy_data_2 == "USAUSAUSA"] <- "United States"
candy_data_2[candy_data_2 == "finland"] <- "Finland"
            
candy_data_2[candy_data_2 == "unhinged states"] <- "United States"
candy_data_2[candy_data_2 =="US of A"] <- "United States"
candy_data_2[candy_data_2 == "Unites States"] <- "United States"
candy_data_2[candy_data_2 == "USA? Hard to tell anymore.."] <- "United States"
candy_data_2[candy_data_2 ==  "merica"] <- "United States"
candy_data_2[candy_data_2 == "usas"] <- "United States"

candy_data_2[candy_data_2 == "Pittsburgh"] <- "United States"
candy_data_2[candy_data_2 == "australia"] <- "Australia"
candy_data_2[candy_data_2 == "can"] <- "Canada"
candy_data_2[candy_data_2 == "Canae"] <- "Canada"
candy_data_2[candy_data_2 == "New York"] <- "United States"

candy_data_2[candy_data_2 == "California"] <- "United States"
candy_data_2[candy_data_2 == "USa"] <- "United States"
candy_data_2[candy_data_2 == "South africa"] <- "South Africa"
candy_data_2[candy_data_2 == "I pretend to be from Canada, but I am really from the United States."] <- "United States"
candy_data_2[candy_data_2 == "Uk"] <- "United Kingdom"
candy_data_2[candy_data_2 == "Scotland"] <- "United Kingdom"
candy_data_2[candy_data_2 == "United Stated"] <- "United States"
candy_data_2[candy_data_2 == "Ahem....Amerca"] <- "United States"
candy_data_2[candy_data_2 =="UD"] <- "United States"
candy_data_2[candy_data_2 == "New Jersey"] <- "United States"
candy_data_2[candy_data_2 == "CANADA"] <- "Canada"
candy_data_2[candy_data_2 == "United ststes"] <- "United States"
candy_data_2[candy_data_2 == "United Statss"] <- "United States"
candy_data_2[candy_data_2 == "endland"] <- "United Kingdom"
candy_data_2[candy_data_2 =="murrika"] <- "United States"
candy_data_2[candy_data_2 == "USAA"] <- "United States"
candy_data_2[candy_data_2 =="Alaska"] <- "United States"
candy_data_2[candy_data_2 == "united States"] <- "United States" 
candy_data_2[candy_data_2 =="N. America"] <- "United States"
candy_data_2[candy_data_2 == "hong kong"] <- "Hong Kong"
candy_data_2[candy_data_2 == "spain"] <- "Spain"
candy_data_2[candy_data_2 == "u s a"] <- "United States"
candy_data_2[candy_data_2 =="United Statea"] <- "United States"
candy_data_2[candy_data_2 == "united ststes"] <- "United States"
candy_data_2[candy_data_2 == "United kingdom"] <- "United Kingdom"
candy_data_2[candy_data_2 == "USA USA USA!!!!"] <- "United States"
candy_data_2[candy_data_2 == "Ahem....Amerca"] <- "United States"


candy_data_2[candy_data_2 == "US"] <- "United States" 
candy_data_2[candy_data_2 == "The Yoo Ess of Aaayyyyyy"] <- "United States" 
candy_data_2[candy_data_2 == "North Carolina"] <- "United States" 
candy_data_2[candy_data_2 == "U.K."] <- "United Kingdom"
candy_data_2[candy_data_2 == "unite states"] <- "United States"
candy_data_2[candy_data_2 == "Can"] <- "Canada"
candy_data_2[candy_data_2 == "sweden"] <- "Sweden"
candy_data_2[candy_data_2 == "Unied States"] <- "United States"
candy_data_2[candy_data_2 == "Canada`"] <- "Canada"
candy_data_2[candy_data_2 == "See above"] <- NA 
candy_data_2[candy_data_2 == "The United States"] <- "United States"
candy_data_2[candy_data_2 == "U S"] <- "United States"
candy_data_2[candy_data_2 == "The United States of America"] <- "United States"
candy_data_2[candy_data_2 == "'merica"] <- "United States" 
candy_data_2[candy_data_2 == "USA"] <- "United States"

unique(candy_data_2$country)
glimpse(candy_data_2)

#### Writing into .csv
write.csv(candy_data_2, ("clean_data/candy_data_clean.csv"))
      
                    
