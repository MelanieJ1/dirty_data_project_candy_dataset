<h1> Candy Data Report </h1>

<h2>Introduction</h2>
<p>The candy ranking raw data sets contain information about candy ratings for specific types of candy for the years 2015, 2016, and 2017.  The datasets used are boing-boing-candy-2015.xlsx, boing-boing-candy-2016.xlsx and boing-boing-candy-2017.xlsx </p> 

<h2>Assumptions made about the data</h2>
<li>To convert country values to a standard set of categories, entries were altered to country names that were the nearest matches to the entries that were given.</li> 
<li> The data for 2015 does not contain gender or year columns so these were added as NA columns.</li> 

<h2>Steps for Cleaning data</h2>
<p>Each of the raw datasets were cleaned by year.  They were then combined and the country column was cleaned.</p>
<h3>Packages used:</h3>
<li>library(tidyverse)</li>
<li>library(dplyr)</li>
<li>library(janitor)</li>
<li>library(tidyr)</li>
<li>library(here)</li>
<li>library(readxl)</li>


<h3>Separating timestamp column to get year column</h3>
candy_2015_year <- separate(candy_2015, Timestamp, into = "year", sep="-")


<h3>Adjusting column names</h3>
 candy_2015_treat <- rename(candy_2015_pivot,</br>
                            "going_trick_or_treating" =  "Are you going actually going trick or treating yourself?")

<h3>Adding gender and country columns for 2015 data.</h3>
candy_2015_col_gender <- candy_2015_treat_b %>%<br/>
  mutate(gender = NA, country = NA)<br/>

<h3>selecting columns from 2016 data.</h3>
candy_2016_table <- candy_2016_age %>%<br/>
  select(1:5, 7:106, 123)<br/>

<h3>Subsetting columns</h3>
<p>Unnecessary columns were removed using subset().</p> 
candy_2016_cols <- subset(candy_2016_table, select = -c(11, 14, 20, 21, 25, 26,</br>
                                                        30, 31, 37, 42, 45, 48, 67, 68, <br/>
                                                        77, 78, 89, 100:104, 106))</br>


<h3>Pivot_longer on candy type and ratings columns.</h3>
candy_2016_pivot <- candy_2016_cols %>%<br/>
  pivot_longer( cols = -c("year", "age", "Are you going actually going trick or treating yourself?","Your gender:", "Which country do you live in?"), names_to = "candy", values_to = "rating")

<h3>Removing brackets from candy column.</h3>
candy_2016_treat_a <- candy_2016_treat$candy %>%<br/>
  str_remove_all(fixed("[")) %>%<br/>
  str_remove_all(fixed("]"))<br/>

candy_2016_treat_b <- candy_2016_treat %>%<br/>
  mutate(candy = candy_2016_treat_a)<br/>

<h3>Changing age column from character to numeric.</h3>
candy_2016_country <- transform(candy_2016_country,<br/>
                                   age = as.numeric(as.factor(age)))<br/>
glimpse(candy_2016_country)<br/>



<h3>Changing NAs in age column to 0.</h3>
candy_2016_clean <- candy_2016_country %>%<br/>
  mutate(age = coalesce(age, 0))<br/>

<p>Checking NAs across all columns</p>
candy_2016_clean %>%<br/>
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))<br/>


<h3>Combining selected data from each year using rbind().</h3>
candy_data_total <- rbind(candy_2015_clean, candy_2016_clean, candy_2017_clean)<br/>
glimpse(candy_data_total)<br/>

candy_data_total %>%<br/>
  summarise(across(.cols = everything(), .fns = ~ sum(is.na(.x))))<br/>

<h3>Adding rating_id column to each rating.</h3>
candy_data_total <- tibble::rowid_to_column(candy_data_total, "rating_id")<br/>
glimpse(candy_data_total)<br/>

candy_data_total <- transform(candy_data_total,<br/>
                                rating_id = as.numeric(as.factor(rating_id)))<br/>

glimpse(candy_data_total)<br/>


<h3>Converting invalid country values to NA.</h3>
unique(candy_data_total$country)<br/>

candy_data_total[candy_data_total == "Fear and Loathing"] <- NA<br/>
candy_data_total[candy_data_total == "I don't know anymore"] <- NA<br/>


<h3>Converting country entries to standard categories of country<h3/>
candy_data_2 <- candy_data_total<br/>
candy_data_2[candy_data_2 == "usa"] <- "United States"<br/>
candy_data_2[candy_data_2 == "UK"] <- "United Kingdom"<br/>

<h3> Writing into .csv</h3>
The cleaned data was written into a .csv file in the clean data folder.
write.csv(candy_data_2, ("clean_data/candy_data_clean.csv"))
      


<h2>Answers to Analysis Questions</h2> 
<p>1. What is the total number of candy ratings given across the three years. (Number of candy ratings, not the number of raters. Don't count missing values)</p>
candy_dataset %>%<br/>
  filter(!is.na(rating)) %>%<br/>
  count(rating_id)<br/>
The total number of candy ratings given over the three years is 595,654. 

<p>2. What was the average age of people who are going out trick or treating?</p>
candy_dataset %>%<br/>
  filter(going_trick_or_treating == "Yes") %>%<br/>
  mean(age)<br/>

<p>3. What was the average age of people who are not going trick or treating?</p>
candy_dataset2 <- candy_dataset %>% <br/>
  transform(candy_dataset, age = as.numeric(as.factor(age))) %>% <br/>
  mutate(age = coalesce(age, 0))<br/>

candy_dataset2 %>%<br/>
  filter(going_trick_or_treating == "No") %>%<br/>
  mean("age")<br/>


<p>4. How many people rated Starburst as despair?</p>
candy_dataset %>%<br/>
  filter(candy == "Starburst", rating == "DESPAIR") %>%<br/>
  count()<br/>
```<br/>

1990 people rated starburst as “DESPAIR”.

