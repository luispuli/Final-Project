
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(forcats)

# Read and clean data -----------------------------------------------------

gdp_growth <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_4770505.csv",
         skip = 4)  %>% 
  clean_names() %>% 
  pivot_longer(starts_with("x"),
               names_to = "Year",
               values_to = "Gdppc_growth") %>% 
  select(-contains(c("indicator","code"))) %>% 
  mutate(Year = as.numeric(str_remove(Year,"x")))

pop_growth <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/API_SP.POP.GROW_DS2_en_csv_v2_4770493.csv",
         skip = 4) %>% 
  clean_names() %>% 
  pivot_longer(starts_with("x"),
               names_to = "Year",
               values_to = "Pop_growth") %>% 
  select(-contains(c("indicator","code"))) %>% 
  mutate(Year = as.numeric(str_remove(Year,"x")))

continent <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/continents-according-to-our-world-in-data.csv") %>%
  select(Entity,Continent)

school_years <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/mean-years-of-schooling-long-run.csv")
names(school_years)[4] <- "School_years"
school_years <- school_years %>% 
  select(-Code)

educ_expend <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/total-government-expenditure-on-education-gdp.csv")
names(educ_expend)[4] <- "Educ_expend"
educ_expend <- educ_expend %>% 
  select(-Code)
educ_expend

health <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/life-expectancy-vs-healthcare-expenditure.csv")
names(health)[c(4,5)] <- c("Life_expec","health_expdpc")
health <- health %>% 
  select(1:5, -2)

migration <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/migration.csv") %>% 
  select(contains("Net"), Year, Country) %>% 
  select(c(1,3,4)) %>% 
  clean_names()

Age <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/median-age.csv") %>% 
  select(c(1,3,4))
names(Age)[3] <- "Median_age"

marriage <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/marriage-rate-per-1000-inhabitants.csv") %>% 
  select(c(1,3,4))
names(marriage)[3] <- "marriage_per_1000"

civil_rights <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/civil-liberties-fh.csv") %>%
  select(c(1,3,4))
names(civil_rights)[3] <- "civil_rights"
#Recall 1 is best and 7 is worst

geography <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/average-latitude-longitude-countries.csv") %>% 
  select(c(2,3,4))

religion <- read.csv("C:/Users/Asus/OneDrive/Desktop/Data TFM/religion.txt",
         sep = "") %>% 
  select(Country, Feel) %>% 
  rename("relig_feel" = "Feel")


# Merging datasets --------------------------------------------------------

df <- gdp_growth %>% 
  left_join(pop_growth,
            by = c("country_name","Year")) %>% 
  left_join(continent,
            by = c("country_name" = "Entity")) %>% 
  left_join(school_years,
            by = c("country_name" = "Entity","Year")) %>%
  left_join(educ_expend,
            by = c("country_name" = "Entity","Year")) %>% 
  left_join(health,
           by = c("country_name" = "Entity","Year")) %>% 
  left_join(migration,
            by = c("country_name" = "country","Year" = "year")) %>% 
  left_join(Age,
            by = c("country_name" = "Entity","Year")) %>% 
  left_join(marriage,
            by = c("country_name" = "Entity","Year")) %>% 
  left_join(civil_rights,
            by = c("country_name" = "Entity","Year"))%>% 
  left_join(geography,
            by = c("country_name" = "Country"))%>% 
  left_join(religion,
            by = c("country_name" = "Country"))

read.csv("C:/Users/ignat/OneDrive/Desktop/Data TFM/Country_data.csv") %>% 
  ggplot()+
  aes(relig_feel)+
  geom_density()+
  facet_wrap(~civil_rights)

read.csv()

da
