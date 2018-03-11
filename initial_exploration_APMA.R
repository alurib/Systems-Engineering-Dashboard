
library(tidyverse)
library(stringr)

## Inital Exploration of the Applied Mathematics Course Data

engineer1 <- read.csv("~/Box Sync/Systems_Report/engineer_data.csv")
summary(engineer)
engineer <- engineer1 %>% 
  mutate(Year = factor(str_sub(TERM,2,3)),
         Semester = factor(str_sub(TERM,4)),
         floored_catalog = floor(CATALOG_NUMBER/1000))


## 
dropped <- engineer %>% 
  mutate(Dropped = ifelse(STUDENT_ENROLLMENT_STATUS == "Dropped",1,0)) %>% 
  group_by(SUBJECT,floored_catalog,Year) %>% 
  summarise( PercentDropped = mean(Dropped))

ggplot(dropped, aes(Year,PercentDropped, color = factor(floored_catalog) ))+
  geom_point( )+
  geom_line()+
  xlab("Year")+ ylab("Percentage of Students Dropped")+
  ggtitle("Percentage of Students Dropped Over Years")


## Confirms that there is just one class title per catalog number
titles_per_catalog <- engineer %>% 
  group_by(CATALOG_NUMBER) %>% 
  summarise(count = n_distinct(CLASS_TITLE))
summary(titles_per_catalog)

## Looking at mean withdrawls number in a class over years

withdrawls <- engineer %>% 
  filter(ENROLLMENT_TOTAL != 0) %>% 
  mutate(Withdrew = ifelse(OFFICIAL_GRADE == "W",1,0),
         Failed = ifelse(OFFICIAL_GRADE== "F",1,0))%>% 
  group_by(SUBJECT, floored_catalog,Year) %>% 
  summarise(PercentWithdrew = mean(Withdrew),
            PercentFailed = mean(Failed))


ggplot(withdrawls, aes(Year,PercentWithdrew, color = factor(floored_catalog) ))+
  geom_point( )+
  geom_line()+
  xlab("Year")+ ylab("Percentage of Students Withdrew")+
  ggtitle("Percentage of Students Withdrew Over Years")

## Looking at a similar plot for Percent Failed

ggplot(withdrawls, aes(Year,PercentFailed, color = factor(floored_catalog) ))+
  geom_point( )+
  geom_line()+
  xlab("Year")+ ylab("Percentage of Students Failed")+
  ggtitle("Percentage of Students Failed Over Years")

## Number of students stated compared to the data of number of students enrolled in a class or section.

student_comparision <- engineer %>% 
  group_by(SUBJECT,CATALOG_NUMBER,Year, Semester, CLASS_SECTION) %>% 
  summarise(data_count = n(),
            mentioned_count = mean(ENROLLMENT_TOTAL)) %>% 
  mutate(enrollment_count_isequal = ifelse(data_count == mentioned_count,1,0))

## Doest the ENROLLMENT_TOTAL have all the student enrolled to the course in sis whether they dropped
## or withdrew?


## IT WOULD BE INTERESTING TO SEE HOW THE SAME APPLIES TO
## SYSTEMS AND INFORMATION ENGINEERING

