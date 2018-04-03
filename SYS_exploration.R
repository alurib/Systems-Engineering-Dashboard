
library(tidyverse)
library(stringr)

## Data for all courses across universities
systems <- read.csv("engineer_data_full.csv")

systems1 <- systems %>% 
  mutate(Year = as.numeric(str_sub(TERM,2,3)),
         Semester = ordered(str_sub(TERM,4),labels = c("January","Spring","Summer","Fall")),
         floored_catalog = factor(floor(CATALOG_NUMBER/1000)),#,labels = c("2000","3000","4000","6000","7000")),
         enteringterm = factor(enteringterm),
         enter_year = factor(enter_year),
         CATALOG_NUMBER = factor(CATALOG_NUMBER),
         OFFICIAL_GRADE = factor(OFFICIAL_GRADE),
         Grade_Collapsed = OFFICIAL_GRADE,
         academic_year = factor(if_else(Semester == "Fall", Year,Year-1 )))

## Subsetting system courses
systems_courses <- systems %>% 
  filter(SUBJECT == "SYS") %>% 
  mutate(Year = as.numeric(str_sub(TERM,2,3)),
         Semester = ordered(str_sub(TERM,4),labels = c("January","Spring","Summer","Fall")),
         floored_catalog = factor(floor(CATALOG_NUMBER/1000),labels = c("2000","3000","4000","6000","7000")),
         enteringterm = factor(enteringterm),
         enter_year = factor(enter_year),
         CATALOG_NUMBER = factor(CATALOG_NUMBER),
         OFFICIAL_GRADE = factor(OFFICIAL_GRADE),
         Grade_Collapsed = OFFICIAL_GRADE,
         academic_year = factor(if_else(Semester == "Fall", Year,Year-1 )))
summary(systems_courses)

levels(systems_courses$academic_year)

############################################################################

## Dropped Cases in SYS Courses

dropped_sys_catalog <- systems_courses %>% 
  mutate(Dropped = ifelse(STUDENT_ENROLLMENT_STATUS == "Dropped",1,0)) %>% 
  group_by(academic_year,Semester,CATALOG_NUMBER) %>% 
  summarise( PercentDropped = mean(Dropped)) %>% 
  filter(PercentDropped != 1)

## LABELLING OUTLIERS

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outlier <- dropped_sys_catalog %>%
  group_by(academic_year,Semester) %>%
  mutate(outlier = ifelse(is_outlier(PercentDropped), as.numeric(as.character(CATALOG_NUMBER)), as.numeric(NA))) %>%
  ungroup()

## BOX PLOT FOR Drop Percentage  
ggplot(outlier,aes(academic_year, PercentDropped))+
  geom_boxplot()+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)+
  facet_wrap(~Semester)+
  ylab("Percentage of Students Dropped")+
  ggtitle("Drop Percentage by Year")


# Drop Percentage by Year -------------------------------------------------

dropped_sys_flooredcatalog <- systems_courses %>% 
  mutate(Dropped = ifelse(STUDENT_ENROLLMENT_STATUS == "Dropped",1,0)) %>% 
  group_by(SUBJECT,floored_catalog,academic_year,Semester ) %>% 
  summarise( PercentDropped = mean(Dropped)) %>% 
  filter(PercentDropped != 1)

dropped_sys_flooredcatalog %>% 
  filter(floored_catalog %in% c("2000","3000","4000")) %>% 
  ggplot(aes(academic_year, PercentDropped, fill = Semester))+
  geom_bar( stat = "identity", color = "black")+
  facet_grid(Semester~floored_catalog)+
  ylab("Percentage of Students Dropped")+
  ggtitle("Drop Percentage by Year")

######################################################################

## Grouping by Subject, Floored_Catalog and Year to get the percentage of withdrawls from the course
## and the fail percentage of a course.

withdraw_fail_sys <- systems_courses %>% 
  filter(ENROLLMENT_TOTAL != 0) %>% 
  mutate(Withdrew = ifelse(OFFICIAL_GRADE == "W",1,0),
         Failed = ifelse(OFFICIAL_GRADE== "F",1,0))%>% 
  group_by(SUBJECT, floored_catalog,academic_year,Semester) %>% 
  summarise(PercentWithdrew = mean(Withdrew),
            PercentFailed = mean(Failed),
            Count = n())

## Plot showing percentage of students withdrew from classes
ggplot(withdraw_fail_sys, aes(x= academic_year,y = PercentWithdrew))+
  geom_bar( stat = "identity", color = "black")+
  facet_grid(Semester~floored_catalog)+
  ylab("Percentage of Students Withdrew")+
  ggtitle("Withdrawl Percentage by Year")

## Plot showing percentage of students failed
ggplot(withdraw_fail_sys, aes(x= academic_year,y = PercentFailed, color = Semester))+
  geom_bar( stat = "identity", color = "black")+
  facet_grid(Semester~floored_catalog)+
  ylab("Percentage of Students Failed")+
  ggtitle("Fail Percentage by Year")

###################################################################################

## Total number of students enrolled in same level courses in different years seems
## off. Checking why the number of students enrolled in 3000 and 4000 level courses 
## are low in the year 2009.
systems_courses %>% 
  filter(floored_catalog %in% c("3000","4000")
         & Year == "10")
### Above code shows that there are 3 cases of 3000 level courses in 2010 that too
## in Fall and 2 cases of 4000 level courses and in Summer term. Interestingly, the 
## enteryear of students who took that class is 2009. i.e., the second years.

n <- systems_courses %>% 
  filter(floored_catalog %in% c("3000","4000")
         & Year == "12" & Semester %in% c("Fall","Summer"))
summary(n)
## The above code syas that there were 237 cases of 3000 level courses in 2011 only in fall and 
## 7 cases of 4000 level courses in summer. The entering year of students who took the class
## is 2009 again. i.e., students in their 3rd year.
### This raises a question if the data has information about courses taken by students 
## entering in 2009 and after??

## Looking for evidence to prove that question.

summary(factor(systems_courses$enteringterm))
## This answers the question. Yes, we have information on courses taken by students
## entering in year 2009 and after. The problem with this is that information on
## courses also starts from 2009 which is good fo looking at the APMA courses since
## every student mostly finishes his/her APMA requirements in first year. But,
## it is improper to look at department courses for 2009 because the data contains
## students entering in and after 2009 and students don't take departmental courses 
## until their 2nd year or 3rd year(in case of 3000 level and 4000 level courses).

## Also, another interesting thing to look here is that the information of courses 
## taken by students entering after 2014 or 2013 is incomplete because they would still be
## their 2nd or 3rd year or even 1st year. But, this wouldn't affect the course and 
## class information since students who entered early are still taking them.
## But this would be a hurdle if comparing popularity of courses among students
## grouped by the year they entered in and the year they are in.

## Lokking at the same for the larger data set

summary(factor(systems1$enteringterm))
summary(factor(systems1$academic_year))
#################################################################################

## Looking if the total number of students in the data is equal to the number that is
## given in the data

student_comparision_sys_bysection <- systems_courses %>% 
  group_by(SUBJECT,CATALOG_NUMBER,Year, Semester, CLASS_SECTION) %>% 
  summarise(data_count = n(),
            mentioned_count = mean(ENROLLMENT_TOTAL),
            enrolled_by_mentioned = data_count/mentioned_count) %>% 
  mutate(enrollment_count_isequal = ifelse(data_count == mentioned_count,1,0))

student_comparision_sys <- student_comparision_sys_bysection %>% 
  filter(Semester %in% c("Fall","Spring")) %>% 
  group_by(SUBJECT,CATALOG_NUMBER,Year, Semester) %>% 
  summarise(Data_Count = sum(data_count),
            Mentioned_Count = sum(mentioned_count),
            enrolled_by_mentioned = Data_Count/Mentioned_Count)
## Most of the classes don't have the same counts as mentioned in the data

#################################################################################

## Making line plots showing grades

levels(systems_courses$Grade_Collapsed) <- c("Dropped", "A","A","A","AU","B","B","B","C","C","C",
                                            "CR","D","D","D","F","IN","NC","W")

grades <- systems_courses %>% 
  mutate(Withdrew = if_else(Grade_Collapsed == "W",1,0),
         Dropped = if_else(Grade_Collapsed == "Dropped",1,0)) %>% 
  filter(Grade_Collapsed %in% c("A","B","C","D","F","W") &
           academic_year %in% c("12","13","14","15","16"))

ggplot(grades, aes(x= academic_year,fill = Grade_Collapsed))+
  geom_bar( position = "fill")+
  facet_grid(Semester ~ floored_catalog)

##################################################################################

## Looking at characteristics of unique students from SYS department

c <- systems %>% 
  group_by(studentid) %>% 
  summarise(majors = n_distinct(majdesc1),
            terms = n_distinct(term_sequence))

summary(factor(c$majors))
## 1- 43, 2-3802, 3-795, 4-239, 5-61, 6-10, 7-1

summary(factor(c$terms))


n_distinct(systems$studentid) ## 4951 students as whole

#################################################################################
