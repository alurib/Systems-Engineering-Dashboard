library(shiny)
library(tidyverse)
library(stringr)

systems_courses <- readRDS("systemcourses.rds")

student_comparision_sys_bysection <- systems_courses %>% 
  group_by(SUBJECT,CATALOG_NUMBER,academic_year, Semester, CLASS_SECTION) %>% 
  summarise(data_count = n(),
            mentioned_count = mean(ENROLLMENT_TOTAL),
            enrolled_by_mentioned = data_count/mentioned_count) %>% 
  mutate(enrollment_count_isequal = ifelse(data_count == mentioned_count,1,0))

student_comparision_sys <- student_comparision_sys_bysection %>% 
  filter(Semester %in% c("Fall","Spring") &
           academic_year %in% c("12","13","14","15","16")) %>% 
  group_by(academic_year, Semester,CATALOG_NUMBER) %>% 
  summarise(Enrolled = sum(data_count),
            Mentioned_Count = sum(mentioned_count),
            Ratio = Enrolled/Mentioned_Count) %>% 
  filter(Ratio != 0)

ui <- fluidPage(
  
  titlePanel("Top Classes by Year"),
  wellPanel(
    selectInput(inputId = "year", label = "Academic Year",
                choices = c("2016-2017" = "16",
                            "2015-2016" = "15",
                            "2014-2015" = "14",
                            "2013-2014" = "13",
                            "2012-2013" = "12"),
                selected = "2016-2017"),
    
    selectInput(inputId = "semester", label = "Semseter",
                choices = c("Fall" = "Fall",
                            "Spring" = "Spring")),
    
    radioButtons(inputId = "metrics", label = "Choose Metric",
                 choices = c("Total Enrolled" = "Enrolled",
                             "Ratio of Actual Enrolled by Class Size" ="Ratio"),
                 selected = "Enrolled")#,
    
    #radioButtons(inputId = "rank", label = "Ranking",
                # choices = c("Top 5" = "desc",
                #             "Bottom 5" = "asc"),
                # selected = "Top 5")
  ),
  
  plotOutput(outputId = "plot")#,
  
  #tableOutput(outputId = "table")
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    
    d <- student_comparision_sys %>%
      group_by(academic_year, Semester) %>%
      #top_n(n=5, wt = get(input$metrics)) %>% 
      ungroup() 
    d %>% 
      filter(academic_year == input$year &
               Semester == input$semester) %>%
      ggplot(aes_string(x="CATALOG_NUMBER",as.name(input$metrics),fill = "CATALOG_NUMBER"))+
      geom_bar( stat = "identity", color = "black")+
      ggtitle("Top 5 Courses by Metrics")+
      xlab("Catalog Number")
      
  })
}

shinyApp(ui, server)