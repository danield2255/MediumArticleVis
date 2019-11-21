library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(ggplot2)
library(grid)
library(scales)


data = as.data.table(fread("MediumProcessed.csv") %>% select(-c(V1)))
timeData = as.data.table(fread("timeData.csv"))

body = dashboardBody(
            fluidRow(
                tabBox(
                    title = "Clap Data Visuals", 
                    id = "vis1", 
                    width = 15,
                    tabPanel("Clap Distribution", plotOutput("distPlotClaps")),#dataTableOutput("texted")),
                    tabPanel("Claps VS Reading TIme", plotOutput("ClapsToRT")), 
                    tabPanel("Claps Over Time", h6("Note: Different from other tabs, here claps are included in monthly average if any tag is present, not all"),plotOutput("ClapsOverTime"))
                )
            ),
            fluidRow(
                uiOutput("summary")
            )
        )

# Define UI for application
ui = dashboardPage(
    dashboardHeader(title = "Medium Article Data Exploration"),
    
    # Sidebar
    dashboardSidebar(
        collapsed = FALSE,
        checkboxGroupInput("tags", 
                           "Tags of Article:", 
                           c("Art" = "Tag_art", 
                             "Artificial Intelligence" = "Tag_artificial_intelligence", 
                             "Bitcoin" = "Tag_bitcoin", 
                             "Blockchain" = "Tag_blockchain", 
                             "Business" = "Tag_business", 
                             "Computer Science" = "Tag_computer_science",
                             "Creativity" = "Tag_creativity",
                             "Cryptocurrency" = "Tag_cryptocurrency",
                             "Culture" = "Tag_culture", 
                             "Design" = "Tag_design", 
                             "Education" = "Tag_education", 
                             "Entrepreneurship" = "Tag_entrepreneurship", 
                             "Ethereum" = "Tag_ethereum", 
                             "Fiction" = "Tag_fiction",
                             "Food" = "Tag_food", 
                             "Health" = "Tag_health", 
                             "Humor" = "Tag_humor", 
                             "Inspiration" = "Tag_inspiration", 
                             "Investing" = "Tag_investing", 
                             "Javascript" = "Tag_javascript", 
                             "Leadership" = "Tag_leadership", 
                             "Life" = "Tag_life", 
                             "Life Lessons" = "Tag_life_lessons",
                             "Love" = "Tag_love",
                             "Machine Learning" = "Tag_machine_learning", 
                             "Motivation" = "Tag_motivation", 
                             "Movies" = "Tag_movies", 
                             "Music" = "Tag_music", 
                             "News" = "Tag_news", 
                             "Personal Development" = "Tag_personal_development", 
                             "Photography" = "Tag_photography", 
                             "Poetry" = "Tag_poetry", 
                             "Politics" = "Tag_politics", 
                             "Productivity" = "Tag_productivity", 
                             "Programming" = "Tag_programming", 
                             "Relationships" = "Tag_relationships", 
                             "Self Improvement" = "Tag_self_improvement", 
                             "Social Media" = "Tag_social_media", 
                             "Sports" = "Tag_sports", 
                             "Startup" = "Tag_startup",
                             "Tech" = "Tag_tech", 
                             "Technology" = "Tag_technology",
                             "Travel" = "Tag_travel", 
                             "UX" = "Tag_ux", 
                             "Web Development" = "Tag_web_development", 
                             "Writing" = "Tag_writing"))),
        body

)

# Define server
server = function(input, output) {
    options(shiny.error = browser)
    options(rsconnect.max.bundle.size=2147483648)
    
    dataSub = reactive({
        newData = data
        if(is.null(input$tags)){
            newData = data
        }else{
            newData=filter_at(newData, vars(input$tags), all_vars(.==1))
        }
    })
    
    correlationC = reactive({
        round(cor(dataSub()$Claps, dataSub()$Reading_Time , method = "pearson", use = "na.or.complete" ) ^ 2,2)
    })
    
    xLimVal = reactive({
        max(50, quantile(dataSub()$Claps, 0.95))
    })
    
    distData = reactive({
        binData = dataSub() %>%
            select(Claps) %>%
            mutate(bin = Claps %/% 20) %>%
            group_by(bin) %>%
            summarise(binCount = n())
            
    })
    
    yLimVal = reactive({
        max(distData()$binCount, na.rm = TRUE)
    })
    
    timeDataSub = reactive({
        newTimeData = timeData
        if (is.null(input$tags)){
            newTimeData = timeData
        }else{
            newTimeData = newTimeData %>% 
                filter(tag %in% input$tags)
        }
    })

    output$summary = renderUI({ 
        if (nrow(dataSub()) ==0){
            infoBox("There exist no articles with all of the selectd tags", width = 10)
        }else{
            val = summary(dataSub())
            
            infoBox("Summary Stats of Claps for Articles Only with Selected Tags", 
                    value = HTML(paste(gsub(pattern ="\\.:", replacement = "artile :",val[,5][1:6]), br())), 
                    width = 10)
        }
    })

        
    output$distPlotClaps = renderPlot({
        plotdata = as.data.frame(dataSub())
        if (nrow(plotdata) == 0){
            ggplot(data.frame()) + geom_point() + annotation_custom(textGrob("There are no articles in the data with all of these tags\n Unselect tags to keep exploring!"), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
        }else{
            ggplot(distData(), aes(x =bin*20, y = binCount)) + 
                geom_col() +
                stat_function(fun = dnorm, args = list(mean = mean(dataSub()$Claps), sd = sd(dataSub()$Claps)))+
                scale_x_continuous(name = "Number of Claps",
                                   breaks = seq(0, 1500, 50), 
                                   limits = c(-10, xLimVal()))+
                scale_y_continuous(name = "Count",
                                   limits = c(0, yLimVal())) + 
                ggtitle("Distribution of Claps Given to Articles with Only Selected Tags")+
                theme(plot.title = element_text(hjust = 0.5))
        }
    })
    
    output$ClapsToRT = renderPlot({
        plotdata = as.data.frame(dataSub())
        if (nrow(plotdata) == 0){
            ggplot(data.frame()) + geom_point() + annotation_custom(textGrob("There are no articles in the data with all of these tags\n Unselect tags to keep exploring!"), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
        }else{
            ggplot(plotdata, aes(x = Reading_Time, y = Claps)) + 
                geom_point() +
                scale_x_continuous(name = "Estimated Reading Time (Minutes)") + 
                scale_y_continuous(name = "Claps") + 
                annotate(x= Inf, y=Inf, hjust= 1, vjust = 1,
                        label=paste("Pearson Correlation Coefficient= ", correlationC()), 
                        geom="text", size=5) +
                ggtitle("Claps Versus Estimated Reading Time of Articles with Only Selected Tags")+
                theme(plot.title = element_text(hjust = 0.5))
        }
    })
    
    output$ClapsOverTime = renderPlot({
        if (is.null(input$tags)){
            pooledMonthMeans = timeDataSub()%>%
                group_by(Date) %>%
                summarize(meanClaps = mean(Claps))
            ggplot(pooledMonthMeans, aes(x = as.Date(Date), y = meanClaps)) + 
                geom_point() + 
                scale_x_date(date_breaks = "1 month", 
                             labels = date_format("%m-%Y"),
                             name = "Date")+
                scale_y_continuous(name = "Average Number Claps")+
                stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + 
                ggtitle("Pooled Averages for Each Month of Average Claps for All Selectable Tags")+
                geom_text(x= Inf, y=Inf, hjust= 1,
                         vjust = 1,
                         label="Pick a tag from the sidebar to start seeing individual means!"
                         , size=5) +
                theme(plot.title = element_text(hjust = 0.5))
        }else{
            ggplot(data = timeDataSub(), aes(x = as.Date(Date), y = Claps, col = tag)) + 
                geom_point() +
                scale_x_date(date_breaks = "1 month", 
                             labels = date_format("%m-%Y"),
                             name = "Date")+
                scale_y_continuous(name = "Average Number Claps")+
                geom_smooth(method = "lm") + 
                ggtitle("Average Number of Claps Each Month for each Selected Tag")+
                theme(plot.title = element_text(hjust = 0.5))+
                guides(color=guide_legend("Tag"))
        }
     })
}

# Run the application
shinyApp(ui = ui, server = server)
