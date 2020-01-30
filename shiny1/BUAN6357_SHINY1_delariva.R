#Author: Jorge de la Riva
#View app live at

#https://jdelariva.shinyapps.io/Popularbabynames/

library(shiny)
library(tidyverse)
library(xlsx)
library(dplyr)
theme_set(theme_light())

#data read and prep 
names.girl <- read.xlsx("Top100_Popular_Baby_Names.xlsx", sheetIndex = 1, startRow = 4, endRow = 107)
names.girl <- names.girl[-3,]
names.girl <- names.girl %>%
    mutate_all(as.character)
names.girl[1,2] <- '1954'
names.girl[1,3] <- NA
#boys data read and prep (girl)
names.boy <- read.xlsx("Top100_Popular_Baby_Names.xlsx", sheetIndex = 2, startRow = 4, endRow = 107)
names.boy <- names.boy[-3,]
names.boy <- names.boy %>%
    mutate_all(as.character)
names.boy[1,2] <- '1954'
names.boy[1,3] <- NA
names.boy <- names.boy[,-197]

#new dataframe for name over years
years.boy <- names.boy[c(3:102),c(3:196)]
years.seqname <- seq(1,194, by=3)
years.seqcount <- seq(2,195,by=3)
years.boycount <- years.boy[c(years.seqcount)]
years.boyname <- years.boy[c(years.seqname)]
names(years.boyname) <- c(1954:2018)
rownames(years.boyname) <- NULL
years.boycount <- years.boy[c(years.seqcount)]
names(years.boycount) <- c(1954:2018)
rownames(years.boycount) <- NULL

years.girl <- names.girl[c(3:102),c(3:196)]
years.seqname <- seq(1,194, by=3)
years.seqcount <- seq(2,195,by=3)
years.girlcount <- years.girl[c(years.seqcount)]
years.girlname <- years.girl[c(years.seqname)]
names(years.girlname) <- c(1954:2018)
rownames(years.girlname) <- NULL
years.girlcount <- years.girl[c(years.seqcount)]
names(years.girlcount) <- c(1954:2018)
rownames(years.girlcount) <- NULL
#End data preparation

# Define UI 
ui <- fluidPage(
    # Application title
    tabsetPanel(
        tabPanel("Top 10 Female",textOutput('splashtextgirl'),selectInput("yeargirl", label = "Select year from list",
                             choices = c(1954:2018)
        ),
        tableOutput("tablegirl")
        ),
        tabPanel("Top 10 Boy",textOutput('splashtextboy'),selectInput("yearboy", label = "Year",
                                             choices = c(1954:2018)
        ),
        tableOutput("tableboy")
        ),
        tabPanel("Track name popularity (Female)", selectInput('girlname', label= 'Select a name', choices = unique(unlist(years.girlname))
        ),
        plotOutput("plotgirl")
    ),
    tabPanel("Track name popularity (Male)", selectInput('boyname', label= 'Select a name', choices = unique(unlist(years.boyname))
    ),
    plotOutput("plotboy")    
    )
    ))


# Define server logic
server <- function(input, output) {
    output$splashtextgirl <- renderText('Top 10 New Zealand baby names by user selected year (1954-2018):')
    output$splashtextboy <- renderText('Top 10 New Zealand baby names by user selected year (1954-2018):')
    output$tablegirl <- renderTable({
        yearsplice <- names.girl[c(3:102),c((which(input$yeargirl == names.girl[1,])+1),(which(input$yeargirl == names.girl[1,])+2))]
        names(yearsplice) <- c('year','# of births')
        head(yearsplice,n=10)
    })
    output$tableboy <- renderTable({
        yearsplice <- names.boy[c(3:102),c((which(input$yearboy == names.boy[1,])+1),(which(input$yearboy == names.boy[1,])+2))]
        names(yearsplice) <- c('year','# of births')
        head(yearsplice, n=10)
    })
    output$plotboy <- renderPlot({
        namesearch <- names(years.boyname[grep(input$boyname, years.boyname)])
        index <- which(years.boyname == input$boyname, arr.ind = TRUE)
        searchresults <- data.frame('Year' = namesearch, BirthNum = years.boycount[index])
        ggplot(data=searchresults,aes(x=as.integer(as.character(Year)),y=as.integer(as.character(BirthNum)), group=1)) +
            geom_line(color= 'blue') + geom_point()+ggtitle(paste0('Babies named ',input$boyname,' each year'),subtitle = 'Based on babies born in New Zealand between 1954-2018') + ylab('Births') + xlab('Time (years)')
    })
    output$plotgirl <- renderPlot({
        namesearch <- names(years.girlname[grep(input$girlname, years.girlname)])
        #get row index
        index <- which(years.girlname == input$girlname, arr.ind = TRUE)
        #use row index from names on counts and combine to a dataframe with V1 = year, V2= count
        searchresults <- data.frame('Year' = namesearch, BirthNum = years.girlcount[index])
        ggplot(data=searchresults,aes(x=as.integer(as.character(Year)),y=as.integer(as.character(BirthNum)), group=1)) +
            geom_line(color= 'deeppink') + geom_point() + ggtitle(paste0('Babies named ',input$girlname,' each year'),subtitle = 'Based on babies born in New Zealand between 1954-2018') + ylab('Births') + xlab('Time (years)')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


