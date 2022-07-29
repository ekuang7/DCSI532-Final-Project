#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(readr)
library(plotly)
bank<-read_csv("BankChurners.csv")
data<-bank
library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
library(dplyr)
bank$Attrition_Flag<-ifelse(bank$Attrition_Flag=="Attrited Customer",1,2)
bank<- bank %>% mutate(Card_Category = recode(Card_Category, "Blue" = 1, "Silver" = 2,"Gold" = 3,"Platinum" = 4))
dbWriteTable(con, "bank", bank)

dbSendStatement(con, "CREATE TABLE Tier
           (
              Card_ID INTEGER PRIMARY KEY, 
              Card_Category VARCHAR(25))")
dbSendStatement(con, "Insert into Tier (Card_ID, Card_Category) values (1, 'Blue'), (2,'Silver'), (3, 'Gold'), (4, 'Platinum')")
dbSendStatement(con, "CREATE TABLE Status(Status_ID PRIMARY KEY, Attrition_Flag VARCHAR(25) )")
dbSendStatement(con, "INSERT INTO Status(Status_ID, Attrition_Flag) values (1,'Attrited Customer'), (2,'Existing Customer')")

b64 <- base64enc::dataURI(file="pic2.jpeg", mime="image/jpeg")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
                navbarPage(
                    "Credit Card Customer Information",
                    tabPanel("Welcome",
                             
                             mainPanel(
                                 img(src=b64, height=500, width=700,style="display: block; margin-left: auto; margin-right: auto;"),
                                 h1("Welcome to the banking information app!"),
                                 "Here you can visualize customers credit limit by card categories and look up customer's information such as
                                 their credit limit and if they are still with the bank or not. To
                                 protect their identity, you are only able to see their ID number."
                                 
                                 
                                 
                             ) # mainPanel
                             
                    ), 
                    tabPanel("Visualization",
                             h2("Visualization tab"),
                             "Here you can see the visualization of customer's credit limit within the bank.",
                             selectInput('var', label = h3('Select a Card Category'),
                                         choices = unique(data$Card_Category),
                                         selected="Blue"),
                             plotlyOutput("plot1")),
                    
                    tabPanel("Look up Information",
                             h2("Here, we can look up customer's information."),
                             selectInput('var2', label = h3('Select a Card Category'),
                                         choices = unique(data$Card_Category),
                                         selected="Blue"),
                             dataTableOutput('table1')))
                                       
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1<-renderPlotly({
        
        
        res2<-dbSendStatement(con, "Select b.Credit_Limit from bank b left join Tier t  on b.Card_Category=t.Card_ID where t.Card_Category= ?",  params = input$var)
        data2<-dbFetch(res2)
        dbClearResult(res2)
        data2  %>% do(p=plot_ly(.,x=~Credit_Limit, type='histogram', color="red",colors = "Set1") %>%layout(legend=list(title=list(text='Card Category')), title = "Here are the Distribution of Credit Limits within the Bank based on Categories")) %>% subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
        
        
        
        
        
        
    }) 
    
    output$table1 <- renderDataTable({
        res3<-dbSendStatement(con, "Select  b.CLIENTNUM as 'Client ID', b.Credit_Limit as 'Credit Limit', b.Total_Revolving_Bal as 'Total Balance Owed' from bank b left join Tier t  on b.Card_Category=t.Card_ID  where t.Card_Category=?", params=input$var2)
        data3<-dbFetch(res3)
        
    })
        
        
    
    
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
