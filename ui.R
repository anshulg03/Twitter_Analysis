library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  dashboardPage(
    dashboardHeader(title="Twitter Sentiments"),
    dashboardSidebar(
      textInput("entity1", "Handle: ",""),
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=300,max=2500,value=600,step=100),
      actionButton(inputId='go',icon =icon("twitter"), label="Hit it!"),
      hr(),
      sidebarMenu(
        # menuItem("Sentiment Analysis", tabName = "dashboard", icon = icon("signal")),
        menuItem("Word Clouds", tabName = "wordcloud", icon = icon("cloud")),
        menuItem("Score", tabName = "score", icon = icon("dashboard"))
      )
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
      tabItems(

        tabItem(tabName = "wordcloud",
                HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >WordCloud</div>"),
                plotOutput("entity1wcplot")
        ),
        tabItem(tabName = "score",
                HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >Score</div>"),
                HTML("<br>"),
                plotlyOutput("plot3")

        )
        )
      )
    )
  ))