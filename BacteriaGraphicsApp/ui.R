library(shiny)
library(shinydashboard)
library(data.table)
library(magrittr)
library(d3heatmap)
library(ggvis)
library(visNetwork)
library(igraph)
library(XLConnect)
options(shiny.maxRequestSize = 10*1024^2)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      # menuItem("Summary", tabName = "summary", icon = icon("th")),
      menuItem("Plot", tabName = "plot", icon = icon("th")),
      menuItem("Network Exploratory Analysis", tabName = "networkeda", icon = icon("th")),
      menuItem("Heatmap Exploratory Analysis", tabName = "heatmapeda", icon = icon("th")),
      fileInput("dataset", "Upload Data", accept = ".xlsx"),
      selectInput("family", "Select Family", choices = ""),
      sliderInput("cutoff", "Select Minimum Count", min = 1, max = 1000, value = 10)
      
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Select Data", status = "primary", solidHeader = TRUE, width = 8
                )
              )
      ),
      
      
#       # Summary tab
#       tabItem(tabName = "summary",
#               fluidRow(
#                 column(width = 4,
#                        box(width = 12,
#                            selectizeInput("summaryvar", "Select Summary Variables", 
#                                           multiple = TRUE, choices = "")
#                            )
#                        ),
#                 
#                 column(width = 8,
#                        dataTableOutput("table")
#                        )
#               )
#       ),
      
      # Exploratory Analysis Network tab
      tabItem(tabName = "plot",
              fluidRow(
                column(width = 4,
                       box(width = 12,
                           actionButton("createplot", "Update Plot"),
                           selectInput("plotType", "Select Plot Type",  
                                       choices = c("Histogram", "Boxplot", "Scatter")),
                           selectInput("plotxvar", "Choose x variable", choices = ""),
                           selectInput("plotyvar","Choose Y Variable", choices = ""),
                           selectizeInput("plotfilter", "Choose filter variable", multiple = TRUE, choices = ""),
                           uiOutput("filterlevel"),
                           selectizeInput("plotgroup", "Choose Comparison Grouping", multiple = TRUE, choices = ""),
                           textInput("titletext", "Input Plot Title:")
                         
                       )
                      ),
                column(width = 8,
                       ggvisOutput("plot"),
                       conditionalPanel(
                         condition = "input.plotType == 'Boxplot'",
                         sliderInput("coef", "Select Whisker Length", min = 0, max = 3, value = 1.5, step=.1)
                       ),
                       
                       conditionalPanel(
                         condition = "input.plotType == 'Scatter'",
                         selectizeInput("size", "Choose Point Size Variable", choices = "")
                       ),
                       
                       conditionalPanel(
                         condition = "input.plotType == 'Scatter'",
                         selectizeInput("color", "Choose Point Color Variable", choices = "")
                       )
             
                      )
              )
      ),
      
      # Exploratory Analysis Network tab
      tabItem(tabName = "networkeda",
              fluidRow(
                       actionButton("updatenetwork", "Update Network"),
                       visNetworkOutput("network")
                      )
      ),
      
      # Exploratory Analysis Heatmap tab
      tabItem(tabName = "heatmapeda",
              fluidRow(
                      actionButton("updateheatmap", "Update Heatmap"),
                      checkboxInput("rowcluster", "Cluster Row", value = TRUE),
                      checkboxInput("colcluster", "Cluster Column", value = TRUE),
                      d3heatmapOutput("heatmap")        
                      )
      )
    ) # tabItems
  ) # dashboardBody
) # dashboardPage
