# App for visualizing extracted pulse data by variable type

library(shiny)

variables <- c("WUE", "ET", "T", "Gs", "PWP",
               "ecosystemR", "abovegroundR", "belowgroundR",
               "NPP", "GPP", "Anet")

# Define UI for application that draws a histogram
shinyUI(navbarPage("Pulse dynamics meta-analysis", 
    tabPanel("Scaled pulses",
             # Application title
             titlePanel("Visualize scaled pulses"),
             # Sidebar 
             sidebarLayout(
                 sidebarPanel(
                     # Checkbox to select variable types
                     checkboxGroupInput(inputId = "vars",
                                        label = "Select variable type:",
                                        choices = variables,
                                        selected = "WUE")
                     ),
                 # Show a plot of the generated distribution
                 mainPanel(
                     h4('Responses are scaled by study, pulse, and variable. Click for specific information.'),
                     fluidRow(plotOutput("scale_plot",
                                         width = "100%",
                                         height = "500px",
                                         click = clickOpts("plot_click"))),
                     uiOutput("click_info")
                     )
                 )
    )
))
