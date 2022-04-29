# Create functions that plot pulse data

library(dplyr)
library(ggplot2)
library(shiny)

# Load data and universal manipulation
load("d_all.Rdata") 
d_all <- d_all %>%
    mutate(time.days = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                 Time.relative.to.pulse.unit == "hr" ~ Time.relative.to.pulse/24,
                                 Time.relative.to.pulse.unit == "min" ~ Time.relative.to.pulse/(24*60))) %>% # input$vars
    group_by(Study.ID, Pulse.ID, varType, .drop = FALSE) %>%
    mutate(rescale_Mean = scale(Mean))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$scale_plot <- renderPlot({

        # Filter d_all by selected variable types
        temp_d <- d_all %>%
            filter(varType %in% "WUE") #input$vars

        # Plot time-series for each pulse
        ggplot(temp_d, aes(x = time.days, 
                           y = rescale_Mean, 
                           color = varType)) +
            geom_vline(xintercept = 0, color = "black") +
            geom_hline(yintercept = 0, color = "black") +
            geom_point(alpha = 0.25) +
            facet_wrap(~varType,
                       scales = "free") +
            scale_x_continuous("Days since pulse") +
            scale_y_continuous("Scaled mean") +
            theme_bw(base_size = 16) +
            theme(panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(size = 14)) +
            guides(color = "none")

    })
    
    output$click_info <- renderUI({
        click <- input$plot_click
        point <- nearPoints(d_all, 
                            click,
                            threshold = 5, maxpoints = 1, addDist = TRUE)
        
        if (nrow(point) == 0) return(NULL)
        
        left_px <- click$coords_css$x
        top_px <- click$coords_css$y
        
        # create style property for tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 1, 
                        "px; top:", top_px + 1, "px;")
        
        # Write tooltip
        tooltip <- paste0("<b> Study</b>: ", point$Citation.name,
                          "<br> <b>Pulse</b>: ", point$Pulse.ID,
                          "<br> <b>Time</b>: ", point$Time.relative.to.pulse, " ", point$Time.relative.to.pulse.unit,
                          "<br> <b> Variable</b>: ", point$varType,
                          "<br> <b>Value</b>: ", round(point$Mean, 3), " ", point$Units)
        wellPanel(
            style = style,
            p(HTML(tooltip))
        )
    })

})
