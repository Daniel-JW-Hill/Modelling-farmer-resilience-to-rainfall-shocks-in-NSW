
# Shiny app
# Description here

# load required functions
library(shiny)
library(ggplot2) # for charts
library(sf) # for map
library(shinyWidgets) # for additional widgets
library(shinyjs) #for dynamic updates to buttons
library(stringr) #wrapping strings
library(readxl) # for Arellano Bond coefficients

# load helper functions
source(file.path("utilities", "createRainfallNumeric.R"))
source(file.path("utilities", "loadData.R"))
source(file.path("utilities", "getRainfallIndices.R"))
source(file.path("utilities", "runSimulation.R"))
source(file.path("utilities", "getSummaryCentral.R"))
source(file.path("utilities", "runSimulationCI.R"))
source(file.path("utilities", "plotRainfall.R"))
source(file.path("utilities", "plotInputs.R"))
source(file.path("utilities", "plotOutputs.R"))
source(file.path("utilities", "getTable.R"))
source(file.path("utilities", "getFullResults.R"))

#read in and manipulate map boundaries
load("SA4_boundaries.Rdata")

# Pre-define levels (models are demeaned and these are added back in)
# levels derived from summary statistics. Median of medians for all years for each region. 
regions = c(
  "Central_West",
  "Far_West",
  "Murray_Riverina",
  "Northern_Tablelands"
)

levels_grossReturns = data.frame(regions = regions,
                                 levels = c(247.32, 
                                            115.69, 
                                            321.62, 
                                            194.47))

levels_exp = data.frame(regions = regions,
                        levels = c(273.99, 
                                   109.29, 
                                   331.07, 
                                   202.79))

levels_stock = data.frame(regions = regions,
                          levels = c(33.92,
                                     9.86,
                                     39.24,
                                     23.16))

levels_assets = data.frame(regions = regions,
                          levels = c(78.65,
                                     28.71,
                                     109.20,
                                     80.09))

levels_termsoftrade = data.frame(regions = regions,
                           levels = c(100,
                                      100,
                                      100,
                                      100)) # set to base year values 

# Rainfall index levels we extract from the simulated data at SPI values = 0 for all years. 

# min and max confidence intervals.
min_b = 0.05
max_b = 0.95

# Time period for impulse. 
yrs = 20 # min of 10 years. 

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

  # styles for check box buttons
  tags$style(
    HTML(
      "
    .btn-group > .btn.active {
      background-color: SteelBlue;
      color: white;
    }

    .btn-group > .btn {
      background-color: grey;
      color: white;
    }
  "
    )
  ),
  
  #Switch styles
  tags$head(tags$style(
    HTML(
      '.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
          background: SteelBlue;
          color: white;
        }

        .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-info,
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-info {
          background: grey;
          color: white;
        }'
    )
  )),
  
  
  # Drop down button styles
  tags$head(tags$style(
    HTML(
      '.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
          background: SteelBlue;
          color: white;
        }

        .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-info,
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-info {
          background: grey;
          color: white;
        }

      .dropdown-toggle {
        background: SteelBlue !important;
        color: white !important;
        border: none;
      }'
    )
  )),
  
  # download button class
  tags$head(tags$style(
    HTML(".btn-download {
                  background-color: SteelBlue !important;
                  color: white !important;
                  border: none !important;
                }
                .btn-download:hover {
                  background-color: #4682B4 !important; 
                  color: white !important;
                }")
  )),
  
  

  # Title
  titlePanel(
    tags$h3(
      "Modelling farmer resilience to rainfall shocks in NSW."
    )
  ),
  
  # About application
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "About this application",
        align = "center",
        br(),
        p("This application simulates farn outcomes (per ha) for representative farms in 4 regions across NSW."),
        p("The simulations are based on parameters derived from econometric analysis of ABS BLADE data."),
        p("This app was written under the ARC linkage project 'Innovations in Agricultural Greenhouse Gas Management and Policy'. More details to come!"),
        p("Written by Daniel Hill (UNE), Oscar Cacho (UNE), Jeff Connor (UniSA) and Daniel Gregg (Flinders University)"),
        
        #Add select region box here
        fluidRow(column(
          12,
          selectInput(
            "Region_select",
            label = p("Region for analysis"),
            choices = list(
              "Central West" = 'Central West',
              "Far West" = 'Far West',
              "Murray-Riverina" = 'Murray and Riverina',
              "Northern Tablelands" = 'Northern Tablelands'
            ),
            selected = 'Central_West'
          )
        ), # Add map of NSW and analysis region to tab.
        column(
          12, plotOutput("map", width = "100%", height = "400px")
        ))
      )  ,
      
      tabPanel(
        "Select rainfall scenario",
        align = "left",
        br(),
        p("This tab allows the user to change the realisations of rainfall to test how rainfall outcomes impact farm revenues and input responses.  
          The rainfall index is represented as an annual Standardised Precipitation Index (SPI). 
          The SPI index is measured as the standard deviation from mean annual rainfall.
          The model can accept any SPI value between -0.75 and 1.00."
        ),
        br(),
        
        fluidRow(column(12,
            createRainfallNumeric("spi_t4", "Four year lag rainfall index (SPI t-4)"),
            createRainfallNumeric("spi_t3", "Three year lag rainfall index (SPI t-3)"),
            createRainfallNumeric("spi_t2", "Two year lag rainfall index (SPI t-2)"),
            createRainfallNumeric("spi_t1", "One year lag rainfall index (SPI t-1)"),
            createRainfallNumeric("spi_current", "Current year rainfall index (SPI T)"),
            createRainfallNumeric("spi_lead", "Expectations of next year's rainfall index (SPI t+1)"))),
        
        br(),
        
        fluidRow(column(12,
            actionButton("runSimulation", "Simulate results", icon = icon("rocket")))),
       
        br(), 
        br()
      ),
      
      #Weather chart
      tabPanel(
        "Rainfall index summary",
        align = "Left",
        p("This tab presents the chosen rainfall index scenario"
        ),
        dropdownButton(
          downloadButton("download_rainfall", "Download rainfall chart", icon = icon('floppy-disk'), class = "btn btn-download"),
          br(),
          circle = TRUE,
          status = "danger",
          icon = icon("floppy-disk"),
          width = "300px",
          tooltip = tooltipOptions(title = "Save chart")
        ),
        plotOutput("rainfallPlot", width = "100%", height = "500px")
        
      ),
      
      #Input results tab
      tabPanel(
        "Input results",
        align = "Left",
        p("This tab presents the simulation results for expenditure and stock value in response to the modelled realisations in the rainfall index.
          The plot presents the expenditure and stock values per hectare relative to 'normal/equilibrium' conditons, which is indexed at 100. Normal conditions occur when rainfal is equal to the long-term mean for all years.
          For example, a value of 120 for expenditure indicates expenditure per ha for a given year is 20% higher relative to what it would be under normal conditions"
          ),
        dropdownButton(
          tags$h3("Chart settings"),
          checkboxGroupButtons(
            inputId = "input_display",
            label = "Show results",
            choices = c(
              "Changes in expenditure ($AUD/ha)",
              "Changes in stock value ($AUD/ha)"
            ),
            selected = c(
              "Changes in expenditure ($AUD/ha)",
              "Changes in stock value ($AUD/ha)"
            ),
            direction = "vertical",
            width = '100%',
          ),
          switchInput(
            inputId = "show_confidence_intervals_input",
            label = "Confidence intervals",
            onLabel = "Show Intervals",
            offLabel = "No intervals",
            onStatus = "danger",
            offStatus = "info",
            value = TRUE,
          ),
          textInput("ymin_Input", "Y-axis minimum", width = "100%"),
          textInput("ymax_Input", "Y-axis maximum", width = "100%"),
          downloadButton("download_inputs", "Download input response chart", icon = icon('floppy-disk'), class = "btn btn-download"),
          br(),
          circle = TRUE,
          status = "danger",
          icon = icon("gear"),
          width = "300px",
          tooltip = tooltipOptions(title = "Change chart settings")
        ),
        plotOutput("resultsInput", width = "100%", height = "500px")
        
      ),
      
      # Output results panel
      tabPanel(
        "Gross Return results",
        align = "Left",
        p("This tab presents the simulation results for gross margins per ha (cash revenue + changes in stock value) in response to the modelled realisations in the rainfall index.
          The plot presents gross margin changes relative to 'normal' conditons (indexed at 100). Normal conditions are defined as ten years of average annual rainfall.
          Changes in gross margins are decomposed into three effects, which together result in a net change in gross margins relative to normal conditions:"
        ),
        p("Change directly attributable to rainfall - represents how total gross margins per ha are expected to change directly due to the realisation of rainfall."),
        p("Change mediated by expenditure changes - the expected % change in gross margins per ha derived from expenditure decisions."),
        p("Change mediated by stock value changes - the expected % change in gross margins per ha derived from stocking decisions."),
        
        dropdownButton(
          tags$h3("Chart settings"),
          checkboxGroupButtons(
            inputId = "output_display",
            label = "Show change in gross Returns",
            choices = c(
              "Change directly attributable to rainfall",
              "Change mediated by expenditure changes",
              "Change mediated by stock value changes",
              "Net Change"
            ),
            selected = c(
              "Change directly attributable to rainfall",
              "Change mediated by expenditure changes",
              "Change mediated by stock value changes",
              "Net Change"
            ),
            direction = "vertical"
          ),
          switchInput(
            inputId = "show_confidence_intervals_output",
            label = "Confidence intervals",
            onLabel = "Show",
            offLabel = "None",
            onStatus = "danger",
            offStatus = "info",
            value = TRUE,
            width = '100%'
          ),
          textInput("ymin_Output", "Y-axis minimum", width = "100%"),
          textInput("ymax_Output", "Y-axis maximum", width = "100%"),
          downloadButton("download_outputs", "Download output response chart", icon = icon('floppy-disk'), class = "btn btn-download"),
          br(),
          circle = TRUE,
          status = "danger",
          icon = icon("gear"),
          width = "300px",
          tooltip = tooltipOptions(title = "Change chart settings")
        ),
        
        plotOutput("resultsOutput", width = "100%", height = "500px")
      ),
      
      tabPanel(
        "Summary of outcomes",
        align = "Left",
        p("This tab summarises the simulation results. Use the dropdown menu to save the summary data and/or the detailed simulation results. Net present value results are dependent on the chosen discount rate in this dropdown menu"),
        dropdownButton(
          tags$h4("Table settings"),
          numericInput("Discount_rate", "Annual discount rate", value = 0.1, width = "100%"),
          downloadButton("download_summary", "Download summary data", icon = icon('floppy-disk'), class = "btn btn-download"),
          br(),
          downloadButton("download_full", "Download full simulation results", icon = icon('floppy-disk'), class = "btn btn-download"),
          br(),
          circle = TRUE,
          status = "danger",
          icon = icon("gear"),
          width = "300px",
          tooltip = tooltipOptions(title = "Save data and change discount rate")
        ),
        tableOutput("summary_outputs")
      )
    )
  )
)


# Define server logic and objects for output
server <- function(input, output, session) {
  
  # retrieve boundaries for plot based on analysis region
  boundaries = reactive({
    boundary_row = which(boundaries_no_coastal$labels == input$Region_select)
    boundaries_filtered = boundaries_no_coastal[c(1, boundary_row), ]
    return(boundaries_filtered)
  })
  
  # plot map
  output$map = renderPlot({
    fill_colors = c("NSW" = "white")
    fill_colors[input$Region_select] = "#4682B4"
    
    ggplot(boundaries()) +
      geom_sf(aes(fill = labels)) +
      theme_void() +
      scale_fill_manual(values = fill_colors) +
      theme(legend.position = "none")
  })
  
  # create reactive value to store outputs from simulation
  plot_data = reactiveValues(plot_data = NULL)
  
  # Run simulation when button is pressed.
  observeEvent(input$runSimulation, {
    disable('runSimulation')
    updateActionButton(
      session = session,
      inputId = 'runSimulation',
      label = "Simulation in progress",
      icon = icon("sync", class = "fa-spin")
    )
    
    shinyjs::delay(100, {
      
      # Retrieve data for relevant region. 
      data_files = loadData(input$Region_select)
      gams_path = data_files[[1]]
      exp_coefs = data_files[[2]]
      stock_coefs = data_files[[3]]
      revenue_coefs = data_files[[4]]
      rm(data_files)
      
      
      # Retrieve weather indices for simulation
      rainfall_indices = getRainfallIndices(yrs, 
                                            gams_path, 
                                            input$spi_t4, 
                                            input$spi_t3,
                                            input$spi_t2,
                                            input$spi_t1,
                                            input$spi_current,
                                            input$spi_lead)
      
      rainfall_index_baseline = rainfall_indices[[1]] #normal conditions, in SPI,  in a vector
      rainfall_index_scenario = rainfall_indices[[2]] #chosen SPI values in a vector
      index_baseline = rainfall_indices[[3]] # index as derived from GAMs model under normal conditions
      index_scenario = rainfall_indices[[4]] # index as derived from GAMs model for chosen rainfall outcomes
      rm(rainfall_indices)
      
      # Estimate Central Outcomes
      central_simulation  = runSimulation(yrs,
                                          index_baseline,
                                          index_scenario,
                                          exp_coefs,
                                          stock_coefs, 
                                          revenue_coefs)
      
      # save central results and get baseline results. 
      summary_central = getSummaryCentral(yrs, 
                                          central_simulation,
                                          rainfall_index_scenario, 
                                          rainfall_index_baseline,
                                          input$Region_select, 
                                          levels_grossReturns, 
                                          levels_exp, 
                                          levels_stock) 
      

      # Estimate Confidence intervals
      summary_CI = runSimulationCI(yrs,
                                   exp_coefs, 
                                   stock_coefs,
                                   revenue_coefs,
                                   index_baseline,
                                   index_scenario,
                                   input$Region_select, 
                                   levels_grossReturns, 
                                   levels_exp, 
                                   levels_stock, 
                                   min_b,
                                   max_b)
      
      
      summary_lower = summary_CI$summary_lower
      summary_upper = summary_CI$summary_upper
      
      #Save dataframes for ggplot.
      data_inputs = data.frame(
        Year = 1:nrow(summary_central),
        Type = rep(c("ExpOutcomes", "StockOutcomes"), each = yrs),
        Central = c(summary_central$exp_outcomes_scenario_percent, summary_central$stock_outcomes_scenario_percent),
        Lower = c(summary_lower$exp_outcomes_scenario_percent, summary_lower$stock_outcomes_scenario_percent),
        Upper = c(summary_upper$exp_outcomes_scenario_percent, summary_upper$stock_outcomes_scenario_percent))
      
      data_outputs = data.frame(
        Year = 1:nrow(summary_central),
        Type = rep(c("Direct", "Exp", "Stock", 'Total'), each = yrs),
        Central = c(summary_central$revenue_outcomes_direct_scenario_percent, 
                    summary_central$revenue_outcomes_exp_scenario_percent, 
                    summary_central$revenue_outcomes_stock_scenario_percent, 
                    summary_central$revenue_outcomes_scenario_percent),
        Lower = c(summary_lower$revenue_outcomes_direct_scenario_percent, 
                  summary_lower$revenue_outcomes_exp_scenario_percent, 
                  summary_lower$revenue_outcomes_stock_scenario_percent, 
                  summary_lower$revenue_outcomes_scenario_percent),
        Upper = c(summary_upper$revenue_outcomes_direct_scenario_percent, 
                  summary_upper$revenue_outcomes_exp_scenario_percent, 
                  summary_upper$revenue_outcomes_stock_scenario_percent, 
                  summary_upper$revenue_outcomes_scenario_percent))
      
      plot_data$plot_data = list(data_inputs = data_inputs, 
                                 data_outputs = data_outputs,
                                 data_summary = summary_central,
                                 data_summary_lower = summary_lower,
                                 data_summary_upper = summary_upper)
      
      
    })
    
    # change back button
    delay(5000, {
      enable('runSimulation')
      updateActionButton(
        session = session,
        inputId = 'runSimulation',
        label = "Simulate results",
        icon = icon("rocket")
      )
    })
    
  })
  
  
  # Plot weather chart
  output$rainfallPlot = renderPlot({
    req(plot_data$plot_data)
    plot = plotRainfall(plot_data$plot_data$data_summary, yrs)
    plot
  })
  
  # Save weather output
  output$download_rainfall = downloadHandler(
    filename = function() { "rainfall_chart.tiff" },
    content = function(file) {
      req(plot_data$plot_data)
      plot = plotRainfall(plot_data$plot_data$data_summary, yrs)
      ggsave(file, plot = plot, width = 21, height = 20, units = 'cm', dpi = 300)
    }
  )
  
  # plot inputs chart
  output$resultsInput = renderPlot({
    req(plot_data$plot_data)
    plot = plotInputs(yrs,
                      plot_data$plot_data$data_inputs,
                      input$input_display, 
                      input$ymin_Input, 
                      input$ymax_Input,
                      input$show_confidence_intervals_input)
    plot
  })
  
  # Save inputs chart
  output$download_inputs = downloadHandler(
    filename = function() { "input_chart.tiff" },
    content = function(file) {
      req(plot_data$plot_data)
      plot = plotInputs(yrs,
                        plot_data$plot_data$data_inputs,
                        input$input_display, 
                        input$ymin_Input, 
                        input$ymax_Input,
                        input$show_confidence_intervals_input)
      ggsave(file, plot = plot, width = 21, height = 20, units = 'cm', dpi = 300)
    }
  )
  

  # Plot outputs
  output$resultsOutput = renderPlot({
    req(plot_data$plot_data)
    plot = plotOutputs(yrs,
                       plot_data$plot_data$data_outputs,
                       input$output_display,
                       input$ymin_Output,
                       input$ymax_Output, 
                       input$show_confidence_intervals_output)
    plot
  })
  

  # Save outputs chart
  output$download_outputs = downloadHandler(
    filename = function() { "output_chart.tiff" },
    content = function(file) {
      req(plot_data$plot_data)
      plot = plotOutputs(yrs,
                         plot_data$plot_data$data_outputs,
                         input$output_display,
                         input$ymin_Output,
                         input$ymax_Output, 
                         input$show_confidence_intervals_output)
      ggsave(file, plot = plot, width = 21, height = 20, units = 'cm', dpi = 300)
    }
  )
  
  #Print summary table based on discount rate
  output$summary_outputs = renderTable({
    req(plot_data$plot_data)
    summary_table = getTable(plot_data$plot_data, 
                             input$Discount_rate)
    summary_table
    
  }, rownames = FALSE)
  
  
  
  # Download summary data
  output$download_summary <- downloadHandler(
    filename = function() { "simulation_summary_data.csv" },
    content = function(file) {
      req(plot_data$plot_data)
      summary_table = getTable(plot_data$plot_data, 
                                               input$Discount_rate)
      write.csv(summary_table, file, row.names = TRUE)
    }
  )
  

  # Save full data
  output$download_full <- downloadHandler(
    filename = function() { "simulation_results_all.csv" },
    content = function(file) {
      req(plot_data$plot_data)
      data_full = getFullResults(yrs, plot_data$plot_data)
      write.csv(data_full, file, row.names = TRUE)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

