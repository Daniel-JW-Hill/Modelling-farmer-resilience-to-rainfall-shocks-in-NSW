
# Shiny app
# Description here

# load required functions
library(shiny)
library(ggplot2) # for charts
library(sf) # for map
library(shinyWidgets) # for additional widgets
library(shinyjs) #for dynamic updates to buttons
library(stringr) #wrapping strings

# load helper functions
source(file.path("utilities", "createRainfallSlider.R"))
source(file.path("utilities", "loadData.R"))
source(file.path("utilities", "getRainfallIndices.R"))
source(file.path("utilities", 'initialise_frame.R'))
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
boundaries_no_coastal$labels = c(
  'NSW',
  'Far_West',
  'Murray_Riverina',
  "Central_West",
  'Southern_Tablelands',
  'Northern_Tablelands'
)

# Pre-define levels (models are demeaned and these are added back in)
# levels derived from summary statistics.
regions = c(
  "Central_West",
  "Far_West",
  "Murray_Riverina",
  "Northern_Tablelands",
  'Southern_Tablelands'
)

levels_grossReturns = data.frame(regions = regions,
                                 levels = c(714.25, 201.40, 576.77, 509.07, 489.80))

levels_exp = data.frame(regions = regions,
                        levels = c(801.95, 213.80, 622.80, 545.30, 551.02))

levels_stock = data.frame(regions = regions,
                          levels = c(157.52, 43.43, 128.16, 134.46, 129.91))

# min and max confidence intervals.
min_b = 0.05
max_b = 0.95

# load in nickel bias adjustment parameters. 
nickel_adjustment = read.csv(file.path("Regression_parameters","nickel_bias_adjustment.csv"))

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
        p("This application simulates farn outcomes (per ha) for representative farms in 5 regions across NSW."),
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
              "Central West" = 'Central_West',
              "Far West" = 'Far_West',
              "Murray-Riverina" = 'Murray_Riverina',
              "Northern Tablelands" = 'Northern_Tablelands',
              "Southern Tablelands" = "Southern_Tablelands"
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
        align = "Left",
        br(),
        p("This tab allows the user to change the realisations of the rainfall index over a 10 year period, 
        with the model simulating how farm inputs and outputs respond to these realised rainfall outcomes.
        The rainfall index is represented as an annual Standardised Precipitation Index (SPI). 
        The model allows for 15 different realisations of this SPI index for each given year. 
        The SPI index is measured as the standard deviation from mean annual rainfall. In other words, a value of -1 occurs when rainfall is 1 standard deviation lower than average annual rainfall for a given SA4 region. 
        Average rainfall is assumed for the five years prior to the simulation time frame. 
          "),
        br(),
        p("Once the desired SPI indices are chosen for each year, press the 'Simulate results' button at the bottom of this tab.
          The model should take a few seconds to complete" ),
        
        # Add slider for selection of weather index.
        # This involves selecting from a scale from very dry to very wet
        # for each year.
        fluidRow(width = 12, 
                 column(6,
                   lapply(1:5, function(i) createRainfallSlider(paste0("year", i), paste("Year", i))) ),
                   column(
                   6,
                   lapply(6:10, function(i) createRainfallSlider(paste0("year", i), paste("Year", i))))
                ),
        fluidRow(width = 12, 
                 column(12,offset = 0,
                   actionButton('runSimulation', 'Simulate results', icon = icon("rocket")))),
        br(),
        br()
        
      ) ,
      
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
          The plot presents the change in expenditure and stock value per hectare relative to 'normal' conditons (indexed at 100). Normal conditions are defined as ten years of average annual rainfall.
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
      data_files = loadData(input$Region_select, nickel_adjustment )
      opex_gams = data_files[[1]]
      revenue_gams = data_files[[2]]
      stock_gams = data_files[[3]]
      exp_coefs = data_files[[4]]
      exp_sd = data_files[[5]]
      stock_coefs = data_files[[6]]
      stock_sd = data_files[[7]]
      revenue_coefs = data_files[[8]]
      revenue_sd = data_files[[9]]
      rm(data_files)
      
      
      # Retrieve weather indices for simulation
      rainfall_indices = getRainfallIndices(opex_gams, 
                                            stock_gams, 
                                            revenue_gams, 
                                            input$year1,
                                            input$year2,
                                            input$year3,
                                            input$year4,
                                            input$year5,
                                            input$year6,
                                            input$year7,
                                            input$year8,
                                            input$year9,
                                            input$year10)
      
      weather_index_baseline = rainfall_indices[[1]]
      weather_index_scenario = rainfall_indices[[2]]
      fitted_exp_baseline = rainfall_indices[[3]]
      fitted_stock_baseline = rainfall_indices[[4]]
      fitted_revenue_baseline = rainfall_indices[[5]]
      fitted_exp_scenario = rainfall_indices[[6]]
      fitted_stock_scenario = rainfall_indices[[7]]
      fitted_revenue_scenario = rainfall_indices[[8]]
      
      
      # Estimate Central Outcomes
      central_simulation  = runSimulation(fitted_exp_baseline,
                                          fitted_stock_baseline,
                                          fitted_revenue_baseline,
                                          fitted_exp_scenario,
                                          fitted_stock_scenario,
                                          fitted_revenue_scenario,
                                          exp_coefs,
                                          stock_coefs, 
                                          revenue_coefs)
      
      # save central results and get baseline results. 
      summary_central = getSummaryCentral(central_simulation,
                                          weather_index_scenario, 
                                          weather_index_baseline,
                                          input$Region_select, 
                                          levels_grossReturns, 
                                          levels_exp, 
                                          levels_stock) 
      

      # Estimate Confidence intervals
      summary_CI = runSimulationCI(exp_coefs, 
                                   stock_coefs,
                                   revenue_coefs,
                                   exp_sd,
                                   stock_sd, 
                                   revenue_sd,
                                   fitted_exp_baseline,
                                   fitted_stock_baseline,
                                   fitted_revenue_baseline,
                                   fitted_exp_scenario,
                                   fitted_stock_scenario,
                                   fitted_revenue_scenario,
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
        Type = rep(c("ExpOutcomes", "StockOutcomes"), each = 10),
        Central = c(summary_central$exp_outcomes_scenario_percent, summary_central$stock_outcomes_scenario_percent),
        Lower = c(summary_lower$exp_outcomes_scenario_percent, summary_lower$stock_outcomes_scenario_percent),
        Upper = c(summary_upper$exp_outcomes_scenario_percent, summary_upper$stock_outcomes_scenario_percent))
      
      data_outputs = data.frame(
        Year = 1:nrow(summary_central),
        Type = rep(c("Direct", "Exp", "Stock", 'Total'), each = 10),
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
    plot = plotRainfall( plot_data$plot_data$data_summary)
    plot
  })
  
  # Save weather output
  output$download_rainfall = downloadHandler(
    filename = function() { "rainfall_chart.tiff" },
    content = function(file) {
      req(plot_data$plot_data)
      plot = plotRainfall(plot_data$plot_data$data_summary)
      ggsave(file, plot = plot, width = 21, height = 20, units = 'cm', dpi = 300)
    }
  )
  
  # plot inputs chart
  output$resultsInput = renderPlot({
    req(plot_data$plot_data)
    plot = plotInputs(plot_data$plot_data$data_inputs,
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
      plot = plotInputs(plot_data$plot_data$data_inputs,
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
    plot = plotOutputs(plot_data$plot_data$data_outputs,
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
      plot = plotOutputs(plot_data$plot_data$data_outputs,
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
      data_full = getFullResults(plot_data$plot_data)
      write.csv(data_full, file, row.names = TRUE)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

