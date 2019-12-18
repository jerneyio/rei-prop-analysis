library(tibble)
library(ggplot2)
library(FinCal)
library(shiny)

# TODO: 
#  - Local File Persistence (https://shiny.rstudio.com/articles/persistent-data-storage.html)

ui <- fluidPage(
  titlePanel("Property Analysis"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        actionButton(
          inputId = "save",
          label = "Save",
          class = "btn btn-primary"
        ),
        actionButton(
          inputId = "load",
          label = "Load",
        )
      ),
      wellPanel(
        h3("Property Info"),
        textInput(
          inputId = "name",
          label = "Property name (e.g. address)",
          value = "1234 Main St"
        ),
        textInput(
          inputId = "desc",
          label = "Property description (e.g. condo, sfr)",
          value = "SFR"
        ),
        numericInput(
          inputId = "property_value",
          label = "Property value",
          value = 300000,
          min = 0
        ),
        numericInput(
          inputId = "purchase_price",
          label = "Purchase price",
          value = 300000,
          min = 0
        ),
        numericInput(
          inputId = "mortgage_payment",
          label = "Mortgage payment",
          value = 2000,
          min = 0
        ),
        numericInput(
          inputId = "prop_taxes",
          label = "Annual Property Taxes",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "prop_insurance",
          label = "Annual Property Insurance",
          value = 0,
          min = 0
        ),
        h3("One-time Expenses"),
        numericInput(
          inputId = "acquisition_costs",
          label = "Acquisition costs",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "down_pmt",
          label = "Down payment",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "init_repair_cost",
          label = "Initial repair costs",
          value = 0,
          min = 0
        ),
        h3("Monthly Property Expenses"),
        numericInput(
          inputId = "property_mgmt_rate",
          label = "Monthly Property Management Rate",
          value = 0.00,
          min = 0
        ),
        numericInput(
          inputId = "maintenance_reserve",
          label = "Maintenance Reserves Rate",
          value = 0.05,
          min = 0
        ),
        numericInput(
          inputId = "hoa_dues",
          label = "Monthly HOA Dues",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "utilities",
          label = "Monthly Utilities (if not covered by tenant)",
          value = 0,
          min = 0
        ),
        h3("Forecasting"),
        numericInput(
          inputId = "years",
          label = "Years to forecast",
          value = 5,
          min = 0,
          max = 30
        ),
        numericInput(
          inputId = "monthly_rent",
          label = "Monthly rent (yr 1)",
          value = 2500,
          min = 0
        ),
        numericInput(
          inputId = "rent_appr_rate",
          label = "Annual Rent Appreciation",
          value = 0.05,
          min = 0,
          max = 1
        ),
        numericInput(
          inputId = "vacancy_rate",
          label = "Annual Vacancy Rate",
          value = 0.05,
          min = 0
        ),
        numericInput(
          inputId = "property_appr_rate",
          label = "Annual Property Appreciation",
          value = 0.04,
          min = 0
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput(outputId = "summary")),
        tabPanel("Profit",  tableOutput(outputId = "profit"))
      )
    )
  )
)

server <- function(input, output) {
  # Save / Load functionality
  
  form_data <- reactive({
    data <- sapply(fields_to_save, function(x) input[[x]])
    
    data.frame(as.list(data), stringsAsFactors = F)
  })
  
  observeEvent(input$save, {
    save_data(form_data())
  })
  
  # Summary
  output$summary <- renderTable({
    initial_investment <- input$acquisition_costs + input$init_repair_cost + input$down_pmt
    
    ops_data <- calc_ops_data(
      tibble(
        rent_appr_rate      = input$rent_appr_rate,
        years               = input$years,
        rent                = input$monthly_rent * 12,
        maintenance_reserve = input$maintenance_reserve,
        property_mgmt_rate  = input$property_mgmt_rate,
        hoa_dues            = input$hoa_dues,
        utilities           = input$utilities,
        prop_taxes          = input$prop_taxes,
        prop_insurance      = input$prop_insurance,
        vacancy_rate        = input$vacancy_rate
      )
    )
    
    tibble(
      initial_investment = initial_investment,
      cocror             = ops_data$net_operating_income[[1]] / initial_investment,
      cap_rate           = ops_data$net_operating_income[[1]] / input$purchase_price
    )
  })
  
  # Profit table
  output$profit <- renderTable({
    annual_property_values <- fv.simple(input$property_appr_rate, 
                                        1:input$years, 
                                        -input$property_value)
    
    ops_data <- calc_ops_data(
      tibble(
        rent_appr_rate      = input$rent_appr_rate,
        years               = input$years,
        rent                = input$monthly_rent * 12,
        maintenance_reserve = input$maintenance_reserve,
        property_mgmt_rate  = input$property_mgmt_rate,
        hoa_dues            = input$hoa_dues,
        utilities           = input$utilities,
        prop_taxes          = input$prop_taxes,
        prop_insurance      = input$prop_insurance,
        vacancy_rate        = input$vacancy_rate
      )
    )
    
    annual_debt_service <- input$mortgage_payment * 12
    
    tibble(
      year             = 1:input$years,
      property_value   = annual_property_values,
      operating_costs  = ops_data$annual_operating_costs,
      revenue          = ops_data$annual_revenues,
      noi              = ops_data$net_operating_income,
      annual_cash_flow = noi - annual_debt_service
    )
  })
}

# Calculations
annual_expenses <- function(maintenance_reserve, 
                            property_management, 
                            hoa_dues, 
                            utilities, 
                            taxes, 
                            insurance, 
                            rents) {
  reserves <- rents * maintenance_reserve
  prop_mgmt <- rents * property_management
  dues <- hoa_dues * 12
  
  reserves + prop_mgmt + dues + taxes + insurance
}

annual_revenues <- function(rents, vacancy) {
  rents - (rents * vacancy)
}

calc_ops_data <- function(tbl) {
  tibble(
    annual_rents = fv.simple(tbl$rent_appr_rate, 1:tbl$years, -tbl$rent),
    annual_operating_costs = annual_expenses(
      tbl$maintenance_reserve, 
      tbl$property_mgmt_rate, 
      tbl$hoa_dues,
      tbl$utilities,
      tbl$prop_taxes,
      tbl$prop_insurance,
      annual_rents),
    annual_revenues = annual_revenues(annual_rents, tbl$vacancy_rate),
    net_operating_income = annual_revenues - annual_operating_costs
  )
}

# Persistence 
fields_to_save = c("name", "desc", "property_value", "purchase_price", "mortgage_payment", "prop_taxes", "prop_insurance", "acquisition_costs", "down_pmt", "init_repair_cost", "property_mgmt_rate", "maintenance_reserve", "hoa_dues", "utilities", "years", "monthly_rent", "rent_appr_rate", "vacancy_rate", "property_appr_rate")

save_data <- function(data) {
  fileName <- "rei-prop-analysis-properties.csv"
  write.csv(
    x = data,
    file = file.path(fileName),
    row.names = F, quote = T
  )
}

shinyApp(ui = ui, server = server)
