library(tidyverse)
library(FinCal)
library(shiny)
library(uuid)

# TODO: 
#  - Local File Persistence (https://shiny.rstudio.com/articles/persistent-data-storage.html)

# Utils
to_select_input <- function(tibble) {
  setNames(as.character(tibble$id), tibble$name)
}

# Persistence 
fields_to_save = c("id", "name", "desc", "property_value", "purchase_price", "mortgage_payment", "prop_taxes", "prop_insurance", "acquisition_costs", "down_pmt", "init_repair_cost", "property_mgmt_rate", "maintenance_reserve", "hoa_dues", "utilities", "years", "monthly_rent", "rent_appr_rate", "vacancy_rate", "property_appr_rate")

file_name <- "rei-prop-analysis-properties.csv"

save_data <- function(data, file_name) {
  write.csv(
    x = data,
    file = file.path(file_name),
    row.names = F, quote = T
  )
}

load_data <- function(file_name) {
  if(file.exists(file_name))
     as_tibble(read.csv(file_name))
}

# User Interface
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
          inputId = "create",
          label = "Create New",
          class = "btn btn-primary"
        )
      ),
      wellPanel(
        selectInput(
          inputId = "record",
          label = "Active data set",
          choices = c(1,2,3,4)
        )
      ),
      wellPanel(
        h3("Property Info"),
        div(
          class = "hidden",
          textInput(
            inputId = "id",
            value = "",
            label = "Property id"
          )
        ),
        textInput(
          inputId = "name",
          label = "Property name (e.g. address)",
          value = ""
        ),
        textInput(
          inputId = "desc",
          label = "Property description (e.g. condo, sfr)",
          value = ""
        ),
        numericInput(
          inputId = "property_value",
          label = "Property value",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "purchase_price",
          label = "Purchase price",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "mortgage_payment",
          label = "Mortgage payment",
          value = 0,
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
          value = 0.00,
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
          value = 1,
          min = 0,
          max = 30
        ),
        numericInput(
          inputId = "monthly_rent",
          label = "Monthly rent (yr 1)",
          value = 0,
          min = 0
        ),
        numericInput(
          inputId = "rent_appr_rate",
          label = "Annual Rent Appreciation",
          value = 0.00,
          min = 0,
          max = 1
        ),
        numericInput(
          inputId = "vacancy_rate",
          label = "Annual Vacancy Rate",
          value = 0.00,
          min = 0
        ),
        numericInput(
          inputId = "property_appr_rate",
          label = "Annual Property Appreciation",
          value = 0.00,
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

server <- function(input, output, session) {
  
  updateSelectInput(session, "record", choices = to_select_input(load_data(file_name)))
  
  # Save / Load functionality
  form_data <- reactive({
    data <- sapply(fields_to_save, function(x) input[[x]])
    
    data.frame(as.list(data), stringsAsFactors = F)
  })
  
  observeEvent(input$save, {
    record <- form_data()
    
    existing_data <- load_data(file_name)
    
    if(is.null(existing_data)) 
      existing_data <- record
    
    # Remove existing record if it exists and...
    existing_data <- existing_data[ !(existing_data$id %in% c(record$id)), ]
    
    # Replace with updated
    updated_data <- add_row(existing_data, 
                            id                  = record$id,
                            name                = record$name,
                            desc                = record$desc,
                            property_value      = record$property_value,
                            purchase_price      = record$purchase_price,
                            mortgage_payment    = record$mortgage_payment,
                            prop_taxes          = record$prop_taxes,
                            prop_insurance      = record$prop_insurance,
                            acquisition_costs   = record$acquisition_costs,
                            down_pmt            = record$down_pmt,
                            init_repair_cost    = record$init_repair_cost,
                            property_mgmt_rate  = record$property_mgmt_rate,
                            maintenance_reserve = record$maintenance_reserve,
                            hoa_dues            = record$hoa_dues,
                            utilities           = record$utilities,
                            years               = record$years,
                            monthly_rent        = record$monthly_rent,
                            rent_appr_rate      = record$rent_appr_rate,
                            vacancy_rate        = record$vacancy_rate,
                            property_appr_rate  = record$property_appr_rate)
    
    save_data(updated_data, file_name)
    
    updateSelectInput(session, 
                      "record", 
                      choices = to_select_input(updated_data), 
                      selected = record$id)
    
    showNotification("Data saved to local file", type = "message")
  })
  
  observeEvent(input$create, {
    updateTextInput(session, "id", value = UUIDgenerate())
    
    updateTextInput(session, "name",                value = "")
    updateTextInput(session, "desc",                value = "")
    updateTextInput(session, "property_value",      value = 0)
    updateTextInput(session, "purchase_price",      value = 0)
    updateTextInput(session, "mortgage_payment",    value = 0)
    updateTextInput(session, "prop_taxes",          value = 0)
    updateTextInput(session, "prop_insurance",      value = 0)
    updateTextInput(session, "acquisition_costs",   value = 0)
    updateTextInput(session, "down_pmt",            value = 0)
    updateTextInput(session, "init_repair_cost",    value = 0)
    updateTextInput(session, "property_mgmt_rate",  value = 0)
    updateTextInput(session, "maintenance_reserve", value = 0)
    updateTextInput(session, "hoa_dues",            value = 0)
    updateTextInput(session, "utilities",           value = 0)
    updateTextInput(session, "years",               value = 0)
    updateTextInput(session, "monthly_rent",        value = 0)
    updateTextInput(session, "rent_appr_rate",      value = 0)
    updateTextInput(session, "vacancy_rate",        value = 0)
    updateTextInput(session, "property_appr_rate",  value = 0)
    
    showNotification("New template created, please enter data for property.", type = "message")
  })
  
  observeEvent(input$record, {
    print(input$record)
    data = load_data(file_name)
    record = data[ (data$id %in% c(input$record)), ]
  
    updateTextInput(session, "id",                  value = record$id)
    updateTextInput(session, "name",                value = record$name)
    updateTextInput(session, "desc",                value = record$desc)
    updateTextInput(session, "property_value",      value = record$property_value)
    updateTextInput(session, "purchase_price",      value = record$purchase_price)
    updateTextInput(session, "mortgage_payment",    value = record$mortgage_payment)
    updateTextInput(session, "prop_taxes",          value = record$prop_taxes)
    updateTextInput(session, "prop_insurance",      value = record$prop_insurance)
    updateTextInput(session, "acquisition_costs",   value = record$acquisition_costs)
    updateTextInput(session, "down_pmt",            value = record$down_pmt)
    updateTextInput(session, "init_repair_cost",    value = record$init_repair_cost)
    updateTextInput(session, "property_mgmt_rate",  value = record$property_mgmt_rate)
    updateTextInput(session, "maintenance_reserve", value = record$maintenance_reserve)
    updateTextInput(session, "hoa_dues",            value = record$hoa_dues)
    updateTextInput(session, "utilities",           value = record$utilities)
    updateTextInput(session, "years",               value = record$years)
    updateTextInput(session, "monthly_rent",        value = record$monthly_rent)
    updateTextInput(session, "rent_appr_rate",      value = record$rent_appr_rate)
    updateTextInput(session, "vacancy_rate",        value = record$vacancy_rate)
    updateTextInput(session, "property_appr_rate",  value = record$property_appr_rate)
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
  
  reserves + prop_mgmt + dues + taxes + insurance + utilities
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

shinyApp(ui = ui, server = server)
