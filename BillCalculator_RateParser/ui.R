library(shiny)
library(DT)
#library(shinyjqui)

ET <- data.frame(Months = month.name,ETF = c(2.26,2.53,3.08,3.74,4.54,4.44,4.71,4.56,4.97,3.88,3.15,2.29))

shinyUI(fluidPage(
  #tags$style(type="text/css", "body { overflow-y: scroll; }"),
  #fluidRow(
  sidebarLayout(#column(3, wellPanel(
    sidebarPanel(
      imageOutput("logo", height = 120, width = 171),
      h5(em("Please refer to your bill for the required inputs")),
      
      selectInput("district", 
                  label = h5("Water District"),
                  choices = c(DistrictList
                  ),
                  selected = "Moulton Niguel Water District"),
      
      selectInput("cust_class", 
                  label = h5("Customer Class"), 
                  choices = c("Single Family Residential" = "RESIDENTIAL_SINGLE",
                              "Multi Family Residential" = "RESIDENTIAL_MULTI"
                              #"Commercial" = "COMMERCIAL",
                              #"Commercial Non-Irrigation",
                              #"Irrigation" = "IRRIGATION"
      )),
      
      conditionalPanel(
        condition = "input.district == 'Alameda County Water District - 28'||input.district == 'San Bernardino City of - 2503'||input.district == 'Pasadena  City Of - 2136'||input.district == 'Los Angeles Department of Water and Power - 1665'",
        selectInput("city_limits", label = h5("City Limits"), choices = c("inside_city",
                                                                          "outside_city"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Coachella Valley Water District - 661'",
        numericInput("usage_month", label = h5("Usage Month (1-12)"),
                     value = 1, min = 1, max = 12)
        ),
      
      conditionalPanel(
        condition = "input.district == 'Coachella Valley Water District - 661'",
        numericInput("usage_zone", label = h5("Usage Zone (1-5)"),
                     value = 1, min = 1, max = 5)
      ),
      
      conditionalPanel(
        condition = "input.district == 'Desert Water Agency - 832'",
        numericInput("pressure_zone", label = h5("Pressure Zone (1-4)"),
                     value = 1, min = 1, max = 4)
      ),
      
      conditionalPanel(
        condition = "input.district == 'Las Virgenes Municipal Water District - 1566'||input.district == 'San Bernardino City of - 2503'||input.district == 'Pittsburg  City of - 133'",
        numericInput("elevation_zone", label = h5("Elevation Zone"),
                     value = 1, min = 1, max = 6)
      ),
      
      conditionalPanel(
        condition = "input.district == 'Los Angeles Department of Water and Power - 1665'",
        numericInput("lot_size_group", label = h5("Lot Size Group (1-5)"),
                     value = 1, min = 1, max = 5)
      ),
      
      conditionalPanel(
        condition = "input.district == 'Los Angeles Department of Water and Power - 1665'",
        selectInput("temperature_zone", label = h5("Temperature Zone"), choices = c("Low",
                                                                                    "Medium",
                                                                                    "High"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Pittsburg  City of - 133'",
        selectInput("senior", label = h5("Senior"), choices = c("yes",
                                                                "no"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Fresno City of - 1072'",
        selectInput("water_font", label = h5("Water Font"), choices = c("city_delivered",
                                                                        "private_wells"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Long Beach City of - 1656'",
        selectInput("tax_exemption", label = h5("Tax Exemption"), choices = c("granted",
                                                                              "not_granted"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'El Dorado Irrigation District - Main - 934'",
        selectInput("turbine_meter", label = h5("Turbine Meter"), choices = c("Yes",
                                                                              "No"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Huntington Beach City of - 1376'||input.district == 'Livermore  City of - 1631'",
        selectInput("meter_type", label = h5("Meter Type"), choices = c("compound",
                                                                        "FM",
                                                                        "Turbine",
                                                                        "Displacement"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Long Beach City of - 1656'||input.district == 'Arcadia  City Of - 132'||input.district == 'Pasadena  City Of - 2136'||input.district == 'Los Angeles Department of Water and Power - 1665'",
        selectInput("season", label = h5("Season"), choices = c("Winter",
                                                                "Summer"
        )
        )),
      
      conditionalPanel(
        condition = "input.district == 'Suburban Water Systems San Jose Hills - 16'",
        selectInput("tariff_area", label = h5("Tariff Area"), choices = c(1,2,3)
        )),
      
      conditionalPanel(
        condition = "input.district == 'Suburban Water Systems San Jose Hills - 16'",
        selectInput("block", label = h5("Block"), choices = c(1,2)
        )),
      
      # conditionalPanel(
      #   condition = "input.cust_class == 'IRRIGATION'",
      #   selectInput("water_type", label = h5("Water Type"), choices = c("POTABLE",
      #                                                                   "RECYCLED"
      #   )
      #   )),
      conditionalPanel(
        condition = "input.cust_class == 'RESIDENTIAL_SINGLE'||input.cust_class == 'RESIDENTIAL_MULTI'",
        icon("users"),
        numericInput("homesize", label = h5("Persons in Household"),
                     value = NULL, min = 0, max = 99)
      ),
      
      
      # conditionalPanel(
      #   condition = "input.cust_class == 'Commercial Non-Irrigation'",icon("tint"),
      #   numericInput("typical_usage", label = h5("Typical historical Usage (BU) for a Billing Period"),
      #                value = NULL, min = 0, max = 999)
      # ),
      # conditionalPanel(
      #   condition = "input.cust_class != 'Commercial Non-Irrigation'",icon("tree"),
      #   numericInput("irr_area", label = h5("Irrigable Area (sq. ft.)"), value = NULL, min = 0, max = 99999)
      # ),
      # conditionalPanel(
      #   condition = "input.cust_class == 'Multi Family Residential'",
      #   numericInput("homesize", label = h5("Persons in Household"),
      #                value = NULL, min = 0, max = 99)
      # ),
 
      h4(""),icon("tree"),
      h5("Irrigable Area (sq. ft.)"),
      #h6("(1 BU = 748 gallons)"),
      numericInput("irr_area", label = NULL, value = NULL, min = 0, max = 99999),
      
      # h4(""),
      # h5("Address"),
      # #h6("(1 BU = 748 gallons)"),
      # textInput("address", label = NULL, value = ""),
      
      
      h4(""),icon("tint"),
      h5("Billing Units Used"),
      h6("(1 BU = 748 gallons)"),
      numericInput("usage", label = NULL, value = NULL, min = 0, max = 999),
      h4(""),icon("calendar"),
      h5("Days in Billing Cycle"),
      h6("(Typically 28-35 days)"),
      numericInput("bill_days", label = NULL, value = NULL, min = 0, max = 99),
      
      
      h4(""),icon("leaf"),
      h5("Evapotranspiration"),
      h6("(This month's average ET - ",ET$ETF[ET$Months == months.Date(Sys.Date())],")"),
      #       h6("(ET varies daily by microzone. Your water budget is calculated based on the actual ET during the billing period; 
      #         however, it is possible to estimate your water budget based on historical ET. 
      #         The historical average ET values for the MNWD service area are:\n
      #         Jan = 2.17; Feb = 2.80; Mar=3.72;\n
      #         Apr=4.80; May=5.27; Jun=5.40;\n
      #         Jul=5.89; Aug=5.58; Sep=5.10;\n
      #         Oct=4.03; Nov=2.70; Dec=2.17
      #       )"),
      numericInput("et", label = NULL, value = NULL, min = 0, max = 99),
      h4(""),
      h5("Meter Size"),
      h6("(Typically 3/4 in. for residential customers)"),
      selectInput("meter", 
                  label = NULL,
                  choices = c("5/8 in." = '5/8"', "3/4 in." = '3/4"',
                              "1 in." = '1"', "1 1/2 in." = '1 1/2"',
                              "2 in." = '2"', "3 in." = '3"',
                              "4 in." = '4"', "6 in." = '6"',
                              "8 in." = '8"', "10 in." = '10"'),
                  selected = '3/4"'),
      textOutput("budget")
      ,width = 3),#)
    
    mainPanel(#column(9,
      h2("Water Budget Bill Calculator and Bill Comparison",
         style = "font-family: 'Arial Narrow'; margin-top: 0.83 em; margin-bottom: 0em; font-weight:900 ; color:rgb(0,51,127)",
         align = "center"),
      HTML('<h4 style="font-family: &#39;Arial Narrow&#39;">
                Use the bill calculator to see how changes in your water usage will
                affect your bill.  For more information on our water budget based rate structure,
                visit 
                <a href="https://www.mnwd.com/waterbudgetbasedrates/", target="_parent">www.mnwd.com/waterbudgetbasedrates/</a><br>
                To determine the Evapotranspiration amount for your microzone, refer to your bill statement or go
                to our <a href="https://www.mnwd.com/watersavingtools/", target="_blank">water saving tools page</a> and use the Evapotranspiration Map.
                
                </h4>'),
      h4(em("Please note: The bill calculator is to be used for informational purposes only. 
                 Billed amounts may vary based on actual water usage, actual evapotranspiration rates, 
                 any changes in household size or irrigable area, and/or days in the billing cycle.
                 For Single Family and Multi Family Residentials, Tier 1 and Tier 2 account to Indoor and Outdoor
                 Budgets respectively.",
            style = "font-family: 'Arial Narrow'; margin-top: 0em")),
      splitLayout(cellWidths = c("43%", "43%", "14%"), plotOutput("use"), plotOutput("charge"),plotOutput("legend")),
      #plotOutput("use"),
      dataTableOutput("vol_table")
      #dataTableOutput("bill_table")
      ,width = 9)
  )
))