library(shiny)
library(DT)
#library(shinyjqui)

shinyUI(fluidPage(
  tags$head(tags$style(
    type="text/css",
    "#logo img {max-width: 100%; width: 100%; height: auto}"),
    tags$script(src = "enter_button.js")
  ),
  
  sidebarLayout(#column(3, wellPanel(
    sidebarPanel(
      tags$a(imageOutput("logo", height = "100%", width = "100%"), href="http://californiadatacollaborative.org/"),
      h5(em("Please refer to your bill for the required inputs")),
      
      # selectInput("district",
      #             label = h5("Water District"),
      #             choices = c(DistrictList
      #             ),
      #             selected = "Moulton Niguel Water District"),
      
      #h4(""),
      h5("Address"),
      textInput("address", label = NULL, value = ""),
      
      actionButton("go", label = "Update"),
      
      # selectInput("cust_class", 
      #             label = h5("Customer Class"), 
      #             choices = c("Single Family Residential" = "RESIDENTIAL_SINGLE",
      #                         "Multi Family Residential" = "RESIDENTIAL_MULTI"
      # )),
      
      uiOutput("rateInputs"),
      
      width = 3
    ),
    
    mainPanel(#column(9,
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
                 
      h2("Residential Water Budget Bill Calculator",
         style = "font-family: 'Arial Narrow'; margin-top: 0.83 em; margin-bottom: 0em; font-weight:900 ; color:rgb(0,51,127)",
         align = "center"),
      
      HTML('<h4 style="font-family: &#39;Arial Narrow&#39;">
                Use the bill calculator to see how changes in your water usage will
                affect your bill.
                </h4>'),
      
      h4(em("Please note: The bill calculator is to be used for informational purposes only. 
                 Billed amounts may vary based on actual water usage, actual evapotranspiration rates, 
                 any changes in household size or irrigable area, and/or days in the billing cycle.
                 The calculator currently only supports water rates for single-family detached homes,
                 so water residents of multi-family apartments and condos as well as commercial, 
                 industrial or other water users may be charged differently.",
            style = "font-family: 'Arial Narrow'; margin-top: 0em")
      ),
      
      h3(textOutput("district")),
      
      splitLayout(cellWidths = c("43%", "43%", "14%"), 
                  plotOutput("use"), 
                  plotOutput("charge"),
                  plotOutput("legend")
      ),
      
      dataTableOutput("vol_table"),
      width = 9
    )#end mainPanel
  )
))