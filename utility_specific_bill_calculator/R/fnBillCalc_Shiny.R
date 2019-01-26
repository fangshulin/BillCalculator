## Bill Calculation Formula
####To be used as an external call for Shiny Dashboard

library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(forcats)
library(RateParser)
library(sf)
library(dplyr)
library(maps)
library(maptools)
library(ggmap)

source("R/plots.R")


#library(DT)

#not used anymore
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
#helper rounding fn
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#To center the title by default for all ggplots
theme_update(plot.title = element_text(hjust = 0.5))

# Function below to standardize the OWRS files
standardize_OWRS_names <- function(owrs_file, current_class){
  mask <- names(owrs_file$rate_structure[[current_class]]) == "tier_starts_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "tier_starts"

  mask <- names(owrs_file$rate_structure[[current_class]]) == "tier_prices_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "tier_prices"

  mask <- names(owrs_file$rate_structure[[current_class]]) == "flat_rate_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "flat_rate"
  
  mask <- names(owrs_file$rate_structure[[current_class]]) == "budget_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "budget"
  
  mask <- names(owrs_file$rate_structure[[current_class]]) == "indoor_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "indoor"
  
  mask <- names(owrs_file$rate_structure[[current_class]]) == "outdoor_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "outdoor"
  
  mask <- names(owrs_file$rate_structure[[current_class]]) == "gpcd_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "gpcd"
  
  mask <- names(owrs_file$rate_structure[[current_class]]) == "landscape_factor_commodity"
  names(owrs_file$rate_structure[[current_class]])[mask] <- "landscape_factor"

  if(owrs_file$rate_structure[[current_class]]$commodity_charge == "flat_rate_commodity*usage_ccf"){
    owrs_file$rate_structure[[current_class]]$commodity_charge <- "flat_rate*usage_ccf"
  }

  owrs_file$rate_structure[[current_class]]$fixed_drought_surcharge <- NULL
  owrs_file$rate_structure[[current_class]]$variable_drought_surcharge <- NULL
  owrs_file$rate_structure[[current_class]]$fixed_wastewater_charge <- NULL
  owrs_file$rate_structure[[current_class]]$variable_wastewater_charge <- NULL

  return(owrs_file)
}


#Function to Calculate Usage by Tier and Plot
#************One-shot Version********************  
fnUseByTier <- function(df1, owrs_file){
  ##########################################tier###############################################
  custclass <- df1$cust_class
  owrs_file <- standardize_OWRS_names(owrs_file, custclass)
  calced <- calculate_bill(df1, owrs_file)
  commodity_id <- as.character(owrs_file$rate_structure[[as.character(custclass)]]$commodity_charge)
  
  if((commodity_id == "Budget")  || (commodity_id == "Tiered")){
    tierstart <- owrs_file$rate_structure[[as.character(custclass)]]$tier_starts
    if(class(tierstart) == "list"){
      if(!is.null(tierstart$values)){
        num_tiers <- length(tierstart$values[[1]])
      }
      else{
        num_tiers <- length(tierstart)
      }
    }
    else{
      num_tiers <- length(tierstart)
    }
    
    ##******Initial Use and Rate Arrays******
    #Tier Use and Rate Arrays
    tierAll = c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5","Tier 6")
    inputs <- data.frame(
      tiers = c(tierAll[1:num_tiers]),
      tier_use = numeric(num_tiers),
      tier_rates = numeric(num_tiers),  # Stage 1 Rates
      tier_charge =  numeric(num_tiers)
    )
    rownames(inputs) <- tierAll[1:num_tiers]
    
    ##*************************************   
    #determine use per tier given customer usage
    
    
    for(i in c(1:num_tiers)) {
      col_name <- sprintf("X%s", i)
      inputs$tier_use[i] <- calced[[col_name]]
    }
    total_usage <- sum(inputs$tier_use)
    
    #determine charge per tier
    
    for(i in c(1:num_tiers)) {
      col_name <- sprintf("XR%s", i)
      inputs$tier_charge[i] <- calced[[col_name]]
    }
    inputs$tier_charge
    total_charge <- sum(inputs$tier_charge[1:num_tiers])
    
    total_vol_charge <- calced$commodity_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    
    #***************graphs**************************    
    
    p1 <- plot_usage(df, num_tiers, total_usage)
    p2 <- plot_charges(df, num_tiers, total_charge)
    plegd <- plot_legend(df, num_tiers)
    
    #Sum of Current Charges
    total_bill = calced$bill
    
    #Table Preparation
    vol_table <- data.frame(`Usage Tier` = c(tierAll[1:num_tiers],
                                             "Total Water\nUsage Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                           total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier",
                             "Current Charge")
    
    
    #Fn ouputs plot/table
    plotList<-list(p1,p2,plegd,vol_table)
    return(plotList)
      #grid.arrange(arrangeGrob(p1, p2,plegd, ncol = 3, widths = unit(c(2/5,2/5,1/5),"npc")),
       #nullGrob()
                   #arrangeGrob(t1, nullGrob(),t2, ncol = 1, heights = unit(c(4/10,2/10,4/10),"npc")),
                   ## Information on Stage 2 penalties
                   # textGrob("*$7.43 of the $9.21 per BU is a penalty for using water in excess of your water budget.",
                   #          gp=gpar(fontsize=12,fontface="italic")),
                   #nrow=4, ncol=1,
                   #heights=c(3, 1/8, 3, 1)
       #)
    # }else
    #   bill_table

  }
  else{
    total_vol_charge <- calced$commodity_charge
    total_bill = calced$bill
    vol_table <- data.frame(`Usage Tier` = c("Total Water\nUsage Charge",
                                             "Total Bill"),
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(total_vol_charge,
                                                                           total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage",
                             "Current Charge")
    plotList<-list(NULL,NULL,NULL, vol_table)
    return(plotList)
  }
}

# #************************************* 
