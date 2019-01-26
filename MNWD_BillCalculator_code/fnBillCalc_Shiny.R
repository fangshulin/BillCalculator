## Bill Calculation Formula
####To be used as an external call for Shiny Dashboard

library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(forcats)

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

#Function to Calculate Usage by Tier and Plot
#************One-shot Version********************  
fnUseByTier <- function(usage = 70, gpcd = 60, hh_size = 4, days = 30, conv_gal2ccf = 748,
                        lot_size = 6000, plant_factor = 0.7, evapo_trans = 3.43, conv_in2gal = 0.62,
                        meter_selection = 0.625, cust_class, commercial_class, tablemode,
                        typical_usage){
  ##########################################Single Family Residential############################################### 
  if(cust_class == "Single Family Residential"){
    ##******Initial Use and Rate Arrays******
    #Tier Use and Rate Arrays  
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5"),
      tier_breaks = c(0,0,0,0,0),
      tier_use = c(0,0,0,0,0),
      tier_rates = c(1.56,1.78,2.73,4.49,9.28),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0,0),
      tier_charge_drought =  c(0,0,0,0,0),
      new_tier_breaks = c(0,0,0,0,0),
      new_tier_use = c(0,0,0,0,0),
      new_tier_rates = c(1.69,1.94,3.32,5.12,9.59),
      new_tier_charge = c(0,0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(11.91, 11.91, 11.91, 39.73, 63.57,
                      139.06, 238.36, 497.00, 715.10, 1152.50),
      new_meter_rates = c(11.22,11.22,11.22,37.41,59.85,130.94,224.46,467.62,673.37,
                          1084.87),
      ww_service_charge = c(14.36, 14.36,14.36,14.36,14.36,14.36,14.36,14.36,14.36,14.36)
      
    )
    ##*************************************   
    
    #determine tier breaks given customer inputs
    inputs$tier_breaks[1] <- round2(60*hh_size*days/conv_gal2ccf,0)
    inputs$tier_breaks[2] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$tier_breaks[3] <- round2(sum(inputs$tier_breaks[1:2])*1.25,0)-round2(sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_breaks[4] <- round2((sum(inputs$tier_breaks[1:2])*1.5),0)-round2((sum(inputs$tier_breaks[1:2])*1.25),0)
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(55*hh_size*days/conv_gal2ccf,0)
    inputs$new_tier_breaks[2] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$new_tier_breaks[3] <- round2(sum(inputs$new_tier_breaks[1:2])*1.25,0)-round2(sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_breaks[4] <- round2((sum(inputs$new_tier_breaks[1:2])*1.5),0)-round2((sum(inputs$new_tier_breaks[1:2])*1.25),0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(min(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:3]),0)
    inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[3:5])
    #determine new use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(min(usage,sum(inputs$new_tier_breaks[1:4]))-sum(inputs$new_tier_breaks[1:3]),0)
    inputs$new_tier_use[5] <- max(max(usage,sum(inputs$new_tier_breaks[1:4]))-sum(inputs$new_tier_breaks[1:4]),0)
    new_usage <- sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[3:5])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:5])
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    inputs$new_tier_charge[5] <- inputs$new_tier_use[5]*inputs$new_tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:5])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************    
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),n = 5)
    p1 <- ggplot(subset(df, variable %in% c("tier_use", "new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Usage by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", "new_tier_use" = "Proposed\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier  
    bill_breaks <- pretty(c(1:new_use_charge),5)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier  
    plegd_breaks <- c(0:6)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "orange", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4","5"),
                        labels = c("Tier 5","Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    #get current fixed water charge
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    #get new fixed water charge
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    #curent fixed wastewater/sewer charge
    total_ww_service_charge = 26.22
    #get new fixed wastewater/sewer charge
    new_total_ww_service_charge = inputs_fixed$ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    #Sewer Usage Charge($4.31/Person)
    per_person_sewer_charge = hh_size*4.31
    #Sum of Current Charges
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    #Sum of New Charges
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge, per_person_sewer_charge)
    #All Current Charges
    current_charges = c(total_vol_charge,total_fix_charge,total_ww_service_charge,
                        0, total_bill)
    #All Proposed Charges
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge,
                         new_total_ww_service_charge, per_person_sewer_charge,
                         new_total_bill)
    
    #Table Preparation
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4","Tier 5",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Basic Sewer Charge",
                                             "Per Person Sewer Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"","","","",""),
                            
                            `New Usage Per Tier` = c(inputs$new_tier_use,"","","","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","","","",""),
                            
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","","","$4.31",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                           total_fix_charge,total_ww_service_charge,
                                                                           0, total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,new_total_vol_charge,
                                                                            new_total_fix_charge,
                                                                            new_total_ww_service_charge, per_person_sewer_charge,
                                                                            new_total_bill)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
    #                                     "Per Person Sewer Charge","Total Bill"),
    #                          Current = paste0("$",
    #                                          formatC(as.numeric(current_charges),
    #                                                  format="f",
    #                                                  digits=2,
    #                                                  big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    #Fn ouputs plot/table
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }  
    # }else
    #   bill_table
    
    ##########################################Multi Family Residential################################################# 
  } else if (cust_class == "Multi Family Residential" ){
    ##******Initial Use and Rate Arrays******
    #Tier Use and Rate Arrays  
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5"),
      tier_breaks = c(0,0,0,0,0),
      tier_use = c(0,0,0,0,0),
      tier_rates = c(1.56,1.78,2.73,4.49,9.28),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0,0),
      tier_charge_drought =  c(0,0,0,0,0),
      new_tier_breaks = c(0,0,0,0,0),
      new_tier_use = c(0,0,0,0,0),
      new_tier_rates = c(1.69,1.94,3.32,5.12,9.59),
      new_tier_charge = c(0,0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(7.33,7.33,7.33,24.45,39.11,85.57,146.69,305.85,440.06,709.24),
      new_meter_rates = c(10.78,10.78,10.78,25.20,37.56,76.70,128.19,262.09,375.38,
                          601.96),
      ww_service_charge = c(28.58,28.58,28.58,87.76,138.50,299.17,510.54,1060.15,1525.19,
                            2455.30),
      new_ww_service_charge = c(16.94,16.94,16.94,51.38,80.91,174.42,297.44,617.32,887.98,
                                1429.31)
      
    )
    ##*************************************   
    
    #determine tier breaks given customer inputs
    inputs$tier_breaks[1] <- round2(60*hh_size*days/conv_gal2ccf,0)
    inputs$tier_breaks[2] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$tier_breaks[3] <- round2(sum(inputs$tier_breaks[1:2])*1.25,0)-round2(sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_breaks[4] <- round2((sum(inputs$tier_breaks[1:2])*1.5),0)-round2((sum(inputs$tier_breaks[1:2])*1.25),0)
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(55*hh_size*days/conv_gal2ccf,0)
    inputs$new_tier_breaks[2] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$new_tier_breaks[3] <- round2(sum(inputs$new_tier_breaks[1:2])*1.25,0)-round2(sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_breaks[4] <- round2((sum(inputs$new_tier_breaks[1:2])*1.5),0)-round2((sum(inputs$new_tier_breaks[1:2])*1.25),0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(min(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:3]),0)
    inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[3:5])
    #determine new use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(min(usage,sum(inputs$new_tier_breaks[1:4]))-sum(inputs$new_tier_breaks[1:3]),0)
    inputs$new_tier_use[5] <- max(max(usage,sum(inputs$new_tier_breaks[1:4]))-sum(inputs$new_tier_breaks[1:4]),0)
    new_usage<-sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[3:5])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:5])
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    inputs$new_tier_charge[5] <- inputs$new_tier_use[5]*inputs$new_tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:5])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************    
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),5)
    p1 <- ggplot(subset(df, variable %in% c("tier_use", "new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Use by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", 
                                "new_tier_use" = "Proposed\n Usage"))+
      
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier  
    bill_breaks <- pretty(c(1:new_use_charge),5)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier  
    plegd_breaks <- c(0:6)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "orange", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4","5"),
                        labels = c("Tier 5","Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #calculations
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    total_ww_service_charge = inputs_fixed$ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    new_total_ww_service_charge = inputs_fixed$new_ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    per_person_sewer_charge = hh_size*4.31
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge, per_person_sewer_charge)
    current_charges = c(total_vol_charge,total_fix_charge,total_ww_service_charge,
                        0, total_bill)
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge,
                         new_total_ww_service_charge, per_person_sewer_charge,
                         new_total_bill)
    
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4","Tier 5",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Basic Sewer Charge",
                                             "Per Person Sewer Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"","","","",""),
                            
                            `New Usage Per Tier` = c(inputs$new_tier_use,"","","","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","","","",""),
                            
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","","","$4.31",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                           total_fix_charge,total_ww_service_charge,
                                                                           0, total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,new_total_vol_charge,
                                                                            new_total_fix_charge,
                                                                            new_total_ww_service_charge, per_person_sewer_charge,
                                                                            new_total_bill)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
    #                                              "Per Person Sewer Charge","Total Bill"),
    #                          Current = paste0("$",
    #                                           formatC(as.numeric(current_charges),
    #                                                   format="f",
    #                                                   digits=2,
    #                                                   big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }
    #else
    # bill_table
    ##########################################Commercial############################################################
  }else if (cust_class == "Commercial" ){
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4"),
      tier_breaks = c(0,0,0,0),
      tier_use = c(0,0,0,0),
      tier_rates = c(1.78,2.73,4.49,9.28),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0),
      tier_charge_drought =  c(0,0,0,0),
      new_tier_breaks = c(0,0,0,0),
      new_tier_use = c(0,0,0,0),
      new_tier_rates = c(1.94,3.32,5.12,9.59),
      new_tier_charge = c(0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(6.55,6.55,6.55,21.84,34.94,76.42,131.00,273.14,393.00,633.39),
      new_meter_rates = c(5.54,5.54,5.54,18.46,29.54,64.61,110.76,230.76,332.29,
                          535.36))
    if(commercial_class == "Commercial Class 1"){   
      inputs_fixed$ww_service_charge <- c(20.66,20.66,20.66,61.35,96.23,206.69,352.02,729.89,1049.61,1689.08)
      inputs_fixed$new_ww_service_charge <- c(20.84,20.84,20.84,64.39,101.73,219.98,375.55,
                                              780.05,1122.30,1806.83)
    }else if (commercial_class == "Commercial Class 2"){
      inputs_fixed$ww_service_charge <- c(44.02,44.02,44.02,139.21,220.81,479.25,819.25,1703.30,2451.32,3947.40)
      inputs_fixed$new_ww_service_charge <- c(49.92,49.92,49.92,161.31,256.81,559.23,957.11,
                                              1991.64,2866.98,4617.72)
    }else if (commercial_class == "Commercial Class 3"){
      inputs_fixed$ww_service_charge <- c(90.56,90.56,90.56,294.33,469.01,1022.23,1750.04,3642.47,5243.70,8446.24)
      inputs_fixed$new_ww_service_charge <- c(107.85,107.85,107.85,354.40,565.75,1235.12,2115.74,4405.47,
                                              6342.87,10217.77)
    }else{
      inputs_fixed$ww_service_charge <- c(97.70,97.70,97.70,318.12,507.08,1105.51,1892.81,3939.89,5671.99,9136.27)
      inputs_fixed$new_ww_service_charge <- c(117.98,117.98,117.98,388.15,619.76,1353.26,2318.26,4827.39,
                                              6950.43,11196.63)
    }
    ##*************************************
    
    #determine tier breaks given customer inputs
    #   inputs$tier_breaks[1] <- round(gpcd*hh_size*days/conv_gal2ccf, digits = 0)
    #   inputs$tier_breaks[2] <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
    #   inputs$tier_breaks[3] <- round(sum(inputs$tier_breaks[1:2])*0.25, digits = 0)
    #   inputs$tier_breaks[4] <- round(sum(inputs$tier_breaks[1:2])*0.25, digits = 0)
    
    #inputs$tier_breaks[1] <- round2(gpcd*hh_size*days/conv_gal2ccf,0)
    inputs$tier_breaks[1] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$tier_breaks[2] <- round2(inputs$tier_breaks[1]*1.25,0)-round2(inputs$tier_breaks[1],0)
    inputs$tier_breaks[3] <- round2(inputs$tier_breaks[1]*1.5,0)-round2(inputs$tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(max(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[2:4])
    
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$new_tier_breaks[2] <- round2(inputs$new_tier_breaks[1]*1.25,0)-round2(inputs$new_tier_breaks[1],0)
    inputs$new_tier_breaks[3] <- round2(inputs$new_tier_breaks[1]*1.5,0)-round2(inputs$new_tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(max(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    new_usage<-sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[2:4])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:4])
    
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:4])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    #inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),4)
    p1 <- ggplot(subset(df, variable %in% c("tier_use", "new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Use by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", 
                                "new_tier_use" = "Proposed\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier
    bill_breaks <- pretty(c(1:new_use_charge),4)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier  
    plegd_breaks <- c(0:5)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4"),
                        labels = c("Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #calculations
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    total_ww_service_charge = inputs_fixed$ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    new_total_ww_service_charge = inputs_fixed$new_ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    per_person_sewer_charge = 0
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge)
    current_charges = c(total_vol_charge,total_fix_charge,total_ww_service_charge,
                        total_bill)
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge,
                         new_total_ww_service_charge,
                         new_total_bill)
    
    #***************tables*************************
    #build a summary table of total bill
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Basic Sewer Charge",
                                             "Total Bill"),
                            
                            `Usage Per Tier` = c(inputs$tier_use,"","","",""),
                            
                            `New Usage Per Tier` = c(inputs$tier_use,"","","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","","",""),
                            
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","","",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                           total_fix_charge,total_ww_service_charge,
                                                                           total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,new_total_vol_charge,
                                                                            total_fix_charge,
                                                                            new_total_ww_service_charge,
                                                                            new_total_bill)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
    #                                              "Total Bill"),
    #                          Current = paste0("$",
    #                                           formatC(as.numeric(current_charges),
    #                                                   format="f",
    #                                                   digits=2,
    #                                                   big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    # 
    
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }
    # else
    # bill_table
    
    ##################################Commercial Non-Irrigation############################################################
  }else if (cust_class == "Commercial Non-Irrigation" ){
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4"),
      tier_breaks = c(0,0,0,0),
      tier_use = c(0,0,0,0),
      tier_rates = c(1.78,2.73,4.49,9.28),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0),
      tier_charge_drought =  c(0,0,0,0),
      new_tier_breaks = c(0,0,0,0),
      new_tier_use = c(0,0,0,0),
      new_tier_rates = c(1.94,3.32,5.12,9.58),
      new_tier_charge = c(0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(6.55,6.55,6.55,21.84,34.94,76.42,131.00,273.14,393.00,633.39),
      new_meter_rates = c(5.54,5.54,5.54,18.46,29.54,64.61,110.76,230.76,332.29,
                          535.36))
    if(commercial_class == "Commercial Class 1"){   
      inputs_fixed$ww_service_charge <- c(20.66,20.66,20.66,61.35,96.23,206.69,352.02,729.89,1049.61,1689.08)
      inputs_fixed$new_ww_service_charge <- c(20.84,20.84,20.84,64.39,101.73,219.97,375.54,
                                              780.02,1122.27,1806.78)
    }else if (commercial_class == "Commercial Class 2"){
      inputs_fixed$ww_service_charge <- c(44.02,44.02,44.02,139.21,220.81,479.25,819.25,1703.30,2451.32,3947.40)
      inputs_fixed$new_ww_service_charge <- c(49.91,49.91,49.91,161.30,256.78,559.18,957.02,
                                              1991.46,2866.73,4617.31)
    }else if (commercial_class == "Commercial Class 3"){
      inputs_fixed$ww_service_charge <- c(90.56,90.56,90.56,294.33,469.01,1022.23,1750.04,3642.47,5243.70,8446.24)
      inputs_fixed$new_ww_service_charge <- c(107.85,107.85,107.85,354.40,565.76,1235.12,2115.74,4405.48,
                                              6342.88,10217.80)
    }else{
      inputs_fixed$ww_service_charge <- c(97.70,97.70,97.70,318.12,507.08,1105.51,1892.81,3939.89,5671.99,9136.27)
      inputs_fixed$new_ww_service_charge <- c(117.98,117.98,117.98,388.15,619.77,1353.28,2318.29,4827.46,
                                              6950.53,11196.79)
    }
    ##*************************************
    
    #determine tier breaks given customer inputs
    #   inputs$tier_breaks[1] <- round(gpcd*hh_size*days/conv_gal2ccf, digits = 0)
    #   inputs$tier_breaks[2] <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
    #   inputs$tier_breaks[3] <- round(sum(inputs$tier_breaks[1:2])*0.25, digits = 0)
    #   inputs$tier_breaks[4] <- round(sum(inputs$tier_breaks[1:2])*0.25, digits = 0)
    
    #inputs$tier_breaks[1] <- round2(gpcd*hh_size*days/conv_gal2ccf,0)
    inputs$tier_breaks[1] <- round2(((usage+typical_usage)/2),0)
    inputs$tier_breaks[2] <- round2(inputs$tier_breaks[1]*1.25,0)-round2(inputs$tier_breaks[1],0)
    inputs$tier_breaks[3] <- round2(inputs$tier_breaks[1]*1.5,0)-round2(inputs$tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(max(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[2:4])
    
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(((usage+typical_usage)/2),0)
    inputs$new_tier_breaks[2] <- round2(inputs$new_tier_breaks[1]*1.25,0)-round2(inputs$new_tier_breaks[1],0)
    inputs$new_tier_breaks[3] <- round2(inputs$new_tier_breaks[1]*1.5,0)-round2(inputs$new_tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(max(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    new_usage<-sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[2:4])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:4])
    
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:4])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    #inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),4)
    p1 <- ggplot(subset(df, variable %in% c("tier_use", "new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Use by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", 
                                "new_tier_use" = "Proposed\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier
    bill_breaks <- pretty(c(1:new_use_charge),4)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier  
    plegd_breaks <- c(0:5)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4"),
                        labels = c("Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #calculations
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    total_ww_service_charge = inputs_fixed$ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    new_total_ww_service_charge = inputs_fixed$new_ww_service_charge[inputs_fixed$meter_sizes == meter_selection]
    per_person_sewer_charge = 0
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge)
    current_charges = c(total_vol_charge,total_fix_charge,total_ww_service_charge,
                        total_bill)
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge,
                         new_total_ww_service_charge,
                         new_total_bill)
    
    #***************tables*************************
    #build a summary table of total bill
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Basic Sewer Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"","","",""),
                            
                            `New Usage Per Tier` = c(inputs$tier_use,"","","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","","",""),
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","","",""),
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,current_charges)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,proposed_charges)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
    #                                              "Total Bill"),
    #                          Current = paste0("$",
    #                                           formatC(as.numeric(current_charges),
    #                                                   format="f",
    #                                                   digits=2,
    #                                                   big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }
    #else
    # bill_table
    
    ##########################################Irrigation############################################################
    
  }  else if (cust_class == "Irrigation" ){
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4"),
      tier_breaks = c(0,0,0,0),
      tier_use = c(0,0,0,0),
      tier_rates = c(1.78,2.73,4.49,9.28),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0),
      tier_charge_drought =  c(0,0,0,0),
      new_tier_breaks = c(0,0,0,0),
      new_tier_use = c(0,0,0,0),
      new_tier_rates = c(1.94,3.32,5.12,9.58),
      new_tier_charge = c(0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(18.65,18.65,18.65,62.15,99.44,217.54,372.91,777.51,1118.72,1803),
      new_meter_rates = c(18.06,18.06,18.06,60.21,96.34,210.76,361.29,752.68,1083.86,
                          1746.22)
      #ww_service_charge = c(20.66,20.66,20.66,61.35,96.23,206.69,352.02,729.89,1049.61,1689.08)
    )
    ##*************************************
    
    #determine tier breaks given customer inputs
    inputs$tier_breaks[1] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$tier_breaks[2] <- round2(inputs$tier_breaks[1]*1.25,0)-round2(inputs$tier_breaks[1],0)
    inputs$tier_breaks[3] <- round2(inputs$tier_breaks[1]*1.5,0)-round2(inputs$tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(max(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[2:4])
    
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$new_tier_breaks[2] <- round2(inputs$new_tier_breaks[1]*1.25,0)-round2(inputs$new_tier_breaks[1],0)
    inputs$new_tier_breaks[3] <- round2(inputs$new_tier_breaks[1]*1.5,0)-round2(inputs$new_tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(max(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    new_usage<-sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[2:4])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:4])
    
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:4])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    #inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),4)
    p1 <- ggplot(subset(df, variable %in% c("tier_use","new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Usage by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", 
                                "new_tier_use" = "Proposed\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier
    bill_breaks <- pretty(c(1:new_use_charge),4)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier
    plegd_breaks <- c(0:5)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) +
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4"),
                        labels = c("Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #calculations
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    total_ww_service_charge = 0
    new_total_ww_service_charge = 0
    per_person_sewer_charge = 0
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge)
    current_charges = c(total_vol_charge,total_fix_charge, total_bill)
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge,
                         new_total_bill)
    
    #***************tables**************************
    #build a summary table of total bill
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"","",""),
                            
                            `New Usage Per Tier` = c(inputs$tier_use,"","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","",""),
                            
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,current_charges)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,proposed_charges)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge",
    #                                              "Total Bill"),
    #                          Current = paste0("$",
    #                                           formatC(as.numeric(current_charges),
    #                                                   format="f",
    #                                                   digits=2,
    #                                                   big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }
    #else
    # bill_table
    ##########################################Recycled############################################################  
  }  else if (cust_class == "Recycled"){
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4"),
      tier_breaks = c(0,0,0,0),
      tier_use = c(0,0,0,0),
      tier_rates = c(1.29,1.81,3.57,8.36),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      tier_rates_drought = c(0,6.55,4.79,0), #drought rates
      tier_charge =  c(0,0,0,0),
      tier_charge_drought =  c(0,0,0,0),
      new_tier_breaks = c(0,0,0,0),
      new_tier_use = c(0,0,0,0),
      new_tier_rates = c(1.39,2.62,4.42,8.88),
      new_tier_charge = c(0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4")
    
    #meter inputs
    inputs_fixed <- data.frame(
      meters = c("5/8 in.", "3/4 in.",
                 "1 in.", "1 1/2 in.",
                 "2 in.", "3 in.",
                 "4 in.", "6 in.",
                 "8 in.", "10 in."),
      meter_sizes = c(0.625, 0.75, 1, 1.5, 2,
                      3, 4, 6, 8, 10),
      meter_rates = c(18.65,18.65,18.65,62.15,99.44,217.54,372.91,777.51,1118.72,1803),
      new_meter_rates = c(18.06,18.06,18.06,60.21,96.34,210.76,361.29,752.68,1083.86,
                          1746.22)
      #ww_service_charge = c(20.66,20.66,20.66,61.35,96.23,206.69,352.02,729.89,1049.61,1689.08)
    )
    ##*************************************
    #determine tier breaks given customer inputs
    inputs$tier_breaks[1] <- round2(lot_size*0.8*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$tier_breaks[2] <- round2(inputs$tier_breaks[1]*1.25,0)-round2(inputs$tier_breaks[1],0)
    inputs$tier_breaks[3] <- round2(inputs$tier_breaks[1]*1.5,0)-round2(inputs$tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$tier_use[1] <- max(min(usage,inputs$tier_breaks[1]),0)
    inputs$tier_use[2] <- max(min(usage,sum(inputs$tier_breaks[1:2]))-inputs$tier_breaks[1],0)
    inputs$tier_use[3] <- max(min(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:2]),0)
    inputs$tier_use[4] <- max(max(usage,sum(inputs$tier_breaks[1:3]))-sum(inputs$tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    inputs$tier_use
    penalty_use <- sum(inputs$tier_use[2:4])
    
    #determine new tier breaks given customer inputs
    inputs$new_tier_breaks[1] <- round2(lot_size*0.8*evapo_trans*conv_in2gal/conv_gal2ccf,0)
    inputs$new_tier_breaks[2] <- round2(inputs$new_tier_breaks[1]*1.25,0)-round2(inputs$new_tier_breaks[1],0)
    inputs$new_tier_breaks[3] <- round2(inputs$new_tier_breaks[1]*1.5,0)-round2(inputs$new_tier_breaks[1]*1.25,0)
    
    #determine use per tier given customer usage
    inputs$new_tier_use[1] <- max(min(usage,inputs$new_tier_breaks[1]),0)
    inputs$new_tier_use[2] <- max(min(usage,sum(inputs$new_tier_breaks[1:2]))-inputs$new_tier_breaks[1],0)
    inputs$new_tier_use[3] <- max(min(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:2]),0)
    inputs$new_tier_use[4] <- max(max(usage,sum(inputs$new_tier_breaks[1:3]))-sum(inputs$new_tier_breaks[1:3]),0)
    #inputs$tier_use[5] <- max(max(usage,sum(inputs$tier_breaks[1:4]))-sum(inputs$tier_breaks[1:4]),0)
    new_usage<- sum(inputs$new_tier_use)
    new_penalty_use <- sum(inputs$new_tier_use[2:4])
    
    
    #determine charge per tier
    inputs$tier_charge[1] <- inputs$tier_use[1]*inputs$tier_rates[1]
    inputs$tier_charge[2] <- inputs$tier_use[2]*inputs$tier_rates[2]
    inputs$tier_charge[3] <- inputs$tier_use[3]*inputs$tier_rates[3]
    inputs$tier_charge[4] <- inputs$tier_use[4]*inputs$tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:4])
    
    #determine new charge per tier
    inputs$new_tier_charge[1] <- inputs$new_tier_use[1]*inputs$new_tier_rates[1]
    inputs$new_tier_charge[2] <- inputs$new_tier_use[2]*inputs$new_tier_rates[2]
    inputs$new_tier_charge[3] <- inputs$new_tier_use[3]*inputs$new_tier_rates[3]
    inputs$new_tier_charge[4] <- inputs$new_tier_use[4]*inputs$new_tier_rates[4]
    #inputs$tier_charge[5] <- inputs$tier_use[5]*inputs$tier_rates[5]
    inputs$new_tier_charge
    new_use_charge <- sum(inputs$new_tier_charge[1:4])
    
    #determine penalty
    inputs$tier_charge_drought[1] <- inputs$tier_use[1]*inputs$tier_rates_drought[1]
    inputs$tier_charge_drought[2] <- inputs$tier_use[2]*inputs$tier_rates_drought[2]
    inputs$tier_charge_drought[3] <- inputs$tier_use[3]*inputs$tier_rates_drought[3]
    inputs$tier_charge_drought[4] <- inputs$tier_use[4]*inputs$tier_rates_drought[4]
    #inputs$tier_charge_drought[5] <- inputs$tier_use[5]*inputs$tier_rates_drought[5]
    inputs$tier_charge_drought
    #penalty_charge <- sum(inputs$tier_charge_drought[1:5])
    
    total_vol_charge <- use_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    new_total_vol_charge <- new_use_charge
    #  View(inputs)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    #  View(df)
    
    #***************graphs**************************
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),4)
    p1 <- ggplot(subset(df, variable %in% c("tier_use","new_tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Usage by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage", 
                                "new_tier_use" = "Proposed\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier
    bill_breaks <- pretty(c(1:new_use_charge),4)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge","new_tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge", 
                                "new_tier_charge" = "Proposed\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier
    plegd_breaks <- c(0:5)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) +
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4"),
                        labels = c("Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #calculations
    total_fix_charge = inputs_fixed$meter_rates[inputs_fixed$meter_sizes == meter_selection]
    new_total_fix_charge = inputs_fixed$new_meter_rates[inputs_fixed$meter_sizes == meter_selection]
    total_ww_service_charge = 0
    new_total_ww_service_charge = 0
    per_person_sewer_charge = 0
    total_bill = sum(total_fix_charge, total_vol_charge,total_ww_service_charge)
    new_total_bill = sum(new_total_fix_charge, new_total_vol_charge,
                         new_total_ww_service_charge)
    current_charges = c(total_vol_charge,total_fix_charge,total_bill)
    proposed_charges = c(new_total_vol_charge,new_total_fix_charge, new_total_bill)
    
    #***************tables**************************
    #build a summary table of total bill
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Basic Water Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"","",""),
                            
                            `New Usage Per Tier` = c(inputs$new_tier_use,"","",""),
                            
                            Price = c(paste0("$",
                                             formatC(as.numeric(inputs$tier_rates),
                                                     format="f",
                                                     digits=2,
                                                     big.mark=",")
                            ),"","",""),
                            
                            `Proposed Price` = c(paste0("$",
                                                        formatC(as.numeric(inputs$new_tier_rates),
                                                                format="f",
                                                                digits=2,
                                                                big.mark=",")
                            ),"","",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,current_charges)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=",")),
                            
                            
                            `Proposed Charge` = paste0("$",
                                                       formatC(as.numeric(c(inputs$new_tier_charge,proposed_charges)),
                                                               format="f",
                                                               digits=2,
                                                               big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier (ccf)","New Usage Per Tier (ccf)","Price",
                             "Proposed Price","Current Charge","Proposed Charge")
    
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge",
    #                                              "Total Bill"),
    #                          Current = paste0("$",
    #                                           formatC(as.numeric(current_charges),
    #                                                   format="f",
    #                                                   digits=2,
    #                                                   big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
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
    }else if(tablemode == TRUE){
      vol_table
    }
    #else
    # bill_table
    
  }
  
  
  
}
#************************************* 


#Fn to Calculate and Display Current and Proposed Budgets
#************Water Budget Text********************  
fnBudget <- function(usage = 70, gpcd = 60, hh_size = 4, days = 30, conv_gal2ccf = 748,
                     lot_size = 6000, plant_factor = 0.7, evapo_trans = 3.43, conv_in2gal = 0.62,
                     meter_selection = 0.625, cust_class, typical_usage){
  
  if(cust_class == "Single Family Residential"|cust_class == "Multi Family Residential"){
    
    indoor_budget <- round(gpcd*hh_size*days/conv_gal2ccf, digits = 0)
    new_indoor_budget <- round(55*hh_size*days/conv_gal2ccf, digits = 0)
    outdoor_budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
    
    msg <- paste("Your Current Calculated Water Budget = ",
                 round(sum(indoor_budget,outdoor_budget), digits = 0)," BU and",
                 "Your Proposed Water Budget = ",
                 round(sum(new_indoor_budget,outdoor_budget), digits = 0)," BU")
    return(noquote(strsplit(msg, "\n")[[1]]))
    
    
  }else if(cust_class == "Commercial"|cust_class == "Irrigation"){
    
    budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
    
    msg <- paste("Your Current Calculated Water Budget = ",
                 round(budget, digits = 0)," BU and",
                 "Your Proposed Water Budget = ",
                 round(budget, digits = 0)," BU")
    return(noquote(strsplit(msg, "\n")[[1]]))
    
  }else if(cust_class == "Recycled"){ #Recycled Budget
    
    plant_factor <- 0.8
    
    budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
    
    msg <- paste("Your Current Calculated Water Budget = ",
                 round(budget, digits = 0)," BU and",
                 "Your Proposed Water Budget = ",
                 round(budget, digits = 0)," BU")
    return(noquote(strsplit(msg, "\n")[[1]]))
    
  }else if(cust_class == "Commercial Non-Irrigation"){ #Commercial Non-Irrigation Budget
    
    budget <- round((typical_usage+usage)/2, digits = 0)
    
    msg <- paste("Your Current Calculated Water Budget = ",
                 round(budget, digits = 0)," BU and",
                 "Your Proposed Water Budget = ",
                 round(budget, digits = 0)," BU")
    return(noquote(strsplit(msg, "\n")[[1]]))
  }
}

#************************************* 
