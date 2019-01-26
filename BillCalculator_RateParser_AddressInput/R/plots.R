

plot_usage <- function(df, num_tiers, total_usage){
  #use ggplot to create stacked bar graph of use by tier
  use_breaks <- pretty(c(1:total_usage),n = num_tiers)
  colormap <- c("blue", "green3", "yellow", "orange", "red2", "purple")
  p <- ggplot(subset(df, variable %in% c("tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
    geom_bar(stat="identity", width=0.25) +
    ggtitle("Water Usage by Tier\n") +
    xlab("") +
    ylab("Billing Units (ccf)\n") +
    scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
    scale_x_discrete(labels=c("tier_use" = "Current\n Usage"
    ))+
    scale_fill_manual(values = rev(colormap[1:num_tiers])
    ) +
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=12),
          #panel.grid.major.x = element_blank(),
          #panel.grid.minor.x = element_blank(),
          legend.position = "none")+
    guides(fill=guide_legend(title=NULL))
  p
}

plot_charges <- function(df, num_tiers, total_charge){
  #use ggplot to create a legend for plot_usage and plot_charges  
  bill_breaks <- pretty(c(1:total_charge),num_tiers)
  colormap <- c("blue", "green3", "yellow", "orange", "red2", "purple")
  p <- ggplot(subset(df, variable %in% c("tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
    geom_bar(stat="identity", width=0.25) +
    ggtitle("Water Charge by Tier\n") +
    xlab("") +
    ylab("Dollars\n") +
    scale_fill_manual(values = rev(colormap[1:num_tiers])
    ) +
    scale_y_continuous(labels = dollar_format(),
                       breaks = bill_breaks)+
    scale_x_discrete(labels=c("tier_charge" = "Current\n Charge"
    ))+
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=12),
          #panel.grid.major.x = element_blank(),
          #panel.grid.minor.x = element_blank(),
          legend.position = "none")+
    guides(fill=guide_legend(title=NULL))
  p
}

plot_legend <- function(df, num_tiers){
  #use ggplot to create stacked bar graph of charge by tier  
  plegd_breaks <- c(0:num_tiers+1)
  limitsAll <- c("1","2","3","4","5","6")
  valuesAll <- c("purple", "red2", "orange", "yellow", "green3", "blue")
  labelsAll <- c("Tier 6","Tier 5","Tier 4","Tier 3","Tier 2","Tier 1")
  p <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
    geom_bar(stat="identity", width=0) +
    xlab("") +
    ylab("") +
    scale_fill_manual(values = tail(valuesAll, n=num_tiers),
                      limits = limitsAll[1:num_tiers],
                      labels = tail(labelsAll, n=num_tiers)
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
  p
}