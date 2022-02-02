
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggExtra)
library(gtable)
library(grid)
library(gridExtra)
library(patchwork)
library(extrafont) # package to add times new roman
library(ggpattern) # package to add cross hatches to bar plot

# setwd()

# add new fonts
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()          

#####################
# Start of analysis #
#####################
# ---------------------------------------------
lit_bc <- read_xlsx("/Users/lingxiaoxiao/Desktop/PhD/Literature review/Data_Clean_bc.xlsx") # import base-case data
lit_sa <- read_xlsx("/Users/lingxiaoxiao/Desktop/PhD/Literature review/Data_Clean_sa.xlsx") # import sensitivity analysis data

# figure 1
# proportion of missing data by outcome among studies with ACA/CCA
fig1_costs_cca <- lit_bc %>% 
  filter(Basecase.missing.methods.costs.overall.details == "ACA/CCA") %>%
  filter(is.na(miss.costs.prop)==FALSE)  %>% 
  ggplot(aes(x=miss.costs.prop))+  
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.costs.prop [lit_bc$Basecase.missing.methods.costs.overall.details == "ACA/CCA"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.costs.prop[lit_bc$Basecase.missing.methods.costs.overall.details == "ACA/CCA"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing costs (CCA/ACA)",
       y="Number of studies") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618)+
  theme(axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line())+
  theme(legend.title = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman"),
        legend.position = c(0.8,0.75))+
  theme(plot.subtitle= element_text(size = 14, colour = "slategray", hjust = 0.5, family = "Times New Roman", vjust = -10),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank())+
  removeGrid()
plot(fig1_costs_cca)

fig1_utility_cca <- lit_bc %>% 
  filter(Basecase.missing.methods.utility.overall.details == "ACA/CCA") %>% 
  filter(is.na(miss.utility.prop)==FALSE) %>%
  ggplot(aes(x=miss.utility.prop))+
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.utility.prop [lit_bc$Basecase.missing.methods.utility.overall.details == "ACA/CCA"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.utility.prop[lit_bc$Basecase.missing.methods.utility.overall.details == "ACA/CCA"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing QoL (CCA/ACA)", 
       y="Number of studies") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618)+
  theme(axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line())+
  theme(legend.title = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman"),
        legend.position = c(0.8,0.75))+
  theme(plot.subtitle= element_text(size = 14, colour = "slategray", hjust = 0.5, family = "Times New Roman", vjust = -10),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank())+
  removeGrid()
plot(fig1_utility_cca)


# proportion of missing data by outcome among studies with MI
fig1_costs_mi <- lit_bc %>% 
  filter(Basecase.missing.methods.costs.overall.details == "Multiple imputation") %>%
  filter(is.na(miss.costs.prop)==FALSE)  %>% 
  ggplot(aes(x=miss.costs.prop, pattern=Basecase.missing.methods.costs.overall.details))+
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.costs.prop [lit_bc$Basecase.missing.methods.costs.overall.details == "Multiple imputation"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.costs.prop[lit_bc$Basecase.missing.methods.costs.overall.details == "Multiple imputation"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing costs (MI)", 
       y="Number of studies")+
  scale_color_identity()+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618,
        axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line(),
        legend.title = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman"),
        legend.position = c(0.8,0.75),
        plot.subtitle= element_text(size = 14, colour = "slategray", hjust = 0.5, vjust = -10, family = "Times New Roman"),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank()) +
  removeGrid()
plot(fig1_costs_mi)  

fig1_utility_mi <- lit_bc %>%
  filter(Basecase.missing.methods.utility.overall.details=="Multiple imputation") %>%
  filter(is.na(miss.utility.prop)==FALSE)  %>%
  ggplot(aes(x=miss.utility.prop, pattern=Basecase.missing.methods.utility.overall.details))+
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.utility.prop [lit_bc$Basecase.missing.methods.utility.overall.details == "Multiple imputation"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.utility.prop[lit_bc$Basecase.missing.methods.utility.overall.details == "Multiple imputation"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing QoL (MI)", 
       y="Number of studies")+
  scale_color_identity()+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618)+
  theme(axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line())+
  theme(legend.title = element_text(color = "slategray", size = 12, family = "Times New Roman", face = "bold"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman", margin = margin(l=3, unit = "pt")),
        legend.margin = margin(t=1, b=1, l=3, r=2, unit = "line"),
        legend.position = "bottom")+
  theme(plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", hjust = 0.5, vjust = -10),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank())+
  removeGrid()
plot(fig1_utility_mi)  


# proportion of missing data by outcome among studies with composite methods
fig1_costs_composite <- lit_bc %>% 
  filter(Basecase.missing.methods.costs.overall.details=="Composite methods") %>%
  filter(is.na(miss.costs.prop)==FALSE)  %>%
  ggplot(aes(x=miss.costs.prop, pattern=Basecase.missing.methods.costs.overall.details))+
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.costs.prop [lit_bc$Basecase.missing.methods.costs.overall.details == "Composite methods"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.costs.prop[lit_bc$Basecase.missing.methods.costs.overall.details == "Composite methods"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing costs (Composite)", 
       y="Number of studies")+
  scale_color_identity()+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618,
        axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line(),
        legend.title = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman"),
        legend.position = c(0.8,0.75),
        plot.subtitle= element_text(size = 14, colour = "slategray", hjust = 0.5, vjust = -10, family = "Times New Roman"),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank()) +
  removeGrid()
plot(fig1_costs_composite)  

fig1_utility_composite <- lit_bc %>% 
  filter(Basecase.missing.methods.utility.overall.details=="Composite methods") %>%
  filter(is.na(miss.utility.prop)==FALSE)  %>%
  ggplot(aes(x=miss.utility.prop, pattern=Basecase.missing.methods.utility.overall.details))+
  geom_histogram(binwidth=0.05, color="slategray", alpha=0.8, fill="gray") +    
  labs(subtitle = paste("    n =", sum(is.na(lit_bc$miss.utility.prop [lit_bc$Basecase.missing.methods.utility.overall.details == "Composite methods"])==FALSE), 
                        "\nmedian =", sprintf("%1.1f%%", 100*median(lit_bc$miss.utility.prop[lit_bc$Basecase.missing.methods.utility.overall.details == "Composite methods"], na.rm = TRUE)), sep = ""),
       x="Proportion of missing QoL (Composite)", 
       y="Number of studies")+
  scale_color_identity()+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits = c(0,6), breaks = seq(0,5.5,1))+
  coord_cartesian(xlim = c(-0.05,1))+
  theme(aspect.ratio = 0.618)+
  theme(axis.text    = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.title   = element_text(color = "slategray", size = 14, family = "Times New Roman"),
        axis.line    = element_line())+
  theme(legend.title = element_text(color = "slategray", size = 12, family = "Times New Roman", face = "bold"),
        legend.text  = element_text(color = "slategray", size = 12, family = "Times New Roman", margin = margin(l=3, unit = "pt")),
        legend.margin = margin(t=1, b=1, l=3, r=2, unit = "line"),
        legend.position = "bottom")+
  theme(plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", hjust = 0.5, vjust = -10),
        plot.margin  = unit(c(2,1,2,1),"line"),
        panel.background = element_blank())+
  removeGrid()
plot(fig1_utility_composite)  


# to get everything together
fig1 <- ggarrange(fig1_costs_cca,   fig1_costs_mi,   fig1_costs_composite,
                  fig1_utility_cca, fig1_utility_mi, fig1_utility_composite,
                  nrow = 2, ncol = 3, align = "hv")
plot(fig1)


# ---------------------------------------------
# figure 2
# cost- and utility-specific base-case analysis 
## main plot
fig2_bc_CvU_main <- lit_bc %>% 
  ggplot(aes(x=Basecase.missing.methods.costs.overall.details, y=Basecase.missing.methods.utility.overall.details)) +
  geom_count(col="slategray", fill=alpha("gray",0.5), shape=21, show.legend = FALSE, stroke=1) +
  scale_size(range = c(5, 19.4), name="Count") + theme_bw() +
  theme_bw()+
  theme(axis.title.x.bottom = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=1, r=0, b=0, l=0, unit = "lines")),
        axis.title.y.left   = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=0, r=1, b=0, l=0, unit = "lines")),
        axis.text     = element_text(color = "slategray", size = 15.5, family = "Times New Roman"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.margin   = unit(c(0,2,0,2),"lines"),
        plot.subtitle = element_text(color = "white", size = 15.5, family = "Times New Roman", face = "bold", vjust = -1)) +
  scale_color_identity()+
  scale_x_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM", "Single imputation"="SI", "Unspecified imputation"="UI","Unclear"="Unclear"),
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","ACA/CCA"))+
  scale_y_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM", "Single imputation"="SI", "Unspecified imputation"="UI","Unclear"="Unclear"),
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","ACA/CCA"))+
  coord_fixed(ratio = 1)+
  labs(x="Costs-specific base-case analysis methods", y="QoL-specific base-case analysis methods", subtitle = "(2)")
fig2_bc_CvU_main <- fig2_bc_CvU_main + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", size = 0.5, color = "brown") +
  geom_text(data = ggplot_build(fig2_bc_CvU_main)$data[[1]], 
            aes(x,y,label = n), color=alpha("slategray",1), size=4.5, #fontface="bold",
            family="Times New Roman")
grid.draw(fig2_bc_CvU_main)

## costs marginal plot
fig2_bc_CvU_margin_c <- ggplot(lit_bc,aes(x=Basecase.missing.methods.costs.overall.details)) +
  geom_bar(col="slategray", fill=alpha("gray",0.5), width = 0.3, size=0.5) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4.5, color="slategray",family="Times New Roman") +
  scale_x_discrete(limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","ACA/CCA"))+
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,20),expand = expansion(c(0,.1)))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text  = element_blank(), legend.position = "none",
        plot.subtitle= element_text(size = 14, colour = "slategray", face = "bold", family = "Times New Roman", vjust = -1),
        plot.margin  = unit(c(2,1,1,1),"lines"),
        panel.grid = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
plot(fig2_bc_CvU_margin_c)

## utility marginal plot
fig2_bc_CvU_margin_u <- ggplot(lit_bc,aes(x=Basecase.missing.methods.utility.overall.details)) +
  geom_bar(col="slategray", fill=alpha("gray",0.5), width = 0.3, size=0.5) + 
  geom_text(stat='count', aes(label=..count..),hjust = -0.5,size=4.5, color="slategray",family = "Times New Roman") +
  scale_x_discrete(limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","ACA/CCA"))+
  scale_y_continuous(limits =c(0,50), breaks = seq(0,50,20),expand = expansion(c(0,.1)))+
  theme_bw()+
  theme(axis.text  = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), legend.position = "none",
        plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", face = "bold", vjust = -1),
        plot.margin = unit(c(0,1,0,0),"line"),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_flip() 
plot(fig2_bc_CvU_margin_u)

## create a function to put everything together
scatter_comparison <- function(scatter, margin.top, margin.right, title){
  scatter <- ggplotGrob(scatter)
  layout  <- scatter$layout
  # add marginal plot on the top
  scatter <- gtable_add_rows(scatter, unit(0.2,"npc"), pos = 0)
  layout  <- scatter$layout
  margin.top.t <- 1
  margin.top.b <- layout$t[layout$name=="tag"]-1
  margin.top.l <- layout$l[layout$name=="panel"]
  margin.top.r <- layout$r[layout$name=="panel"]
  scatter <- gtable_add_grob(scatter, ggplotGrob(margin.top)$grobs[[6]],
                             t=margin.top.t, l=margin.top.l, b=margin.top.b, r=margin.top.r)
  # extract layout inf
  scatter <- gtable_add_cols(scatter, unit(0.2, "npc"))
  layout  <- scatter$layout
  # add marginal plot on the right
  margin.right.t <- layout$t[layout$name=="panel"]
  margin.right.b <- layout$b[layout$name=="panel"]
  margin.right.l <- ncol(scatter)
  margin.right.r <- ncol(scatter)
  scatter <- gtable_add_grob(scatter, ggplotGrob(margin.right)$grobs[[6]],
                             t=margin.right.t, l=margin.right.l, b=margin.right.b, r=margin.right.r)
  
  title   <- textGrob(title, gp=gpar(col="slategray", fontsize=16, fontfamily="Times New Roman", fontface="bold"))
  scatter <- gtable_add_rows(scatter, heights=grobHeight(title)+unit(1, "line"), pos = 0)
  scatter <- gtable_add_grob(scatter, title, t=1, l=1, r=ncol(scatter))
}


fig2_bc_CvU <- scatter_comparison(scatter = fig2_bc_CvU_main,
                                  margin.top = fig2_bc_CvU_margin_c,
                                  margin.right = fig2_bc_CvU_margin_u,
                                  title = "Costs vs QoL in base-case analysis")
grid.newpage()
grid.draw(fig2_bc_CvU)


# costs- and utility-specific sa
## main plot
fig2_sa_CvU_main <- lit_sa %>% 
  ggplot(aes(x = SA.missing.methods.costs.overall.details, y = SA.missing.methods.utility.overall.details)) +
  geom_count(col="slategray", fill=alpha("gray",0.5), shape=21, show.legend = FALSE, stroke=1) +
  scale_size(range = c(5, 23), name="Count") + theme_bw() +
  scale_x_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","No"="No","NA"="No SA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"))+
  scale_y_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","No"="No","NA"="No SA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"))+
  theme_bw() +
  theme(axis.title.x.bottom = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=1, r=0, b=0, l=0, unit = "lines")),
        axis.title.y.left   = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=0, r=1, b=0, l=0, unit = "lines")),
        axis.text      = element_text(color = "slategray", size = 15.5, family = "Times New Roman"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.margin   = unit(c(0,2,0,2),"lines"),
        plot.subtitle = element_text(color = "white", size = 15.5, family = "Times New Roman", face = "bold", vjust = -1)) +
  scale_color_identity()+
  coord_fixed(ratio = 1)+
  labs(subtitle = "(2)", x="Costs-specific sensitivity analysis methods", y="QoL-specific sensitivity analysis methods")
fig2_sa_CvU_main <- fig2_sa_CvU_main + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", size = 0.5, color = "brown") +
  geom_text(data = ggplot_build(fig2_sa_CvU_main)$data[[1]],
            aes(x,y,label = n), color="slategray", size=4.5, #fontface="bold",
            family="Times New Roman")
grid.draw(fig2_sa_CvU_main)

## costs marginal plot
fig2_sa_CvU_margin_c <- ggplot(lit_sa, aes(x=SA.missing.methods.costs.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4.5, color="slategray",family="Times New Roman") +
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,20), expand = expansion(c(0,.1))) +
  scale_x_discrete(position = "top",limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA")) + 
  labs(subtitle = "(1)") +
  theme_bw()+
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text  = element_blank(), legend.position = "none",
        plot.subtitle= element_text(size = 14, colour = "slategray", face = "bold", family = "Times New Roman", vjust = -1),
        plot.margin  = unit(c(0,1,0,1),"lines"),
        panel.grid = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
plot(fig2_sa_CvU_margin_c)

## utility marginal plot
fig2_sa_CvU_margin_u  <- ggplot(lit_sa, aes(x=SA.missing.methods.utility.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..),hjust = -0.5,size=4.5, color="slategray",family = "Times New Roman") +
  scale_x_discrete(position = "top",limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA")) + 
  labs(subtitle = "(3)")+
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,20), expand = expansion(c(0,.1))) +
  theme_bw()+
  theme(axis.text  = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), legend.position = "none",
        plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", face = "bold", vjust = -1),
        plot.margin = unit(c(0,0,0,0),"lines"),
        panel.grid  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_flip() 
plot(fig2_sa_CvU_margin_u)

# put everything together               
fig2_sa_CvU <- scatter_comparison(scatter = fig2_sa_CvU_main,
                                  margin.top = fig2_sa_CvU_margin_c,
                                  margin.right = fig2_sa_CvU_margin_u,
                                  title = "Costs vs QoL in sensitivity analysis")
grid.newpage()
grid.draw(fig2_sa_CvU)


# costs-specific basecase versus sensitivity analysis
## main plot
fig2_bcVsa_c_main <- lit_sa %>% 
  ggplot(aes(x=Basecase.missing.methods.costs.overall.details, y=SA.missing.methods.costs.overall.details)) +
  geom_count(col="slategray", fill=alpha("gray",0.5), shape=21, show.legend = FALSE, stroke=1) +
  scale_size_continuous(range = c(5,15.2), name="Count") + theme_bw() +
  theme(axis.title.x.bottom = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=1, r=0, b=0, l=0, unit = "lines")),
        axis.title.y.left   = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=0, r=1, b=0, l=0, unit = "lines")),
        axis.text     = element_text(color = "slategray", size = 15.5, family = "Times New Roman"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.margin   = unit(c(0,2,0,2),"lines"),
        plot.subtitle = element_text(color = "white", size = 15.5, family = "Times New Roman", face = "bold", vjust = -1)) +
  scale_color_identity()+
  scale_x_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","Unclear"="Unclear","NA"="NA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","NA"))+
  scale_y_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","No"="No","NA"="No SA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"))+
  labs(subtitle = "(2)", x="Costs-specific base-case analysis methods", y="Costs-specific sensitivity analysis methods") +
  coord_fixed(ratio = 1)
fig2_bcVsa_c_main <- fig2_bcVsa_c_main + 
  geom_text(data = ggplot_build(fig2_bcVsa_c_main)$data[[1]],
            aes(x,y,label = n), color=alpha("slategray",1), size=4.5, #fontface="bold",
            family="Times New Roman")
grid.draw(fig2_bcVsa_c_main)

## costs marginal plot
fig2_bcVsa_c_margin_bc <- ggplot(lit_sa,aes(x=Basecase.missing.methods.costs.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4.5, color="slategray",family="Times New Roman") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,20), expand = expansion(c(0,.1))) +
  labs(subtitle = "(1)") +
  scale_x_discrete(labels=c("ACA/CCA"="CCA/ACA","Multiple imputation"="MI","Composite methods"="Composite","Single imputation"="Single","Unspecified imputation"="Unspecified","Unclear"="Unclear","NA"="NA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","NA"), position = "top")+
  theme_bw()+
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text  = element_blank(), legend.position = "none",
        plot.subtitle= element_text(size = 14, colour = "slategray", face = "bold", family = "Times New Roman", vjust = -1),
        plot.margin  = unit(c(0,1,0,1),"lines"),  
        panel.grid   = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
plot(fig2_bcVsa_c_margin_bc)

## utility marginal plot
fig2_bcVsa_c_margin_sa <- ggplot(lit_sa,aes(x=SA.missing.methods.costs.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..),hjust = -0.5,size=4.5, color="slategray",family = "Times New Roman") +
  scale_x_discrete(labels=c("ACA/CCA"="CCA/ACA","Multiple imputation"="MI","Composite methods"="Composite","Single imputation"="Single","Unspecified imputation"="Unspecified","No"="No","NA"="No SA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"), position = "top")+
  scale_y_continuous(limits = c(0,50),breaks = seq(0,50,20), expand = expansion(c(0,.1))) +
  labs(subtitle = "(3)")+
  theme_bw()+
  theme(axis.text  = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), legend.position = "none",
        plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", face = "bold", vjust = -1),
        plot.margin = unit(c(0,0,0,0),"lines"),
        panel.grid   = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_flip() 
plot(fig2_bcVsa_c_margin_sa)

fig2_bcVsa_c <- scatter_comparison(scatter = fig2_bcVsa_c_main,
                                   margin.top = fig2_bcVsa_c_margin_bc,
                                   margin.right = fig2_bcVsa_c_margin_sa,
                                   title = "Costs-specific base-case vs sensitivity analysis methods")
grid.newpage()
grid.draw(fig2_bcVsa_c)


# utility-specific basecase and SA
## main plot
fig2_bcVsa_u_main <- lit_sa %>% 
  ggplot(aes(x=Basecase.missing.methods.utility.overall.details, y=SA.missing.methods.utility.overall.details))+
  geom_count(col="slategray", fill=alpha("gray",0.5), shape=21, show.legend = FALSE, stroke=1) +
  scale_size(range = c(5, 21.5), name="Count") + theme_bw() +
  theme(axis.title.x.bottom = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=1, r=0, b=0, l=0, unit = "lines")),
        axis.title.y.left   = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=0, r=1, b=0, l=0, unit = "lines")),
        axis.text     = element_text(color = "slategray", size = 15.5, family = "Times New Roman"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.margin   = unit(c(0,2,0,2),"lines"),
        plot.subtitle = element_text(color = "white", size = 15.5, family = "Times New Roman", face = "bold", vjust = -1)) +
  scale_color_identity()+
  scale_x_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","Unclear"="Unclear","NA"="NA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","NA"))+
  scale_y_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="CM","Single imputation"="SI","Unspecified imputation"="UI","Unclear"="Unclear","NA"="NA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"))+
  labs(subtitle = "(2)", x="QoL-specific base-case analysis methods", y="QoL-specific sensitivity analysis methods") +
  coord_fixed(ratio = 1)
fig2_bcVsa_u_main <- fig2_bcVsa_u_main + 
  geom_text(data = ggplot_build(fig2_bcVsa_u_main)$data[[1]],
            aes(x,y,label = n), color=alpha("slategray",1), size=4.5, #fontface="bold",
            family="Times New Roman")
grid.draw(fig2_bcVsa_u_main)

## costs marginal plot
fig2_bcVsa_u_margin_bc <- ggplot(lit_sa, aes(x=Basecase.missing.methods.utility.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4.5, color="slategray",family="Times New Roman") +
  scale_y_continuous(limits = c(0,50),breaks = seq(0,50,20), expand = expansion(c(0,.1)))+
  labs(subtitle = "(1)") +
  scale_x_discrete(labels=c("ACA/CCA"="C/A","Multiple imputation"="MI","Composite methods"="Composite","Single imputation"="Single","Unspecified imputation"="Unspecified","Unclear"="Unclear","NA"="NA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","Unclear","NA"), position = "top")+
  theme_bw()+
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text  = element_blank(), legend.position = "none",
        plot.subtitle= element_text(size = 14, colour = "slategray", face = "bold", family = "Times New Roman", vjust = -1),
        plot.margin  = unit(c(0,1,0,1),"lines"),   
        panel.grid = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
plot(fig2_bcVsa_u_margin_bc)

## utility marginal plot
fig2_bcVsa_u_margin_sa <- ggplot(lit_sa, aes(x=SA.missing.methods.utility.overall.details)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..),hjust = -0.5,size=4.5, color="slategray",family = "Times New Roman") +
  scale_x_discrete(labels=c("ACA/CCA"="CCA/ACA","Multiple imputation"="MI","Composite methods"="Composite","Single imputation"="Single","Unspecified imputation"="Unspecified","No"="No","NA"="No SA"), 
                   limits=c("ACA/CCA","Multiple imputation","Composite methods","Single imputation","Unspecified imputation","No","NA"), position = "top")+
  scale_y_continuous(limits = c(0,50),breaks = seq(0,50,20), expand = expansion(c(0,.1))) +
  labs(subtitle = "(3)")+
  theme_bw()+
  theme(axis.text  = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), legend.position = "none",
        plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", face = "bold", vjust = -1),
        plot.margin = unit(c(0,2,0,0),"lines"),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_flip() 
plot(fig2_bcVsa_u_margin_sa)

fig2_bcVsa_u <- scatter_comparison(scatter = fig2_bcVsa_u_main,
                                   margin.top = fig2_bcVsa_u_margin_bc,
                                   margin.right = fig2_bcVsa_u_margin_sa, 
                                   title = "QoL-specific base-case vs sensitivity analysis methods")
grid.newpage()
grid.draw(fig2_bcVsa_u)


# make the final 2x2 plot 
grid.newpage()
fig_2 <- ggarrange(fig2_bc_CvU,  fig2_sa_CvU, fig2_bcVsa_c, fig2_bcVsa_u,
                   nrow = 2, ncol = 2, heights = c(1,1), widths = c(1,1), align = "hv")
grid.draw(fig_2)


# ---------------------------------------------
# figure 3
## prepare the data with a larger sample size: bring base-case and SA together
### base-case analysis
lit.missing.bc <- lit_bc %>% select(`Study ID`,Basecase.missing.methods.overall,
                                    Basecase.missing.methods.costs.overall,   Basecase.missing.methods.costs.overall.details,   Basecase.missing.methods.costs.total,   Basecase.missing.methods.costs.points,   Basecase.missing.methods.costs.items, 
                                    Basecase.missing.methods.utility.overall, Basecase.missing.methods.utility.overall.details, Basecase.missing.methods.utility.QALYs, Basecase.missing.methods.utility.points, Basecase.missing.methods.utility.items)
names(lit.missing.bc) <- gsub("Basecase.", "", names(lit.missing.bc))
lit.missing.bc$bcsa   <- "bc"  # create a var to tell whether the info is related to base-case analysis (bc) or sensitivity analysis (sa)

### sensitivity analysis
lit.missing.sa <- lit_sa %>% select(`Study ID`,SA.missing.methods.overall,
                                    SA.missing.methods.costs.overall,   SA.missing.methods.costs.overall.details,   SA.missing.methods.costs.total,   SA.missing.methods.costs.points,   SA.missing.methods.costs.items, 
                                    SA.missing.methods.utility.overall, SA.missing.methods.utility.overall.details, SA.missing.methods.utility.QALYs, SA.missing.methods.utility.points, SA.missing.methods.utility.items)
names(lit.missing.sa) <- gsub("SA.","", names(lit.missing.sa))
lit.missing.sa$bcsa   <- "sa"

### create a new dataframe with missing data information at different levels in both bc and sa
lit.missing <- rbind(lit.missing.bc, lit.missing.sa)
table(lit.missing$bcsa)

### create var for the level at which imputation was performed 
table(lit.missing$missing.methods.costs.total)
lit.missing$aggrlvl.costs.total  <- "No"
lit.missing$aggrlvl.costs.total[lit.missing$missing.methods.costs.total =="Single imputation"]      <- "Yes"
lit.missing$aggrlvl.costs.total[lit.missing$missing.methods.costs.total =="Multiple imputation"]    <- "Yes"
lit.missing$aggrlvl.costs.total[lit.missing$missing.methods.costs.total =="Composite methods"]      <- "Yes"
lit.missing$aggrlvl.costs.total[lit.missing$missing.methods.costs.total =="Unspecified imputation"] <- "Yes"
table(lit.missing$aggrlvl.costs.total)

table(lit.missing$missing.methods.costs.points )
lit.missing$aggrlvl.costs.points  <- "No"
lit.missing$aggrlvl.costs.points[lit.missing$missing.methods.costs.points =="Single imputation"]       <- "Yes"
lit.missing$aggrlvl.costs.points[lit.missing$missing.methods.costs.points =="Multiple imputation"]     <- "Yes"
lit.missing$aggrlvl.costs.points[lit.missing$missing.methods.costs.points =="Composite methods"]       <- "Yes"
lit.missing$aggrlvl.costs.points[lit.missing$missing.methods.costs.points =="Unspecified imputation"]  <- "Yes"
table(lit.missing$aggrlvl.costs.points)

table(lit.missing$missing.methods.costs.items )
lit.missing$aggrlvl.costs.items  <- "No"
lit.missing$aggrlvl.costs.items[lit.missing$missing.methods.costs.items =="Single imputation"]       <- "Yes"
lit.missing$aggrlvl.costs.items[lit.missing$missing.methods.costs.items =="Multiple imputation"]     <- "Yes"
lit.missing$aggrlvl.costs.items[lit.missing$missing.methods.costs.items =="Composite methods"]       <- "Yes"
lit.missing$aggrlvl.costs.items[lit.missing$missing.methods.costs.items =="Unspecified imputation"]  <- "Yes"
table(lit.missing$aggrlvl.costs.items)

table(lit.missing$missing.methods.utility.QALYs )
lit.missing$aggrlvl.utility.total  <-"No"
lit.missing$aggrlvl.utility.total[lit.missing$missing.methods.utility.QALYs =="Single imputation"]      <- "Yes"
lit.missing$aggrlvl.utility.total[lit.missing$missing.methods.utility.QALYs =="Multiple imputation"]    <- "Yes"
lit.missing$aggrlvl.utility.total[lit.missing$missing.methods.utility.QALYs =="Composite methods"]      <- "Yes"
lit.missing$aggrlvl.utility.total[lit.missing$missing.methods.utility.QALYs =="Unspecified imputation"] <- "Yes"
table(lit.missing$aggrlvl.utility.total)

table(lit.missing$missing.methods.utility.points )
lit.missing$aggrlvl.utility.points  <- "No"
lit.missing$aggrlvl.utility.points[lit.missing$missing.methods.utility.points =="Single imputation"]      <- "Yes"
lit.missing$aggrlvl.utility.points[lit.missing$missing.methods.utility.points =="Multiple imputation"]    <- "Yes"
lit.missing$aggrlvl.utility.points[lit.missing$missing.methods.utility.points =="Composite methods"]      <- "Yes"
lit.missing$aggrlvl.utility.points[lit.missing$missing.methods.utility.points =="Unspecified imputation"] <- "Yes"
table(lit.missing$aggrlvl.utility.points)

table(lit.missing$missing.methods.utility.items )
lit.missing$aggrlvl.utility.items  <- "No"
lit.missing$aggrlvl.utility.items[lit.missing$missing.methods.utility.items =="Single imputation"]      <- "Yes"
lit.missing$aggrlvl.utility.items[lit.missing$missing.methods.utility.items =="Multiple imputation"]    <- "Yes"
lit.missing$aggrlvl.utility.items[lit.missing$missing.methods.utility.items =="Composite methods"]      <- "Yes"
lit.missing$aggrlvl.utility.items[lit.missing$missing.methods.utility.items =="Unspecified imputation"] <- "Yes"
table(lit.missing$aggrlvl.utility.items)

lit.missing$aggrlvl.costs <- 1
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "Yes"& lit.missing$aggrlvl.costs.points == "No" & lit.missing$aggrlvl.costs.items == "No"]  <- "Total"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "No" & lit.missing$aggrlvl.costs.points == "Yes"& lit.missing$aggrlvl.costs.items == "No"]  <- "Time points"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "No" & lit.missing$aggrlvl.costs.points == "No" & lit.missing$aggrlvl.costs.items == "Yes"] <- "Items"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "Yes"& lit.missing$aggrlvl.costs.points == "Yes"& lit.missing$aggrlvl.costs.items == "No"]  <- "Multiple levels"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "Yes"& lit.missing$aggrlvl.costs.points == "No" & lit.missing$aggrlvl.costs.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "No" & lit.missing$aggrlvl.costs.points == "Yes"& lit.missing$aggrlvl.costs.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "Yes"& lit.missing$aggrlvl.costs.points == "Yes"& lit.missing$aggrlvl.costs.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.costs[lit.missing$aggrlvl.costs.total == "No" & lit.missing$aggrlvl.costs.points == "No" & lit.missing$aggrlvl.costs.items == "No"]  <- "NA"
table(lit.missing$aggrlvl.costs)

lit.missing$aggrlvl.utility <- 1
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "Yes"& lit.missing$aggrlvl.utility.points == "No" & lit.missing$aggrlvl.utility.items == "No"]  <- "Total"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "No" & lit.missing$aggrlvl.utility.points == "Yes"& lit.missing$aggrlvl.utility.items == "No"]  <- "Time points"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "No" & lit.missing$aggrlvl.utility.points == "No" & lit.missing$aggrlvl.utility.items == "Yes"] <- "Items"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "Yes"& lit.missing$aggrlvl.utility.points == "Yes"& lit.missing$aggrlvl.utility.items == "No"]  <- "Multiple levels"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "Yes"& lit.missing$aggrlvl.utility.points == "No" & lit.missing$aggrlvl.utility.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "No" & lit.missing$aggrlvl.utility.points == "Yes"& lit.missing$aggrlvl.utility.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "Yes"& lit.missing$aggrlvl.utility.points == "Yes"& lit.missing$aggrlvl.utility.items == "Yes"] <- "Multiple levels"
lit.missing$aggrlvl.utility[lit.missing$aggrlvl.utility.total == "No" & lit.missing$aggrlvl.utility.points == "No" & lit.missing$aggrlvl.utility.items == "No"]  <- "NA"
table(lit.missing$aggrlvl.utility)


## main plot
fig3_main <- lit.missing %>% 
  filter(aggrlvl.utility!="NA" | aggrlvl.costs!="NA") %>%
  ggplot(aes(x=aggrlvl.costs, y=aggrlvl.utility)) +
  geom_count(col="slategray", fill=alpha("gray",0.5), shape=21, show.legend = FALSE, stroke=1) +
  scale_x_discrete(limits=c("Total","Time points","Items","Multiple levels","NA"),
                   labels=c("Total"="Total","Time points"="Time points","Items"="Items","Multiple levels"="Multiple","NA"="NA")) + 
  scale_y_discrete(limits=c("Total","Time points","Items","Multiple levels","NA"),
                   labels=c("Total"="Total","Time points"="Time points","Items"="Items","Multiple levels"="Multiple","NA"="NA")) + 
  scale_size_continuous(range = c(5,27), name="Count") + theme_bw() +
  theme(axis.title.x.bottom = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=1, r=0, b=0, l=0, unit = "lines")),
        axis.title.y.left   = element_text(color = "slategray", size = 15.5, family = "Times New Roman", margin = margin(t=0, r=1, b=0, l=0, unit = "lines")),
        axis.text     = element_text(color = "slategray", size = 15.5, family = "Times New Roman"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.margin   = unit(c(0,2,0,2),"lines"),
        plot.subtitle = element_text(color = "white", size = 15.5, family = "Times New Roman", face = "bold", vjust = -1)) +
  scale_color_identity()+
  labs(x="Aggregation level of costs", y="Aggregation level of QoL", subtitle = "(2)") +
  coord_fixed(ratio = 1)
fig3_main <- fig3_main + 
  geom_text(data = ggplot_build(fig3_main)$data[[1]],
            aes(x,y,label = n), color=alpha("slategray",1), size=4.5, #fontface="bold",
            family="Times New Roman")
grid.draw(fig3_main)

## marginal plot for the aggregation level of costs
fig3_margin_c <- lit.missing %>%
  filter(aggrlvl.utility!="NA" | aggrlvl.costs!="NA") %>%
  ggplot(aes(x=aggrlvl.costs)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4.5, color="slategray",family="Times New Roman") +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,55,10), expand = expansion(c(0,.1))) +
  scale_x_discrete(limits=c("Total","Time points","Items","Multiple levels","NA"),
                   labels=c("Total"="Total","Time points"="Time points","Items"="Items","Multiple levels"="Multiple","NA"="NA")) + 
  theme_bw()+
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text  = element_blank(), legend.position = "none",
        plot.subtitle= element_text(size = 14, colour = "slategray", face = "bold", family = "Times New Roman", vjust = -1),
        plot.margin  = unit(c(0,1,0,1),"lines"),  
        panel.grid   = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
plot(fig3_margin_c)

## marginal plot for the aggregation level of utility
fig3_margin_u <- lit.missing %>%
  filter(aggrlvl.utility!="NA" | aggrlvl.costs!="NA") %>%
  ggplot(aes(x=aggrlvl.utility)) +
  geom_bar(col="slategray",fill=alpha("gray",0.5), width = 0.3) +
  geom_text(stat='count', aes(label=..count..),hjust = -0.5,size=4.5, color="slategray",family = "Times New Roman") +
  scale_x_discrete(limits=c("Total","Time points","Items","Multiple levels","NA"),
                   labels=c("Total"="Total","Time points"="Time points","Items"="Items","Multiple levels"="Multiple","NA"="NA")) + 
  scale_y_continuous(limits = c(0,55),breaks = seq(0,55,10), expand = expansion(c(0,.1))) +
  theme_bw()+
  theme(axis.text  = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), legend.position = "none",
        plot.subtitle = element_text(color = "slategray", size = 14, family = "Times New Roman", face = "bold", vjust = -1),
        plot.margin = unit(c(0,0,0,0),"lines"),
        panel.grid   = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_flip() 
plot(fig3_margin_u)

fig3 <- scatter_comparison(scatter = fig3_main,
                           margin.top = fig3_margin_c,
                           margin.right = fig3_margin_u,
                           title = "Aggregation level of costs vs QoL")
grid.newpage()
grid.draw(fig3)


# ------------------------------------------------------------------------------------------------------------
# figure 4

# preparation
## we need to first figure out how many studies were included for the imputation at different levels
## Relevant studies: 
## (1) whose overall missing data method (missing.methods.overall) was imputation
## (2) whose overall missing data method (missing.methods.overall) was CCA/ACA but they use some imputation techniques before CCA/ACA

## to find 2nd type of studies:
## note that those "consistent with base case" are not of our interest because they were created for SA and provided the same info as their base-case analyses which have been saved in the corresponding base-case records
lit.missing.cca <- lit.missing %>% filter(missing.methods.overall == "Complete cases" | missing.methods.overall == "Available cases")

table(lit.missing.cca$missing.methods.costs.total)
table(lit.missing.cca$missing.methods.costs.points)
table(lit.missing.cca$missing.methods.costs.items)
head(lit.missing.cca$`Study ID`[lit.missing.cca$missing.methods.costs.items=="Single imputation"]) # "Brown 2018"  "Heller 2017" "Luyt 2019"  
head(lit.missing.cca$`Study ID`[lit.missing.cca$missing.methods.costs.items=="Composite methods"]) # "Glazener 2016 (1)" "Glazener 2016 (2)""

table(lit.missing.cca$missing.methods.utility.QALYs)
table(lit.missing.cca$missing.methods.utility.points)
table(lit.missing.cca$missing.methods.utility.items)
head(lit.missing.cca$`Study ID`[lit.missing.cca$missing.methods.utility.points=="Composite methods"]) # "Heller 2017" "Parry 2016" 
## so there are 5 studies for costs and 2 studies for utility who claimed that their missing data methods were CCA/ACA but performed some kind of imputation before applying CCA/ACA

## create a data.frame of studies that performed some kind of imputation
lit.missing.imp <- lit.missing %>% filter(`Study ID`=="Brown 2018" | `Study ID`=="Heller 2017" | `Study ID`=="Luyt 2019" | `Study ID`=="Glazener 2016 (1)" | 
                                            `Study ID`=="Glazener 2016 (2)" | `Study ID`=="Parry 2016" | missing.methods.overall=="Imputation")

## since we hope to present imputation methods at different levels for costs and utility in one plot...
lit.missing.costs <- lit.missing.imp %>% select(`Study ID`, missing.methods.overall, missing.methods.costs.overall.details, bcsa, 
                                                missing.methods.costs.total, missing.methods.costs.points, missing.methods.costs.items)
lit.missing.costs$c_or_u <- "costs"  # to distinguish whether it is missing data method for costs or utility
names(lit.missing.costs) <- gsub("costs.","", names(lit.missing.costs)) # change var names such that the two data.frames could have identical var names

lit.missing.utility <- lit.missing.imp %>% select(`Study ID`, missing.methods.overall, missing.methods.utility.overall.details, bcsa,
                                                  missing.methods.utility.QALYs, missing.methods.utility.points, missing.methods.utility.items)
lit.missing.utility$c_or_u <- "utility" # to distinguish whether it is missing data method for costs or utility
names(lit.missing.utility) <- gsub("utility.","",      names(lit.missing.utility)) # change var names such that the two data.frames could have identical var names
names(lit.missing.utility) <- gsub("QALYs",   "total", names(lit.missing.utility)) # change var names such that the two data.frames could have identical var names

lit.missing2 <- rbind(lit.missing.costs, lit.missing.utility)
table(lit.missing2$c_or_u)


# available missing data methods at different levels in included studies
# ACA/CCA 
# Single imputation
# Multiple imputation
# Composite methods
# Unspecified imputation
# No                        - the study did not performed imputation at certain level
# Unclear [Imputation]      - this means the study said that it imputed missing data but did not stated which level the imputation was performed
# Unclear                   - no information on missing data methods
# NA                        - not applicable
# consistent with base case - only for SA
# Categories that are relevant include single imputation, multiple imputation, composite methods and unspecified imputation

# Total costs or QALYs
fig4_total <- lit.missing2 %>% 
  filter(missing.methods.total == "Single imputation" | missing.methods.total == "Multiple imputation" | missing.methods.total == "Composite methods" | missing.methods.total == "Unspecified imputation" ) %>%
  ggplot(aes(x = missing.methods.total, fill=c_or_u))+
  geom_bar(color = "slategray", alpha = 0.7, width = 0.8, position = position_dodge2(preserve = "single")) +
  geom_text(stat = 'count', aes(label=..count..), position = position_dodge2(0.9), vjust = -0.5, color = "slategray", size = 5, family = "Times") +
  scale_color_identity() +
  scale_fill_manual(name = "Outcome", labels = c("Costs","QoL"), values = c("slategray","gray")) +
  labs(title = "Total: Costs(n=9); QALYs(n=9)", 
       x = "Imputation methods", 
       y = "Number of studies")+
  scale_x_discrete(labels=c("Single imputation" = "Single",
                            "Multiple imputation" = "MI",
                            "Composite methods" = "Composite",
                            "Unspecified imputation" = "Unspecified"),
                   limits=c("Single imputation","Multiple imputation","Composite methods","Unspecified imputation"),
                   position = "bottom") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,10), position = "left", expand = expansion(c(0,.1))) +
  theme_bw()+
  theme(axis.title   = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        axis.text.x  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.text.y  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.ticks   = element_blank(),
        title        = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        legend.title = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        plot.margin = unit(c(1,1.5,0.5,0.5),"line"), panel.grid = element_blank(), legend.position = "left")
plot(fig4_total)

# Costs or QoL at each time point
fig4_points <- lit.missing2 %>% 
  filter(missing.methods.points == "Single imputation" | missing.methods.points == "Multiple imputation" | missing.methods.points == "Composite methods" | missing.methods.points == "Unspecified imputation" ) %>%
  ggplot(aes(x = missing.methods.points, fill = c_or_u)) +
  geom_bar(color = "slategray", alpha = 0.7, width = 0.8, position = position_dodge2(preserve = "single")) +
  geom_text(stat = 'count', aes(label=..count..), position = position_dodge2(0.9), vjust = -0.5, color = "slategray", size = 5, family = "Times") +
  scale_color_identity() +
  scale_fill_manual(name = "Outcome", labels = c("Costs","QoL"), values = c("slategray","gray")) +
  labs(title = "Each Time Point: Costs (n=10); QoL(n=52)",
       x="Imputation methods",
       y="Number of studies")+
  scale_x_discrete(labels=c("Single imputation" = "Single",
                            "Multiple imputation" = "MI",
                            "Composite methods" = "Composite",
                            "Unspecified imputation" = "Unspecified"),
                   limits=c("Single imputation","Multiple imputation","Composite methods","Unspecified imputation"),
                   position = "bottom") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,10), position = "left",expand = expansion(c(0,.1))) +
  theme_bw()+
  theme(axis.title   = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        axis.text.x  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.text.y  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.ticks   = element_blank(),
        title        = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        legend.title = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        plot.margin = unit(c(1,1.5,0.5,0.5),"line"), panel.grid = element_blank(), legend.position = "left")
plot(fig4_points)


# Cost or QoL items
fig4_items <- lit.missing2 %>% 
  filter(missing.methods.items == "Single imputation" | missing.methods.items == "Multiple imputation" | missing.methods.items == "Composite methods" | missing.methods.items == "Unspecified imputation" ) %>%
  ggplot(aes(x = missing.methods.items, fill = c_or_u)) +
  geom_bar(color = "slategray", alpha = 0.7, width = 0.8, position = position_dodge2(preserve = "single")) +
  geom_text(stat = 'count', aes(label=..count..), position = position_dodge2(0.9), vjust = -0.5, color = "slategray", size = 5, family = "Times") +
  scale_color_identity() +
  scale_fill_manual(name = "Outcome", labels = c("Costs","QoL"), values = c("slategray","gray")) +
  labs(title = "Items: Costs (n=56); QoL(n=1)",
       x="Imputation methods",
       y="Number of studies")+
  scale_x_discrete(labels=c("Single imputation" = "Single",
                            "Multiple imputation" = "MI",
                            "Composite methods" = "Composite",
                            "Unspecified imputation" = "Unspecified"),
                   limits=c("Single imputation","Multiple imputation","Composite methods","Unspecified imputation"),
                   position = "bottom") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,10), position = "left", expand = expansion(c(0,.1))) +
  theme_bw()+
  theme(axis.title   = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        axis.text.x  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.text.y  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        axis.ticks   = element_blank(),
        title        = element_text(colour = "slategray", size = 14, family = "Times New Roman", face = "bold"),
        legend.title = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        legend.text  = element_text(colour = "slategray", size = 14, family = "Times New Roman"),
        plot.margin = unit(c(1,1.5,0.5,0.5),"line"), panel.grid = element_blank(), legend.position = "left")
plot(fig4_items)

grid.newpage()
fig4 <- ggarrange(fig4_total, fig4_points, fig4_items, 
                  nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom")
grid.draw(fig4)
