rm(list = ls())     # clear objects  
graphics.off() 
#######################################
###### gums bar plots ###########
#######################################


# Packages ----------------------------------------------------------------
inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","cluster", "readxl", "magrittr","hrbrthemes",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats",
              "dplyr","writexl","gtools","ggbiplot","ggrepel",
              "ggstatsplot", "plotly", "car", "ez", "openxlsx","reticulate",
              "rstatix", "patchwork", "ggpmisc")
inst(packages)
theme_set(theme_minimal())


# -------------------------- #
#    Importing Data set      #
# -------------------------- #
(df <- read_excel("gums.xlsx"))



# Wrangling ---------------------------------------------------------------
df$Concent <- as.character(df$Concent)

cols <- c("gum", "firmness", "consistency", "cohesiveness", "index", "viscosity") # new column names

df <- df %>%
  select(-Rep) %>% 
  unite("Gum", c("Code", "Concent"), sep = " ", remove = TRUE) %>%
  setNames(cols)




# subsets -----------------------------------------------------------------
texture <- df[-41,1:5] # texture subset
texture <- na.omit(texture)

viscosity <- df[-41,c(1,6)] # viscosity subset
viscosity$viscosity[viscosity$viscosity == "too high"] <- 1000
viscosity$viscosity <- as.numeric(viscosity$viscosity)
viscosity <- na.omit(viscosity)

gel <- data.frame(gum = factor("SiS"),
                  firmness = 26.5,
                  consistency = 247,
                  cohesiveness = -19.6,
                  index = -79.4,
                  viscosity = 133.7) # commercial sample subset

firmness_gel <- gel$firmness[1]
consistency_gel <- gel$consistency[1]
cohesiveness_gel <- gel$cohesiveness[1]
index_gel <- gel$index[1]
viscosity_gel <- gel$viscosity[1]



# plots -------------------------------------------------------------------
# p1
(plot_firmness <- ggplot(texture, aes(x = gum, y = firmness)) +  # mapping
   geom_col(fill = "grey") +  # bar plot
   labs(title = "Firmness of the different gums tested",
        x = " ", y = "Firmness (g)") +  # titles and axis
   theme(
     axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # increased from 10 to 12
     axis.text.y = element_text(size = 12),  # increased from 10 to 12
     plot.title = element_text(size = 16),  # increased from 14 to 16
     axis.title.y = element_text(size = 14),  # increased from 12 to 14
     plot.margin = margin(t = 10, r = 10, b = 10, l = 28)  # adds more space on the left
   ) + 
   geom_hline(yintercept = firmness_gel, color = "darkgreen", linetype = "dashed", size = 2)
)

# reploting with autoselected colors
texture <- texture %>%
  mutate(color = ifelse(row_number() %in% c(3, 4, 6, 7, 8, 9, 11, 12, 14,16), "darkgreen", "grey"))

(plot_firmness <- ggplot(texture, aes(x = gum, y = firmness, fill = color)) + # mapping
    geom_col() +  #bar plot
    labs(title = "Firmness of the different gums tested",
         x = " ", y = "Firmness (g)") + # titles and axis
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # axis settings
          legend.position = "none")) + # Removal of labels
  geom_hline(yintercept = firmness_gel, color = "darkgreen", linetype = "dashed", size = 1) +
  scale_fill_identity()



# p2
(plot_consistency <- ggplot(texture, aes(x = gum, y = consistency)) + # mapping
    geom_col(fill = "grey") +  #bar plot
    labs(title = "Consistency of the different gums tested",
         x = " ", y = "Consistency (g.sec)") + # titles and axis
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # increased from 10 to 12
      axis.text.y = element_text(size = 12),  # increased from 10 to 12
      plot.title = element_text(size = 16),  # increased from 14 to 16
      axis.title.y = element_text(size = 14),  # increased from 12 to 14
      plot.margin = margin(t = 10, r = 10, b = 10, l = 28)  # adds more space on the left
    ) + 
    geom_hline(yintercept = consistency_gel, color = "darkgreen", linetype = "dashed", size = 2)
)




#p3
(plot_cohesiveness <- ggplot(texture, aes(x = gum, y = cohesiveness)) + # mapping
    geom_col(fill = "grey") +  #bar plot
    labs(title = "Cohesiveness of the different gums tested",
         x = " ", y = "Cohesiveness (g)") + # titles and axis
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # increased from 10 to 12
      axis.text.y = element_text(size = 12),  # increased from 10 to 12
      plot.title = element_text(size = 16),  # increased from 14 to 16
      axis.title.y = element_text(size = 14),  # increased from 12 to 14
      plot.margin = margin(t = 10, r = 10, b = 10, l = 28)  # adds more space on the left
    ) + 
    geom_hline(yintercept = cohesiveness_gel, color = "darkgreen", linetype = "dashed", size = 2)
)





# p4
(plot_index <- ggplot(texture, aes(x = gum, y = index)) + # mapping
    geom_col(fill = "grey") +  #bar plot
    labs(title = "Index of viscosity of the different gums tested",
         x = " ", y = "Index of viscosity (g.sec)") + # titles and axis
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # increased from 10 to 12
      axis.text.y = element_text(size = 12),  # increased from 10 to 12
      plot.title = element_text(size = 16),  # increased from 14 to 16
      axis.title.y = element_text(size = 14),  # increased from 12 to 14
      plot.margin = margin(t = 10, r = 10, b = 10, l = 28)  # adds more space on the left
    ) + 
    geom_hline(yintercept = index_gel, color = "darkgreen", linetype = "dashed", size = 2)
)




# p5
(plot_viscosity <- ggplot(viscosity, aes(x = gum, y = viscosity)) + # mapping
    geom_col(fill = "grey") +  #bar plot
    labs(title = "Viscosity of the different gums tested",
         x = " ", y = "Viscosity") + # titles and axis
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # increased from 10 to 12
      axis.text.y = element_text(size = 12),  # increased from 10 to 12
      plot.title = element_text(size = 16),  # increased from 14 to 16
      axis.title.y = element_text(size = 14),  # increased from 12 to 14
      plot.margin = margin(t = 10, r = 10, b = 10, l = 28)  # adds more space on the left
    ) + 
    geom_hline(yintercept = viscosity_gel, color = "darkgreen", linetype = "dashed", size = 2)
)



plot_firmness + plot_consistency + plot_cohesiveness + plot_index + plot_viscosity


# df transformation to mean values ----------------------------------------
(texture <- texture %>%
   group_by(gum) %>% 
   summarise(
     firmness = mean(firmness),
     consistency = mean(consistency),
     cohesiveness = mean(cohesiveness),
     index = mean(index)
   ))

(viscosity <- viscosity %>%
    filter(viscosity<900) %>% 
    group_by(gum) %>% 
    summarise(
      viscosity = mean(viscosity),
    ))
