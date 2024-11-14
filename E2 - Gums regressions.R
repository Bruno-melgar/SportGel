rm(list = ls())     # clear objects  
graphics.off() 
#######################################
###### Regressions ###########
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
(df <- read_excel("gumts.xlsx"))
(df <- df[,c(-1,-12,-13,-14)])


# linear regresions -------------------------------------------------------
# Wrangling ---------------------------------------------------------------
df$`viscosity (mPa.s)`[df$`viscosity (mPa.s)`== "too high"] <- 1000

cols <- c("gum", "percentage", "firmness", "consistency", "cohesiveness", "index", "viscosity") # new column names

(df <- df %>% 
  filter(time == 1) %>% 
   select(c(-time,-Rep,-Code)) %>% 
   mutate(Gum = as.factor(Gum)) %>% 
   setNames(cols) %>%
   mutate_if(is.character, ~as.numeric(as.character(.))))


# subsets -----------------------------------------------------------------
tex.lm <- df[,1:6] # texture subset
tex.lm <- na.omit(texture)

vis.lm <- df[,c(1,2,7)] # viscosity subset
vis.lm <- na.omit(viscosity)



# ploting lm --------------------------------------------------------------
# Viscosity
# Create a dataframe for plotting viscosity response
plot_data <- vis.lm %>%
  mutate(gum = as.factor(gum))  # Ensure 'gum' is a factor

# Viscosity plots linear
ggplot(plot_data, aes(x = percentage, y = viscosity)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Viscosity vs. Percentage by Gum Type", x = "Percentage", y = "Viscosity") 

# Viscosity plots polynomial second degree
ggplot(plot_data, aes(x = percentage, y = viscosity)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Viscosity vs. Percentage by Gum Type", x = "Concentration (%)", y = "Viscosity (mPa/s)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),  # Increase base text size by 2 points
    plot.title = element_text(size = 16),  # Increase title font size by 2 points
    axis.title = element_text(size = 14),  # Increase axis title font size by 2 points
    axis.text = element_text(size = 12),  # Increase axis text font size by 2 points
    strip.text = element_text(size = 12)  # Increase facet label font size by 2 points
  )




# Ajustar el modelo polinomial
model <- lm(viscosity ~ poly(percentage, 2), data = vis.lm)

# Crear datos predichos
vis.lm$pred <- predict(model, newdata = vis.lm)



# Combining poly and linear plots
# Linear subset
gua_data <- plot_data %>%
  filter(gum == "gua")

# polynomial subset
other_data <- plot_data %>%
  filter(gum != "gua")

# Polynomial plots
(plot_poly <- ggplot(other_data, aes(x = percentage, y = viscosity)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Polynomial adj
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # R^2 and ec
  facet_wrap(~gum, scales = "free") +  # Facet
  labs(title = "Viscosity vs. Percentage (Polynomial Model)", x = "Percentage", y = "Viscosity"))

# linear plot
(plot_linear <- ggplot(gua_data, aes(x = percentage, y = viscosity)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "darkgreen") +  # lineal ad
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # R^2 and ec
  facet_wrap(~gum, scales = "free") +  # Facet
  labs(title = "Viscosity vs. Percentage (Linear Model for Guar)", x = "Percentage", y = "Viscosity"))

# Merging plots
layout <- "
AAAABB
AAAA##
"
(plot_poly | plot_linear) + 
  plot_layout(design = layout)



# Texture responses
# Create a dataframe for plotting viscosity response
plot_data.tex <- tex.lm %>%
  mutate(gum = as.factor(gum))  # Ensure 'gum' is a factor

# Firmness plots
# Linear
ggplot(plot_data.tex, aes(x = percentage, y = firmness)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Firmness vs. Percentage by Gum Type", x = "Percentage", y = "Firmness") 
# Poly
ggplot(plot_data.tex, aes(x = percentage, y = firmness)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Firmness vs. Percentage by Gum Type", x = "Percentage", y = "Firmness")

# Consistency plots
# Linear
ggplot(plot_data.tex, aes(x = percentage, y = consistency)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Consistency vs. Percentage by Gum Type", x = "Percentage", y = "Consistency")
# Poly
ggplot(plot_data.tex, aes(x = percentage, y = consistency)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Consistency vs. Percentage by Gum Type", x = "Percentage", y = "Consistency")

# Cohesiveness plots
# Linear
ggplot(plot_data.tex, aes(x = percentage, y = cohesiveness)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Cohesiveness vs. Percentage by Gum Type", x = "Percentage", y = "Cohesiveness")
# Poly
ggplot(plot_data.tex, aes(x = percentage, y = cohesiveness)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Cohesiveness vs. Percentage by Gum Type", x = "Percentage", y = "Cohesiveness")

# Index of viscosity plots
# Linear
ggplot(plot_data.tex, aes(x = percentage, y = index)) +
  geom_point() +  # Scatterplot of the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Index of viscosity vs. Percentage by Gum Type", x = "Percentage", y = "Index of viscosity")
# Poly
ggplot(plot_data.tex, aes(x = percentage, y = index)) +
  geom_point() +   # Scatterplot of the data points
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +  # Add regression lines (without confidence intervals)
  stat_poly_eq(formula = y ~ poly(x, 2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + # Add equation and R^2
  facet_wrap(~gum, scales = "free") +  # Create facets for each gum type
  labs(title = "Index of viscosity vs. Percentage by Gum Type", x = "Percentage", y = "Index of viscosity")

