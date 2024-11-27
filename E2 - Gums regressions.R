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
df <- df %>%
  mutate(Gum = case_when(
    Gum == "xan" ~ "Xanthan",
    Gum == "gua" ~ "Guar",
    Gum == "loc" ~ "Locust bean",
    Gum == "car" ~ "Carrageenan (iota)",
    Gum == "gel" ~ "Gellan"
  ))

str(df)

df$`viscosity (mPa.s)`[df$`viscosity (mPa.s)`== "too high"] <- 1000

cols <- c("gum", "percentage", "firmness", "consistency", "cohesiveness", "index", "viscosity") # new column names

(df <- df %>% 
  filter(time == 1) %>% 
   select(c(-time,-Rep,-Code)) %>% 
   mutate(Gum = as.factor(Gum)) %>% 
   setNames(cols) %>%
   mutate_if(is.character, ~as.numeric(as.character(.))))

str(df)
summary (df)


# subsets -----------------------------------------------------------------
tex.lm <- df[,1:6] # texture subset
tex.lm <- na.omit(tex.lm)

vis.lm <- df[,c(1,2,7)] # viscosity subset
vis.lm <- na.omit(vis.lm)



# ploting lm --------------------------------------------------------------
# Viscosity
# Create a dataframe for plotting viscosity response
plot_data <- vis.lm %>%
  mutate(gum = as.factor(gum))  # Ensure 'gum' is a factor

str(plot_data)
summary(plot_data)

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
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Viscosity vs. Percentage by Gum Type", 
       x = "Concentration (%)", 
       y = "Viscosity (mPa/s)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
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
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Firmness vs. Percentage by Gum Type", 
       x = "Concentration (%)",
       y = "Firmness (g)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

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
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Consistency vs. Percentage by Gum Type", 
       x = "Concentration (%)",
       y = "Consistency (g/s)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

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
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Cohesiveness vs. Percentage by Gum Type", 
       x = "Concentration (%)",
       y = "Cohesiveness (g)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

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
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, color = "blue") +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Index of viscosity vs. Percentage by Gum Type", 
       x = "Concentration (%)",
       y = "Index of viscosity (g/s)") +
  scale_x_continuous(breaks = seq(0, max(plot_data$percentage), by = 0.5)) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )



# -------------------------- #
#    Interpolations          #
# -------------------------- #
## General structure for viscosity
# Function to get polynomial coefficients for each gum type
get_polynomial_coefficients <- function(data) {
  # Split data by gum type and fit models
  gum_models <- data %>%
    group_by(gum) %>%
    group_modify(~ {
      model <- lm(viscosity ~ poly(percentage, 2, raw = TRUE), data = .x)
      coef_vals <- coef(model)
      tibble(
        intercept = coef_vals[1],
        linear = coef_vals[2],
        quadratic = coef_vals[3],
        r_squared = summary(model)$r.squared
      )
    })
  
  return(gum_models)
}

# Function to predict viscosity for a given percentage
predict_viscosity <- function(percentage, coefficients) {
  coefficients$intercept + 
    coefficients$linear * percentage + 
    coefficients$quadratic * percentage^2
}

# Function to find percentage for target viscosity using numerical method
find_percentage <- function(target_viscosity, coefficients, 
                            min_percent = 0, max_percent = 3) {
  
  # Function to minimize (difference from target viscosity)
  objective <- function(x) {
    predicted <- predict_viscosity(x, coefficients)
    abs(predicted - target_viscosity)
  }
  
  # Use optimize to find the percentage
  result <- optimize(objective, interval = c(min_percent, max_percent))
  
  return(result$minimum)
}

# Get coefficients for all gum types
coefficients_df <- get_polynomial_coefficients(plot_data)

# Print equations and R² for each gum type
equations <- coefficients_df %>%
  mutate(equation = sprintf("y = %.2f + %.2fx + %.2fx²", 
                            intercept, linear, quadratic),
         r_squared = sprintf("R² = %.4f", r_squared))

print(equations)

# Example usage for interpolation to find viscosity at X% for each gum
interpolation_example <- coefficients_df %>%
  group_by(gum) %>%
  mutate(
    viscosity_at_2.5 = predict_viscosity(2.5, cur_data())
  )

print("Predicted viscosity at X%:")
print(select(interpolation_example, gum, viscosity_at_2.5))

# Example of finding percentage for target viscosity
target_viscosity <- 133.70
percentage_predictions <- coefficients_df %>%
  group_by(gum) %>%
  mutate(
    percentage_for_Y = find_percentage(target_viscosity, cur_data())
  )

print("\nPercentages needed for viscosity = 133.70:")
print(select(percentage_predictions, gum, percentage_for_Y))

# Verification plot
verification_plot <- ggplot(plot_data, aes(x = percentage, y = viscosity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), 
              se = TRUE, color = "blue") +
  geom_hline(yintercept = target_viscosity, linetype = "dashed", color = "red") +
  geom_vline(data = percentage_predictions,
             aes(xintercept = percentage_for_Y),
             linetype = "dashed", color = "green") +
  facet_wrap(~gum, scales = "free") +
  labs(title = "Viscosity vs. Percentage with Target Line",
       x = "Concentration (%)",
       y = "Viscosity (mPa/s)") +
  theme_minimal()

print(verification_plot)


###########
###########
## General structure for firmness
# Function to extract equations and create interpolation functions
extract_equations <- function(data) {
  models <- data %>%
    group_by(gum) %>%
    summarise(
      model = list(lm(firmness ~ poly(percentage, 2, raw = TRUE), data = cur_data())),
      coefficients = list(coef(model[[1]])),
      r_squared = summary(model[[1]])$r.squared,
      equation = list(
        sprintf("%.2f %s %.2fx %s %.2fx²",
                coefficients[[1]][1],
                ifelse(coefficients[[1]][2] >= 0, "+", ""),
                coefficients[[1]][2],
                ifelse(coefficients[[1]][3] >= 0, "+", ""),
                coefficients[[1]][3])
      )
    ) %>%
    mutate(
      # Create function to predict firmness for each gum type
      predict_firmness = map(model, ~function(x) predict(.x, newdata = data.frame(percentage = x))),
      # Create function to find percentage for target firmness
      find_percentage = map2(model, gum, ~function(target) {
        f <- function(x) {
          abs(predict(.x, newdata = data.frame(percentage = x)) - target)
        }
        optimize(f, interval = c(0, max(data$percentage)))$minimum
      })
    )
  return(models)
}

# Extract equations and create functions
equations_df <- extract_equations(plot_data.tex)

# Print equations and R² values
print("Equations and R² values for each gum type:")
equations_df %>%
  select(gum, equation, r_squared) %>%
  mutate(
    equation = unlist(equation),
    r_squared = sprintf("R² = %.4f", r_squared)
  ) %>%
  print(n = Inf)

# Function to predict firmness at specific percentage
predict_at_percentage <- function(percentage, equations_df) {
  equations_df %>%
    mutate(
      firmness = map_dbl(predict_firmness, ~.(percentage))
    ) %>%
    select(gum, firmness)
}

# Function to find percentage for target firmness
find_target_percentage <- function(target_firmness, equations_df) {
  equations_df %>%
    mutate(
      percentage = map_dbl(find_percentage, ~.(target_firmness))
    ) %>%
    select(gum, percentage)
}

# Example usage:
# 1. Predict firmness at 2% concentration
print("\nPredicted firmness at 2% concentration:")
predict_at_percentage(2, equations_df)

# 2. Find percentage needed for target firmness of Yg
print("\nPercentages needed for firmness = Yg:")
find_target_percentage(26.50, equations_df)

# Function to create interpolation plot with specific target
create_interpolation_plot <- function(data, equations_df, target_firmness = NULL) {
  p <- ggplot(data, aes(x = percentage, y = firmness)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), 
                se = TRUE, color = "blue") +
    stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    facet_wrap(~gum, scales = "free") +
    labs(title = "Firmness vs. Percentage by Gum Type",
         x = "Concentration (%)",
         y = "Firmness (g)") +
    scale_x_continuous(breaks = seq(0, max(data$percentage), by = 0.5)) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
  
  if (!is.null(target_firmness)) {
    target_percentages <- find_target_percentage(target_firmness, equations_df)
    p <- p +
      geom_hline(yintercept = target_firmness, linetype = "dashed", color = "red") +
      geom_vline(data = target_percentages,
                 aes(xintercept = percentage),
                 linetype = "dashed", color = "green")
  }
  
  return(p)
}

# Example: Create plot with target firmness of 300g
plot_with_target <- create_interpolation_plot(plot_data.tex, equations_df, 26.5)
print(plot_with_target)



###########
###########
## General structure for Consistency
library(dplyr)
library(ggplot2)
library(ggpmisc)

# Function to extract equations and create interpolation functions
extract_equations <- function(data) {
  models <- data %>%
    group_by(gum) %>%
    summarise(
      model = list(lm(consistency ~ poly(percentage, 2, raw = TRUE), data = cur_data())),
      coefficients = list(coef(model[[1]])),
      r_squared = summary(model[[1]])$r.squared,
      equation = list(
        sprintf("%.2f %s %.2fx %s %.2fx²",
                coefficients[[1]][1],
                ifelse(coefficients[[1]][2] >= 0, "+", ""),
                coefficients[[1]][2],
                ifelse(coefficients[[1]][3] >= 0, "+", ""),
                coefficients[[1]][3])
      )
    ) %>%
    mutate(
      # Create function to predict consistency for each gum type
      predict_consistency = map(model, ~function(x) predict(.x, newdata = data.frame(percentage = x))),
      # Create function to find percentage for target consistency
      find_percentage = map2(model, gum, ~function(target) {
        f <- function(x) {
          abs(predict(.x, newdata = data.frame(percentage = x)) - target)
        }
        optimize(f, interval = c(0, max(data$percentage)))$minimum
      })
    )
  return(models)
}

# Extract equations and create functions
equations_df <- extract_equations(plot_data.tex)

# Print equations and R² values
print("Equations and R² values for each gum type:")
equations_df %>%
  select(gum, equation, r_squared) %>%
  mutate(
    equation = unlist(equation),
    r_squared = sprintf("R² = %.4f", r_squared)
  ) %>%
  print(n = Inf)

# Function to predict consistency at specific percentage
predict_at_percentage <- function(percentage, equations_df) {
  equations_df %>%
    mutate(
      consistency = map_dbl(predict_consistency, ~.(percentage))
    ) %>%
    select(gum, consistency)
}

# Function to find percentage for target consistency
find_target_percentage <- function(target_consistency, equations_df) {
  equations_df %>%
    mutate(
      percentage = map_dbl(find_percentage, ~.(target_consistency))
    ) %>%
    select(gum, percentage)
}

# Example usage:
# 1. Predict consistency at 2% concentration
print("\nPredicted consistency at 2% concentration:")
predict_at_percentage(2, equations_df)

# 2. Find percentage needed for target consistency of 247 
print("\nPercentages needed for consistency = 247 :")
find_target_percentage(247, equations_df)

# Function to create interpolation plot with specific target
create_interpolation_plot <- function(data, equations_df, target_consistency = NULL) {
  p <- ggplot(data, aes(x = percentage, y = consistency)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), 
                se = TRUE, color = "blue") +
    stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    facet_wrap(~gum, scales = "free") +
    labs(title = "Consistency vs. Percentage by Gum Type",
         x = "Concentration (%)",
         y = "Consistency (cP)") +
    scale_x_continuous(breaks = seq(0, max(data$percentage), by = 0.5)) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
  
  if (!is.null(target_consistency)) {
    target_percentages <- find_target_percentage(target_consistency, equations_df)
    p <- p +
      geom_hline(yintercept = target_consistency, linetype = "dashed", color = "red") +
      geom_vline(data = target_percentages,
                 aes(xintercept = percentage),
                 linetype = "dashed", color = "green")
  }
  
  return(p)
}

# Example: Create plot with target consistency of 300 cP
plot_with_target <- create_interpolation_plot(plot_data.tex, equations_df, 247)
print(plot_with_target)



###########
###########
## General structure for Cohesiveness
# Function to extract equations and create interpolation functions
extract_equations <- function(data) {
  models <- data %>%
    group_by(gum) %>%
    summarise(
      model = list(lm(cohesiveness ~ poly(percentage, 2, raw = TRUE), data = cur_data())),
      coefficients = list(coef(model[[1]])),
      r_squared = summary(model[[1]])$r.squared,
      equation = list(
        sprintf("%.2f %s %.2fx %s %.2fx²",
                coefficients[[1]][1],
                ifelse(coefficients[[1]][2] >= 0, "+", ""),
                coefficients[[1]][2],
                ifelse(coefficients[[1]][3] >= 0, "+", ""),
                coefficients[[1]][3])
      )
    ) %>%
    mutate(
      # Create function to predict cohesiveness for each gum type
      predict_cohesiveness = map(model, ~function(x) predict(.x, newdata = data.frame(percentage = x))),
      # Create function to find percentage for target cohesiveness
      find_percentage = map2(model, gum, ~function(target) {
        f <- function(x) {
          abs(predict(.x, newdata = data.frame(percentage = x)) - target)
        }
        optimize(f, interval = c(0, max(data$percentage)))$minimum
      })
    )
  return(models)
}

# Extract equations and create functions
equations_df <- extract_equations(plot_data.tex)

# Print equations and R² values
print("Equations and R² values for each gum type:")
equations_df %>%
  select(gum, equation, r_squared) %>%
  mutate(
    equation = unlist(equation),
    r_squared = sprintf("R² = %.4f", r_squared)
  ) %>%
  print(n = Inf)

# Function to predict cohesiveness at specific percentage
predict_at_percentage <- function(percentage, equations_df) {
  equations_df %>%
    mutate(
      cohesiveness = map_dbl(predict_cohesiveness, ~.(percentage))
    ) %>%
    select(gum, cohesiveness)
}

# Function to find percentage for target cohesiveness
find_target_percentage <- function(target_cohesiveness, equations_df) {
  equations_df %>%
    mutate(
      percentage = map_dbl(find_percentage, ~.(target_cohesiveness))
    ) %>%
    select(gum, percentage)
}

# Example usage:
# 1. Predict cohesiveness at 2% concentration
print("\nPredicted cohesiveness at 2% concentration:")
predict_at_percentage(2, equations_df)

# 2. Find percentage needed for target cohesiveness of -19.60
print("\nPercentages needed for cohesiveness = -19.60:")
find_target_percentage(-19.60, equations_df)

# Function to create interpolation plot with specific target
create_interpolation_plot <- function(data, equations_df, target_cohesiveness = NULL) {
  p <- ggplot(data, aes(x = percentage, y = cohesiveness)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), 
                se = TRUE, color = "blue") +
    stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    facet_wrap(~gum, scales = "free") +
    labs(title = "Cohesiveness vs. Percentage by Gum Type",
         x = "Concentration (%)",
         y = "Cohesiveness") +
    scale_x_continuous(breaks = seq(0, max(data$percentage), by = 0.5)) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
  
  if (!is.null(target_cohesiveness)) {
    target_percentages <- find_target_percentage(target_cohesiveness, equations_df)
    p <- p +
      geom_hline(yintercept = target_cohesiveness, linetype = "dashed", color = "red") +
      geom_vline(data = target_percentages,
                 aes(xintercept = percentage),
                 linetype = "dashed", color = "green")
  }
  
  return(p)
}

# Example: Create plot with target cohesiveness of 0.8
plot_with_target <- create_interpolation_plot(plot_data.tex, equations_df, -19.60)
print(plot_with_target)



###########
###########
## General structure for Index of viscosity
# Function to extract equations and create interpolation functions
extract_equations <- function(data) {
  models <- data %>%
    group_by(gum) %>%
    summarise(
      model = list(lm(index ~ poly(percentage, 2, raw = TRUE), data = cur_data())),
      coefficients = list(coef(model[[1]])),
      r_squared = summary(model[[1]])$r.squared,
      equation = list(
        sprintf("%.2f %s %.2fx %s %.2fx²",
                coefficients[[1]][1],
                ifelse(coefficients[[1]][2] >= 0, "+", ""),
                coefficients[[1]][2],
                ifelse(coefficients[[1]][3] >= 0, "+", ""),
                coefficients[[1]][3])
      )
    ) %>%
    mutate(
      # Create function to predict index for each gum type
      predict_index = map(model, ~function(x) predict(.x, newdata = data.frame(percentage = x))),
      # Create function to find percentage for target index
      find_percentage = map2(model, gum, ~function(target) {
        f <- function(x) {
          abs(predict(.x, newdata = data.frame(percentage = x)) - target)
        }
        optimize(f, interval = c(0, max(data$percentage)))$minimum
      })
    )
  return(models)
}

# Extract equations and create functions
equations_df <- extract_equations(plot_data.tex)

# Print equations and R² values
print("Equations and R² values for each gum type:")
equations_df %>%
  select(gum, equation, r_squared) %>%
  mutate(
    equation = unlist(equation),
    r_squared = sprintf("R² = %.4f", r_squared)
  ) %>%
  print(n = Inf)

# Function to predict index at specific percentage
predict_at_percentage <- function(percentage, equations_df) {
  equations_df %>%
    mutate(
      index = map_dbl(predict_index, ~.(percentage))
    ) %>%
    select(gum, index)
}

# Function to find percentage for target index
find_target_percentage <- function(target_index, equations_df) {
  equations_df %>%
    mutate(
      percentage = map_dbl(find_percentage, ~.(target_index))
    ) %>%
    select(gum, percentage)
}

# Example usage:
# 1. Predict index at 2% concentration
print("\nPredicted index at 2% concentration:")
predict_at_percentage(2, equations_df)

# 2. Find percentage needed for target index of -79.40
print("\nPercentages needed for index = -79.40:")
find_target_percentage(-79.40, equations_df)

# Function to create interpolation plot with specific target
create_interpolation_plot <- function(data, equations_df, target_index = NULL) {
  p <- ggplot(data, aes(x = percentage, y = index)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), 
                se = TRUE, color = "blue") +
    stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE) +
    facet_wrap(~gum, scales = "free") +
    labs(title = "Index vs. Percentage by Gum Type",
         x = "Concentration (%)",
         y = "Index") +
    scale_x_continuous(breaks = seq(0, max(data$percentage), by = 0.5)) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
  
  if (!is.null(target_index)) {
    target_percentages <- find_target_percentage(target_index, equations_df)
    p <- p +
      geom_hline(yintercept = target_index, linetype = "dashed", color = "red") +
      geom_vline(data = target_percentages,
                 aes(xintercept = percentage),
                 linetype = "dashed", color = "green")
  }
  
  return(p)
}

# Example: Create plot with target index of -79.40
plot_with_target <- create_interpolation_plot(plot_data.tex, equations_df, -79.40)
print(plot_with_target)
