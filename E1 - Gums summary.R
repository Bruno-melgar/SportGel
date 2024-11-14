rm(list = ls())     # clear objects  
graphics.off() 
#######################################
###### Gums properties summary ########
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

# replace "too high" for 1000
df$`Viscosity (mPa/s)`[df$`Viscosity (mPa/s)` == "too high"] <- 1000

# Convert viscosity to numeric
df$`Viscosity (mPa/s)` <- as.numeric(df$`Viscosity (mPa/s)`)

# filter "control"
control_values <- df %>%
  filter(Code == "SiS") %>%
  select(-Code, -Rep, -Concent) %>%
  slice(1)


# Similarity percentage calculation
df_similarity <- df %>%
  filter(Code != "SiS") %>%
  mutate(across(starts_with("Firmness"):starts_with("viscosity"), 
                ~ (. / control_values[[cur_column()]]) * 100))

# Transformed df to long format
df_long <- df_similarity %>%
  pivot_longer(cols = starts_with("Firmness"):starts_with("viscosity"), names_to = "property", values_to = "similarity")


# -------------------------- #
#    Plot  #
# -------------------------- #
# Limit extream values between 0% and 200%
df_long <- df_long %>%
  mutate(similarity = ifelse(similarity > 200, 200, similarity),
         similarity = ifelse(similarity < 0, 0, similarity))

# similarity average calculation for the combinations of Code, Concent and property
df_long_avg <- df_long %>%
  group_by(Code, Concent, property) %>%
  summarise(similarity = mean(similarity, na.rm = TRUE)) %>%
  ungroup()

# Final plot
ggplot(df_long_avg, aes(x = Code, y = factor(Concent), fill = similarity)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(similarity == 200, ">200", round(similarity, 1))), 
            color = "black", size = 3) +
  facet_wrap(~property) +
  scale_fill_gradient2(low = "white", mid = "darkgreen", high = "white", 
                       midpoint = 100, limits = c(0, 200), na.value = "white") +
  labs(x = "Gum type", y = "Concentration", fill = "Similarity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
