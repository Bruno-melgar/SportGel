rm(list = ls())     # clear objects  
graphics.off() 
###################################
###### Stats time series  #############
###################################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "fpc","plot3D", "cluster", "readxl","writexl", "magrittr",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats","drc",
              "ggstatsplot", "car")
inst(packages)
theme_set(theme_minimal())




# Dataframes --------------------------------------------------------------
(df <- read_excel("gumts.xlsx"))
str(df)
summary(df)

# Transformation -----------------------------------------------------------
# Subset for statistics
st <- df[, !names(df) %in% c("Replicate", 
                             "Rep", 
                             "Code", 
                             "gum stirring time (min)", 
                             "heat at mix?", 
                             "temp of test")]

# Rename
names(st) <- c("time (days)", 
               "gum", 
               "concentration (%)", 
               "Firmness (g)", 
               "Consistency (g/s)", 
               "Cohesiveness (g)", 
               "Index of Viscosity (g/s)", 
               "Viscosity (mPa/s)")

# Changing "too high" in column "Viscosity (mPa/s)" to 1000
st$`Viscosity (mPa/s)` <- as.numeric(gsub("too high", "1000", st$`Viscosity (mPa/s)`))

# Filter gum observations only
st <- st %>% 
  filter(gum %in% c("xan", "gua", "loc", "car", "gel"))

# Change names to gums observations
st <- st %>%
  mutate(gum = case_when(
    gum == "xan" ~ "Xanthan",
    gum == "gua" ~ "Guar",
    gum == "loc" ~ "Locust bean",
    gum == "car" ~ "Carrageenan (iota)",
    gum == "gel" ~ "Gellan"
  ))

str(st)

# Change factors structure to factor
st <- st %>%
  mutate(
    `time (days)` = as.factor(`time (days)`),
    gum = as.factor(gum),
    `concentration (%)` = as.factor(`concentration (%)`),
    gum = as.factor(gum),
    gum = as.factor(gum),
    gum = as.factor(gum),
    gum = as.factor(gum),
    gum = as.factor(gum)
  )

summary(st)


# -------------------------- #
#    Statistics              #
# -------------------------- #
# Assumptions  -----------------------------------------------------------
st %>%
  group_by(gum, `concentration (%)`, `time (days)`) %>%
  summarise(
    shapiro_p_value = shapiro.test(`Firmness (g)`) $p.value
  )

# normality plot
ggplot(st, aes(sample = `Firmness (g)`)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ gum + `concentration (%)` + `time (days)`)

"""Samples have only duplicate in the mayority of the cases therefore 
is not possible to determine Normality or Levene, so we will be using
no parametric tests"""

# No parametric tests  ------------------------------------------------------

## Firmness
# Factors
(fg <- kruskal.test(`Firmness (g)` ~ gum, data = st)) 
(fc <- kruskal.test(`Firmness (g)` ~ `concentration (%)`, data = st))  
(ft <- kruskal.test(`Firmness (g)` ~ `time (days)`, data = st))  
# Interactions
(fgc <- kruskal.test(`Firmness (g)` ~ interaction(gum, `concentration (%)`), data = st))
(fgt <- kruskal.test(`Firmness (g)` ~ interaction(gum, `time (days)`), data = st))
(fct <- kruskal.test(`Firmness (g)` ~ interaction(`concentration (%)`, `time (days)`), data = st))

# Table summary
tests_f <- list(
  gum = fg,
  `concentration (%)` = fc,
  `time (days)` = ft,
  `gum x concentration (%)` = fgc,
  `gum x time (days)` = fgt,
  `concentration (%) x time (days)` = fct
)

# Automatic tibble
(firmness_kw <- tibble(
  Factor = names(tests_f),
  Chi_squared = sapply(tests_f, function(x) x$statistic),
  DF = sapply(tests_f, function(x) x$parameter),
  p_value = sapply(tests_f, function(x) x$p.value)
))

## Consistency
# Factors
cg <- kruskal.test(`Consistency (g/s)` ~ gum, data = st)  
cc <- kruskal.test(`Consistency (g/s)` ~ `concentration (%)`, data = st)  
ct <- kruskal.test(`Consistency (g/s)` ~ `time (days)`, data = st)  

# Interactions
cgc <- kruskal.test(`Consistency (g/s)` ~ interaction(gum, `concentration (%)`), data = st)
cgt <- kruskal.test(`Consistency (g/s)` ~ interaction(gum, `time (days)`), data = st)
cct <- kruskal.test(`Consistency (g/s)` ~ interaction(`concentration (%)`, `time (days)`), data = st)

# Table summary
tests_c <- list(
  gum = cg,
  `concentration (%)` = cc,
  `time (days)` = ct,
  `gum x concentration (%)` = cgc,
  `gum x time (days)` = cgt,
  `concentration (%) x time (days)` = cct
)

# Automatic tibble
(consistency_kw <- tibble(
  Factor = names(tests_c),
  Chi_squared = sapply(tests_c, function(x) x$statistic),
  DF = sapply(tests_c, function(x) x$parameter),
  p_value = sapply(tests_c, function(x) x$p.value)
))


## Cohesiveness
# Factors
cog <- kruskal.test(`Cohesiveness (g)` ~ gum, data = st)  
coc <- kruskal.test(`Cohesiveness (g)` ~ `concentration (%)`, data = st)  
cot <- kruskal.test(`Cohesiveness (g)` ~ `time (days)`, data = st)  

# Interactions
cogc <- kruskal.test(`Cohesiveness (g)` ~ interaction(gum, `concentration (%)`), data = st)
cogt <- kruskal.test(`Cohesiveness (g)` ~ interaction(gum, `time (days)`), data = st)
coct <- kruskal.test(`Cohesiveness (g)` ~ interaction(`concentration (%)`, `time (days)`), data = st)

# Table summary
tests_co <- list(
  gum = cog,
  `concentration (%)` = coc,
  `time (days)` = cot,
  `gum x concentration (%)` = cogc,
  `gum x time (days)` = cogt,
  `concentration (%) x time (days)` = coct
)

# Automatic tibble
(cohesiveness_kw <- tibble(
  Factor = names(tests_co),
  Chi_squared = sapply(tests_co, function(x) x$statistic),
  DF = sapply(tests_co, function(x) x$parameter),
  p_value = sapply(tests_co, function(x) x$p.value)
))


## Index of viscosity
# Factors
ig <- kruskal.test(`Index of Viscosity (g/s)` ~ gum, data = st)  
ic <- kruskal.test(`Index of Viscosity (g/s)` ~ `concentration (%)`, data = st)  
it <- kruskal.test(`Index of Viscosity (g/s)` ~ `time (days)`, data = st)  

# Interactions
igc <- kruskal.test(`Index of Viscosity (g/s)` ~ interaction(gum, `concentration (%)`), data = st)
igt <- kruskal.test(`Index of Viscosity (g/s)` ~ interaction(gum, `time (days)`), data = st)
ict <- kruskal.test(`Index of Viscosity (g/s)` ~ interaction(`concentration (%)`, `time (days)`), data = st)

# Table summary
tests_i <- list(
  gum = ig,
  `concentration (%)` = ic,
  `time (days)` = it,
  `gum x concentration (%)` = igc,
  `gum x time (days)` = igt,
  `concentration (%) x time (days)` = ict
)

# Automatic tibble
(index_viscosity_kw <- tibble(
  Factor = names(tests_i),
  Chi_squared = sapply(tests_i, function(x) x$statistic),
  DF = sapply(tests_i, function(x) x$parameter),
  p_value = sapply(tests_i, function(x) x$p.value)
))


## Viscosity
# Factors
vg <- kruskal.test(`Viscosity (mPa/s)` ~ gum, data = st)  
vc <- kruskal.test(`Viscosity (mPa/s)` ~ `concentration (%)`, data = st)  
vt <- kruskal.test(`Viscosity (mPa/s)` ~ `time (days)`, data = st)  

# Interactions
vgc <- kruskal.test(`Viscosity (mPa/s)` ~ interaction(gum, `concentration (%)`), data = st)
vgt <- kruskal.test(`Viscosity (mPa/s)` ~ interaction(gum, `time (days)`), data = st)
vct <- kruskal.test(`Viscosity (mPa/s)` ~ interaction(`concentration (%)`, `time (days)`), data = st)

# Table summary
tests_v <- list(
  gum = vg,
  `concentration (%)` = vc,
  `time (days)` = vt,
  `gum x concentration (%)` = vgc,
  `gum x time (days)` = vgt,
  `concentration (%) x time (days)` = vct
)

# Automatic tibble
(viscosity_kw <- tibble(
  Factor = names(tests_v),
  Chi_squared = sapply(tests_v, function(x) x$statistic),
  DF = sapply(tests_v, function(x) x$parameter),
  p_value = sapply(tests_v, function(x) x$p.value)
))


# Summary table  ------------------------------------------------------
# Crear listas de cada tibble de resultados
firmness_kw$Response <- "Firmness"
consistency_kw$Response <- "Consistency"
cohesiveness_kw$Response <- "Cohesiveness"
index_viscosity_kw$Response <- "Index of Viscosity"
viscosity_kw$Response <- "Viscosity"

# Combinar todas las tablas en una sola
all_results_kw <- bind_rows(
  firmness_kw,
  consistency_kw,
  cohesiveness_kw,
  index_viscosity_kw,
  viscosity_kw
)

# Reorganizar las columnas para que 'Response' esté al principio
all_results_kw <- all_results_kw %>%
  select(Response, Factor, Chi_squared, DF, p_value)

# Mostrar la tabla combinada
all_results_kw


write_xlsx(all_results_kw, path = "all_results_kw.xlsx")




