rm(list = ls())     # clear objects  
graphics.off() 
###################################
###### Sprectro data  #############
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
              "fpc","plot3D", "cluster", "readxl", "magrittr",
              "multipanelfigure","klaR","psych","MASS","ggord","devtools",
              "reshape2","RColorBrewer","SensoMineR","FactoMineR","stats","drc",
              "ggstatsplot", "car")
inst(packages)
theme_set(theme_minimal())




# Dataframes --------------------------------------------------------------
(df <- read_excel("DPPH_Najjar.xlsx"))
Calib <- df[-c(41:49),]
DPPH <- df[c(41:49),]
  


de# Controls prep -------------------------------------------------------------
PC.Calib <- Calib[grep("Positive", Calib$ID), c(1,3)] %>% 
  summarise(Average = mean(`Abs (515)`))

NC.Calib <- Calib[grep("Negative", Calib$ID), c(1,3)] %>% 
  summarise(Average = mean(`Abs (515)`))

names(Calib) <- c("ID", "Concent", "Abs_515")


# Scavenging calculation --------------------------------------------------
Calib <- Calib %>% 
  mutate(Scav = (((Abs_515 - NC.Calib$Average)/(PC.Calib$Average - NC.Calib$Average))*100))


# Model subsetting --------------------------------------------------------
(ss1 <-Calib %>% 
   select(ID,Concent,Scav))

# Column format -----------------------------------------------------------
plot(ss1$Concent,ss1$Scav)


# -------------------------- #
#    Modeling  #
# -------------------------- #

mod1 <- drm(Scav ~ Concent,
            data = ss1, fct = LL.4(names = c("Slope", "Lower limit", "Upper limit", "EC50")))
plot(mod1)
plot(mod1, type = "all")
summary(mod1)


#  Model Selection -------------------------------------------------------------
mselect(mod1, fctList = list(W1.3(fixed=c(NA, 100, NA)),
                             W1.4(), 
                             W2.3(fixed=c(NA, 100, NA)), 
                             W2.4(),  
                             LL.4()), linreg=TRUE)

#  Fit stats -------------------------------------------------------------------
coef(mod1)
plot(fitted(mod1), residuals(mod1))
hatvalues(mod1)
cooks.distance(mod1)
logLik(mod1)
plot(mod1, log="", main = "Logistic function")

"""
# Subsetting for improved model ------------------------------------------------
ss2 <- ss1[c(),]
mod1.1 <- drm(Scav ~ Concent,
              data = ss2, fct = LL.4(names = c("Slope", "Lower limit", "Upper limit", "EC50")))
plot(mod1.1)
plot(mod1.1, type = "all")
summary(mod1.1)

#  Model Selection V.2 ---------------------------------------------------------
mselect(mod1.1, fctList = list(W1.3(fixed=c(NA, 100, NA)),
                               W1.4(), 
                               W2.3(fixed=c(NA, 100, NA)), 
                               W2.4(),  
                               LL.4()), linreg=TRUE)

####  Model comparison V.1 -----------------------------------------------------
mselect(mod1, fctList = list(W1.3(fixed=c(NA, 100, NA)),
                             W1.4(), 
                             W2.3(fixed=c(NA, 100, NA)), 
                             W2.4(),  
                             LL.4()), linreg=TRUE)
"""


# --------------------------------------- #
#     DPPH test                           #
#    Subsetting and wrangling             #
# --------------------------------------- #

# Prep interpol terms mod1 ---------------------------------------------------
S <- mod1$fit$par[1]
LL <- mod1$fit$par[2]
UL <- mod1$fit$par[3]
ED50 <- mod1$fit$par[4]

# Interpolation test 4 param log logistic --------------------------------------
# Scavenging calculation --------------------------------------------------
names(DPPH) <- c("ID", "Concent", "Abs_515")

DPPH <- DPPH %>% 
  mutate(Scav = (((Abs_515 - NC.Calib$Average)/(PC.Calib$Average - NC.Calib$Average))*100))

DPPH_avg <- DPPH %>%
  group_by(ID) %>%
  summarise(Average = mean(Scav, na.rm = TRUE))


# Calc conc from a given % of Scav  --------------------------------------------
# Assuming NC.Calib is your summarized data frame
Y <- DPPH_avg %>% pull(Average)
Conc <- ED50/((UL-Y)/(Y-LL))^(-1/S)
Conc

#Calc % of Scav from a given conc  ---------------------------------------------
X <- Conc
Scav <- LL + (UL - LL)/(1 + (ED50/X)^-S)
Scav

# Re-plot interpolations -------------------------------------------------------
plot(mod1, type="all", 
     main = "Trolox Calibration Curve and Samples Interpolation",     
     xlab = "Trolox concentration (mg/mL)",             
     ylab = "Scavenging activity (%)") 
points(c(X, Conc), c(Scav, Y), pch = 18, col = "red", cex=1.5)

D_stat <- DPPH %>% 
  select(ID, Scav)

# -------------------------- #
#    5.5. Statistics         #
# -------------------------- #


# Normal distribution for Final Product "FP" from DPPH test ------------------
shapiro.test(D_stat$Scav[D_stat$ID == "FP"]) 
# Normal distributed

# Normal distribution for Aloe vera sample "AV" from DPPH test ---------------
shapiro.test(D_stat$Scav[D_stat$ID == "AV"]) 
# Normal distributed

# Normal distribution fo Final Product without Aloe "FPNAV" from DPPH test --
shapiro.test(D_stat$Scav[D_stat$ID == "FPNAV"]) 
# Normal distributed


leveneTest(Scav ~ ID, data = D_stat)
# Homosedasticity checked


# Prueba t de Student para comparar las medias (si los datos son normales)  ----
#t.test(Equiv_μg_mL ~ Solv, data = dpph.obs.treated) 
#### Prueba de Wilcoxon-Mann-Whitney para comparar las medianas (si los datos no son normales)  ------
#wilcox.test(Equiv_μg_mL ~ Solv, data = dpph.obs.treated, paired = FALSE) #Both distributions are not normal distributions and means are not equal

# ANOVA model
mod <- aov(Scav ~ ID, data = D_stat)

# Q-Q plot for residuals normality
qqnorm(resid(mod))
qqline(resid(mod), col = "red")

# Tukey post-ANOVA
tukey_results <- TukeyHSD(mod)

# Tukey results
tukey_results[["ID"]]



# -------------------------- #
#    5.5. Stat plot          #
# -------------------------- #

# Combine plot and statistical test with violin plots for DPPH -----------
ggbetweenstats(
  D_stat, ID, Scav,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  p.adjust.method = "none",
  effsize.type = "bonferroni",
  title = "Trolox equivalent scavenged in DPPH samples",
  package = "ggsci",
  palette = "nrc_npg"
)
