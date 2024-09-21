# Load necessary libraries
library(dplyr)
library(rdd)
library(gmm)
library(stargazer)
library(ggplot2)

# Read datasets
combined_df <- read.csv("prDrugYr.csv")
semaglutide <- read.csv("semaglutide.csv")
pyr <- read.csv("grouped.csv")
saq <- read.csv("saq.csv")

# Filter data for analysis
nrow(semaglutide %>% filter(AGEX >= 55 & AGEX <= 75))
oz <- saq %>% filter(OZ == 1)
mr <- oz %>% filter(INSURC == 4 | INSURC == 6)

# Function to handle duplicated column names
custom_repair <- function(names) {
  if (any(duplicated(names))) {
    dup_names <- names[duplicated(names)]
    suffix <- 1
    for (dup_name in dup_names) {
      names[names == dup_name] <- paste0(dup_name, "_", suffix)
      suffix <- suffix + 1
    }
  }
  return(names)
}

# Filter out invalid data
pyr <- pyr %>% filter(DSDIA != -15 & DSDIET != -15 & DSMED != -15 & DSINSU != -15 & DSKIDN != -15 & DSKIDN != 1)
saq <- saq %>% filter(DSDIA != -15 & DSDIET != -15 & DSMED != -15 & DSINSU != -15 & DSKIDN != -15 & DSKIDN != 1)

# Convert selected columns to character
columns_to_convert <- c("DSDIA", "DSDIET", "DSMED", "DSINSU", "RACEVX", "SEX", "MCRPD", "DSKIDN")
pyr$ins <- as.character(pyr$INSURC)
saq$ins <- as.character(saq$INSURC)
pyr[columns_to_convert] <- lapply(pyr[columns_to_convert], as.character)
saq[columns_to_convert] <- lapply(saq[columns_to_convert], as.character)

# Calculate coverage variables
pyr <- pyr %>%
  mutate(
    pvcov = RXPVX / RXXPX,
    mrcov = RXMRX / RXXPX,
    cov = 1 - RXSFX / RXXPX,
    OZ = as.integer(ozOZ > 0)
  )
saq <- saq %>%
  mutate(
    pvcov = 100 * (RXPVX / RXXPX),
    mrcov = 100 * (RXMRX / RXXPX),
    cov = 100 * (1 - RXSFX / RXXPX),
    OZ = as.integer(ozOZ > 0)
  )

# Save datasets
write.csv(saq, 'saq.csv')
write.csv(oz, 'oz.csv')

# Summary statistics
non_categorical_cols <- sapply(saq, function(x) !is.factor(x) && !is.character(x))
stargazer(as.data.frame(saq[, non_categorical_cols]), out = "sum_num.tex")
stargazer(as.data.frame(oz[, non_categorical_cols]), out = "sum_num_oz.tex")

# Regression models
logit1 <- glm(OZ ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + DSDIA * bmigroup, data = saq, family = binomial(link = 'logit'))
summary(logit1)

ols1e <- lm(log(1 + ozRXXPX) ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + DSDIA * bmigroup, data = saq)
summary(ols1e)

ols1o <- lm(log(1 + RXSFX) ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + DSDIA * bmigroup + cov, data = saq)
summary(ols1o)

# BMI and utilization regressions
logit33 <- glm(OZ ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + DSDIA * bmigroup, data = saq %>% filter(AGEX < 65), family = binomial(link = 'logit'))
summary(logit33)

ols33e <- lm(log(1 + ozRXXPX) ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + DSDIA * bmigroup, data = saq %>% filter(AGEX < 65))
summary(ols33e)

# Regressions for different subgroups (diabetes, insurance, age, etc.)
logit5 <- glm(OZ ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + bmigroup + cov + metf + otdrug + DSINSU + DSDIET, data = saq %>% filter(DSDIA == 1), family = binomial(link = 'logit'))
summary(logit5)

logit51 <- glm(OZ ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + bmigroup + pvcov + metf + otdrug + DSINSU + DSDIET, data = pyr %>% filter(DSDIA == 1 & AGEX < 65), family = binomial(link = 'logit'))
summary(logit51)

logit52 <- glm(OZ ~ AGEX + SEX + RACEVX + POVLEV + EDUCYR + bmigroup + mrcov + metf + otdrug + DSINSU + DSDIET, data = pyr %>% filter(DSDIA == 1 & (INSURC == 4 | INSURC == 6)), family = binomial(link = 'logit'))
summary(logit52)

# Stargazer for reporting results
stargazer(logit1, logit33, logit42, out = 'utilization.tex')
stargazer(logit5, logit51, logit52, out = 'substitution.tex')

# RDD regression
filtered <- saq %>% filter(AGEX >= 55 & AGEX <= 75 & INSURC != 3)
rdd_model <- RDestimate(ozRXXPX ~ AGEX | POVLEV + SEX + RACEVX + EDUCYR + DSDIA * bmigroup, data = filtered, cutpoint = 65, bw = 3)
summary(rdd_model)

# Create a histogram of spending variables
oz <- pyr %>% filter(OZ == 1) %>%
  mutate(
    aveoop = ozRXSFX / ozRXQUANTY,
    avepv = ozRXPVX / ozRXQUANTY,
    avemr = ozRXMRX / ozRXQUANTY
  )

# Histograms for spending data
hist(oz$ozRXSFX)
hist(oz$ozRXPVX)
hist(oz$ozRXMRX)
hist(oz$ozRXQUANTY)
hist(oz$aveoop)
hist(oz$avepv)
hist(oz$avemr)

# Plot for insurance type distribution
oz$insurance_type <- ifelse(oz$ozRXSFX > 0, "OOP", 
                            ifelse(oz$ozRXPVX > 0, "Private Insurance", 
                                   ifelse(oz$ozRXMRX > 0, "Medicare", "None")))

ggplot(oz %>% filter(ozRXSFX > 0 | ozRXPVX > 0 | ozRXMRX > 0),
       aes(x = ifelse(insurance_type == "OOP", ozRXSFX,
                      ifelse(insurance_type == "Private Insurance", ozRXPVX, ozRXMRX)))) +
  geom_histogram(binwidth = 1000, fill = "#A2C7E5", color = "black") +
  labs(title = "Insurance Coverage Distribution") +
  xlab("Insurance Coverage") +
  ylab("Frequency") +
  facet_wrap(~ insurance_type, scales = "free")
