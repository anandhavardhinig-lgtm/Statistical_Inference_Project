# =============================================
# NYKAA CAMPAIGN DATA - STATISTICAL INFERENCE PROJECT
# Proper Data Cleaning + 8 Parametric & Non-Parametric Tests
# Simple R Code with Insights at End of Each Test
# =============================================

# ---------------------------------------------
# 1. LOAD LIBRARIES
# ---------------------------------------------

library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)

# ---------------------------------------------
# 2. LOAD DATASET
# ---------------------------------------------

nykaa <- read.csv("V:/Downloads/nykaa_campaign_data.csv",
                  stringsAsFactors = FALSE)

head(nykaa)
str(nykaa)
summary(nykaa)

# ---------------------------------------------
# 3. DATA CLEANING
# ---------------------------------------------

# Clean column names
nykaa_clean <- clean_names(nykaa)

# Remove duplicate rows
nykaa_clean <- nykaa_clean %>% distinct()

# Check missing values
colSums(is.na(nykaa_clean))

# Remove rows with missing important variables
nykaa_clean <- nykaa_clean %>%
  drop_na(revenue, marketing_spend, impressions, clicks, roi)

# Convert campaign_type to factor (if exists)
if("campaign_type" %in% colnames(nykaa_clean)){
  nykaa_clean$campaign_type <- as.factor(nykaa_clean$campaign_type)
}

# Convert region to factor (if exists)
if("region" %in% colnames(nykaa_clean)){
  nykaa_clean$region <- as.factor(nykaa_clean$region)
}

# ---------------------------------------------
# 4. LOG TRANSFORMATION FOR REVENUE
# ---------------------------------------------

nykaa_clean$log_revenue <- log(nykaa_clean$revenue)

# ---------------------------------------------
# 5. WINSORIZATION FOR REVENUE
# ---------------------------------------------

Q1 <- quantile(nykaa_clean$log_revenue, 0.25)
Q3 <- quantile(nykaa_clean$log_revenue, 0.75)
IQR_val <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

nykaa_clean$log_revenue_final <- ifelse(
  nykaa_clean$log_revenue > upper, upper,
  ifelse(nykaa_clean$log_revenue < lower, lower,
         nykaa_clean$log_revenue)
)

# ---------------------------------------------
# 6. WINSORIZATION FOR ROI
# ---------------------------------------------

Q1_roi <- quantile(nykaa_clean$roi, 0.25)
Q3_roi <- quantile(nykaa_clean$roi, 0.75)
IQR_roi <- Q3_roi - Q1_roi

lower_roi <- Q1_roi - 1.5 * IQR_roi
upper_roi <- Q3_roi + 1.5 * IQR_roi

nykaa_clean$roi_final <- ifelse(
  nykaa_clean$roi > upper_roi, upper_roi,
  ifelse(nykaa_clean$roi < lower_roi, lower_roi,
         nykaa_clean$roi)
)

# ---------------------------------------------
# VISUALIZATION
# ---------------------------------------------

ggplot(nykaa_clean, aes(x = log_revenue_final)) +
  geom_histogram(bins = 30) +
  ggtitle("Log Revenue Distribution")


ggplot(nykaa_clean, aes(x = roi_final)) +
  geom_histogram(bins = 30) +
  ggtitle("ROI Distribution")

ggplot(nykaa_clean,
       aes(x = acquisition_cost,
           y = roi_final)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Acquisition Cost vs ROI")

# Insight:
# Shows whether higher spending improves ROI
# =============================================
# PARAMETRIC TESTS
# =============================================

# ---------------------------------------------
# TEST 1: ONE SAMPLE T-TEST
# Is average ROI equal to 0?
# ---------------------------------------------

test1 <- t.test(nykaa_clean$roi_final,
                mu = 0)

test1

# H0: Mean ROI = 0 (campaigns are not profitable)
# H1: Mean ROI ≠ 0 (campaigns are profitable)

# Insight:
# If p-value < 0.05 -> Average ROI significantly different from 0
# If p-value > 0.05 -> ROI not significantly different from 0

# Conclusion
# p-value < 0.05, so we reject H0.
# Average ROI is significantly greater than zero.
# Nykaa marketing campaigns are statistically profitable.


# ---------------------------------------------
# TEST 2: INDEPENDENT T-TEST
# Compare ROI between two campaign types
# ---------------------------------------------
nykaa_ttest <- nykaa_clean %>%
  filter(campaign_type %in% c("Social Media", "Email"))

test2 <- t.test(roi_final ~ campaign_type,
                data = nykaa_ttest)

test2

# H0: Mean ROI is same for Email and Social Media campaigns
# H1: Mean ROI differs between Email and Social Media campaigns

# Insight:
# p < 0.05 -> ROI differs across campaign types
# p > 0.05 -> No significant difference

# Conclusion:
# p-value = 0.449 > 0.05, so we fail to reject H0.
# Mean ROI of Email (2.26) and Social Media (2.29) is nearly similar.
# There is no significant difference in ROI between Email and Social Media campaigns.
# Both campaign types perform similarly in terms of profitability.


# ---------------------------------------------
# TEST 3: ANOVA
# Compare revenue across regions
# ---------------------------------------------

test3 <- aov(log_revenue_final ~ channel_used,
             data = nykaa_clean)

summary(test3)


# H0: Mean revenue is same across all marketing channels
# H1: Mean revenue differs across marketing channels

# Insight:
# p < 0.05 -> Revenue differs across regions
# p > 0.05 -> No regional difference

# Conclusion:
# If p-value < 0.05, we reject H0 and conclude that revenue differs across channels.
# If p-value > 0.05, revenue is similar across channels.
# This helps identify which marketing channel generates higher revenue.


# ---------------------------------------------
# TEST 4: PEARSON CORRELATION
# Marketing Spend vs Revenue
# ---------------------------------------------

test4 <- cor.test(nykaa_clean$acquisition_cost,
                  nykaa_clean$log_revenue_final,
                  method = "pearson")

test4

# H0: No relationship between acquisition cost and revenue
# H1: Significant relationship between acquisition cost and revenue

# Insight:
# Positive value -> spend increases revenue
# Negative -> inverse relation
# p < 0.05 -> significant relationship


# Conclusiom:
# p-value < 0.05, so we reject H0.
# Correlation = -0.65, which shows a strong negative relationship.
# As acquisition cost increases, revenue tends to decrease.
# This indicates inefficient spending or high-cost campaigns generating lower revenue.


# =============================================
# NON PARAMETRIC TESTS
# =============================================

# ---------------------------------------------
# TEST 5: MANN WHITNEY TEST
# ROI across campaign types
# ---------------------------------------------

nykaa_mw <- nykaa_clean %>%
  filter(campaign_type %in% c("Email", "Social Media"))

test5 <- wilcox.test(roi_final ~ campaign_type,
                     data = nykaa_mw)

test5

# H0: ROI distribution is same for Email and Social Media campaigns
# H1: ROI distribution differs between Email and Social Media campaigns

# Insight:
# p < 0.05 -> ROI differs across campaign types
# Non-parametric alternative to t-test

# Conclusion:
# p-value = 0.2503 > 0.05, so we fail to reject H0.
# ROI distribution of Email and Social Media campaigns is similar.
# There is no significant difference in profitability between the two campaign types.
# Non-parametric test confirms the t-test result.

# ---------------------------------------------
# TEST 6: KRUSKAL WALLIS TEST
# Revenue across regions
# ---------------------------------------------

test6 <- kruskal.test(log_revenue_final ~ channel_used,
                      data = nykaa_clean)

test6

# H0: Revenue distribution is same across all marketing channels
# H1: Revenue distribution differs across marketing channels

# Insight:
# p < 0.05 -> Revenue differs across regions
# Non-parametric alternative to ANOVA

# Conclusion:
# If p-value < 0.05, we reject H0 and conclude that revenue differs across channels.
# If p-value > 0.05, revenue is similar across channels.
# Kruskal-Wallis test checks revenue differences without normality assumption.

# ---------------------------------------------
# TEST 7: SPEARMAN CORRELATION
# Marketing Spend vs ROI
# ---------------------------------------------

test7 <- cor.test(nykaa_clean$acquisition_cost,
                  nykaa_clean$roi_final,
                  method = "spearman")

test7

# H0: No relationship between acquisition cost and ROI
# H1: Significant relationship between acquisition cost and ROI

# Insight:
# Measures monotonic relationship
# p < 0.05 -> significant relationship

# Conclusion:
# p-value < 0.05, so we reject H0.
# Spearman correlation = -0.93, which shows a very strong negative relationship.
# As acquisition cost increases, ROI decreases significantly.
# This indicates high acquisition cost campaigns reduce profitability.
# Nykaa should control acquisition cost to improve ROI.



# ---------------------------------------------
# TEST 8: CHI-SQUARE TEST
# Campaign Type vs Region
# ---------------------------------------------

tbl <- table(nykaa_clean$campaign_type,
             nykaa_clean$customer_segment)

test8 <- chisq.test(tbl)

test8

# H0: Campaign type and customer segment are independent
# H1: Campaign type and customer segment are associated

# Insight:
# p < 0.05 -> Campaign type associated with region
# p > 0.05 -> No association

# Conclusion:
# If p-value < 0.05, we reject H0 and conclude campaign type depends on customer segment.
# If p-value > 0.05, campaign type and customer segment are independent.
# This helps understand whether different customer segments prefer different campaign types.

