# Datathlon: preliminary analysis #
# 07/04/2025


# Clear the workspace and load packages
rm(list = ls())

# List of required packages
packages <- c(
  "tidyverse", "ggplot2", "stargazer", "data.table", "readr", "labelled", "purrr", "bit64", "readxl")

# Function to install missing packages
install_missing_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Install and load required packages
install_missing_packages(packages)

# Load the packages into the session
lapply(packages, library, character.only = TRUE)

# Print a message to confirm all packages are loaded
cat("All required packages are successfully loaded!\n")



################################################################################

# Set wd
setwd("/Users/leonardogeretto/Documents/PSE2024/Datathlon/Data/Bangladesh")

library(haven)

# Load 2020 individual refugee and host data 

R_ind_2020 <- read_dta("2020/dta/R_individual_2020.dta")
H_ind_2020 <- read_dta("2020/dta/H_individual_2020.dta")


# Load 2020 household refugee and host data
R_hh_2020 <- read_dta("2020/dta/R_household_2020.dta")
H_hh_2020 <- read_dta("2020/dta/H_household_2020.dta")


# Load 2021 individual refugee and host data 
R_ind_2021 <- read_dta("2021/dta/R_individual_2021.dta")
H_ind_2021 <- read_dta("2021/dta/H_individual_2021.dta")

# Load 2021 household refugee and host data
R_hh_2021 <- read_dta("2021/dta/R_household_2021.dta", encoding = "latin1")
H_hh_2021 <- read_dta("2021/dta/H_household_2021.dta", encoding = "latin1")


# Load 2023 individual refugee and host data 
R_ind_2023 <- read_dta("2023/dta/R_individual_2023.dta")
H_ind_2023 <- read_dta("2023/dta/H_individual_2023.dta")

# Load 2021 household refugee and host data
R_hh_2023 <- read_dta("2023/dta/R_household_2023.dta")
H_hh_2023 <- read_dta("2023/dta/H_household_2023.dta")


######## Preliminary analysis of 2020 data ########

# Check the structure of the data
str(R_ind_2020)
str(H_ind_2020)
str(R_hh_2020)
str(H_hh_2020)


# No variable labels
sapply(R_ind_2020, function(x) attr(x, "label"))
sapply(H_ind_2020, function(x) attr(x, "label"))
sapply(R_hh_2020, function(x) attr(x, "label"))
sapply(H_hh_2020, function(x) attr(x, "label"))


# Create function counting the share of NAs or empty cells over all observations and all columns
share_missing_or_empty <- function(df) {
  total_cells <- nrow(df) * ncol(df)
  
  # Count NAs or empty strings
  missing_or_empty <- sum(sapply(df, function(col) sum(is.na(col) | col == "", na.rm = T)), na.rm = T)
  
  # Share of missing or empty
  share <- missing_or_empty / total_cells
  return(share)
}


# # Apply the function to each dataset
# share_R_ind_2020 <- share_missing_or_empty(R_ind_2020)
# share_H_ind_2020 <- share_missing_or_empty(H_ind_2020)
# share_R_hh_2020 <- share_missing_or_empty(R_hh_2020)
# share_H_hh_2020 <- share_missing_or_empty(H_hh_2020)
# # Print the results
# cat("Share of missing or empty cells in R_ind_2020:", share_R_ind_2020, "\n") # 68% of cells are missing or empty
# cat("Share of missing or empty cells in H_ind_2020:", share_H_ind_2020, "\n") # 66% of cells are missing or empty
# cat("Share of missing or empty cells in R_hh_2020:", share_R_hh_2020, "\n") # 22% of cells are missing or empty
# cat("Share of missing or empty cells in H_hh_2020:", share_H_hh_2020, "\n") # 36% of cells are missing or empty


# Do the same for 2021 and 2023 data

years <- c("2020", "2021", "2023")

for (year in years) {
  # Load the data
  R_ind <- read_dta(paste0(year, "/dta/R_individual_", year, ".dta"))
  H_ind <- read_dta(paste0(year, "/dta/H_individual_", year, ".dta"))
  R_hh <- read_dta(paste0(year, "/dta/R_household_", year, ".dta"))
  H_hh <- read_dta(paste0(year, "/dta/H_household_", year, ".dta"))
  
  # Apply the function to each dataset
  share_R_ind <- share_missing_or_empty(R_ind)
  share_H_ind <- share_missing_or_empty(H_ind)
  share_R_hh <- share_missing_or_empty(R_hh)
  share_H_hh <- share_missing_or_empty(H_hh)
  
  # Print the results
  cat("Share of missing or empty cells in R_ind_", year, ":", share_R_ind, "\n")
  cat("Share of missing or empty cells in H_ind_", year, ":", share_H_ind, "\n")
  cat("Share of missing or empty cells in R_hh_", year, ":", share_R_hh, "\n")
  cat("Share of missing or empty cells in H_hh_", year, ":", share_H_hh, "\n")
}

#### 2020 data 
# Share of missing or empty cells in R_ind_ 2020 : 0.6809158 
# Share of missing or empty cells in H_ind_ 2020 : 0.6658559 
# Share of missing or empty cells in R_hh_ 2020 : 0.223398 
# Share of missing or empty cells in H_hh_ 2020 : 0.3630909 


#### 2021 data
# Share of missing or empty cells in R_ind_ 2021 : 0.4846302 
# Share of missing or empty cells in H_ind_ 2021 : 0.438309 
# Share of missing or empty cells in R_hh_ 2021 : 0.4027583 
# Share of missing or empty cells in H_hh_ 2021 : 0.4602262 


#### 2023 data
# Share of missing or empty cells in R_ind_ 2023 : 0.610502 
# Share of missing or empty cells in H_ind_ 2023 : 0.6299208 
# Share of missing or empty cells in R_hh_ 2023 : 0.2691797 
# Share of missing or empty cells in H_hh_ 2023 : 0.244747 


# Filter mobility issues variables

mobility_2021_r_children <- R_hh_2021 %>% 
  select(starts_with("mobility_issues_children"))

mobility_2021_h <- H_hh_2021 %>%
  select(starts_with("mobility_issues_children"))


attr(mobility_2021_r_children$mobility_issues_children_no_i_1, "label") <- "Is there any issue? 1 = No, 2 = Yes"

str(mobility_2021_r_children$mobility_issues_children_no_i_1)

mobility_2021_r_children_encoded <- mobility_2021_r_children %>%
  mutate(across(everything(), ~ case_when(
    as.character(.) == "1" ~ 0,
    as.character(.) == "2" ~ 1,
    TRUE ~ NA_real_
  )))

#  
mean(mobility_2021_r_children_encoded$mobility_issues_children_no_i_1, na.rm = TRUE) # 0.7071202


# No access to previously accessible facilities due to fencing 
mean(mobility_2021_r_children_encoded$mobility_issues_children_acce_1, na.rm = TRUE) # 0.002594408


# Danger due to harassement 
mean(mobility_2021_r_children_encoded$mobility_issues_children_dang_2, na.rm = TRUE) # 0.00461228







############# Health barriers ##################


### Refugee

hbr <- R_hh_2021 %>% 
  select(pseudo_id, starts_with("health_barriers_not")) %>%
  mutate(across(2:ncol(.), ~ case_when(
    as.character(.) == "1" ~ 0,
    as.character(.) == "2" ~ 1,
    TRUE ~ NA_real_
  )))


#### no challenge?
attr(hbr$health_barriers_not_acc_no_ch_1, "label") <- "Any challenge in accessing healthcare? 1 = No, 0 = Yes"

sum(!is.na(hbr$health_barriers_not_acc_no_ch_1))/nrow(hbr) # only 341 observations, 90% is missing 
mean(hbr$health_barriers_not_acc_no_ch_1, na.rm = TRUE) # 0.2140762 reports no challenge ==> 80% reports challenges

#### Info constraints on where/how access health facility
attr(hbr$health_barriers_not_acc_dont__1, "label") <- "Did not know where/how access healthcare: 0 = No, 1 = Yes"
unique(hbr$health_barriers_not_acc_dont__1) # 0 = No, 1 = Yes

#how many not missing?
sum(!is.na(hbr$health_barriers_not_acc_no_ch_1))/nrow(hbr)
mean(hbr$health_barriers_not_acc_dont__1, na.rm = TRUE) # only 0.3% report this as a reason

#### No functional health facility nearby 
attr(hbr$health_barriers_not_acc_no_fu_1, "label") <- "No functional health facility nearby: 0 = No, 1 = Yes"
unique(hbr$health_barriers_not_acc_no_fu_1) # 0 = No, 1 = Yes
mean(hbr$health_barriers_not_acc_no_fu_1, na.rm = TRUE) # 0.1260997 12% report this is the reason

#### Specific treatment not available
attr(hbr$health_barriers_not_acc_treat_1, "label") <- "Specific treatment not available: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_treat_1, na.rm = TRUE) # 0.3665689 report this is the reason

#### Long wait/overcrowded 
attr(hbr$health_barriers_not_acc_long__1, "label") <- "Long wait/overcrowded: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_long__1, na.rm = TRUE) # 0.4516129 report this is the reason

#### No money to pay for treatment
attr(hbr$health_barriers_not_acc_canno_1, "label") <- "No money to pay for treatment: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_canno_1, na.rm = TRUE) # 0.008797654 report this is the reason

#### Health services are too far
attr(hbr$health_barriers_not_acc_healt_1, "label") <- "Health services are too far: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_healt_1, na.rm = TRUE) # 0.03519062 report this is the reason

#### Inaccessible due to road conditions
attr(hbr$health_barriers_not_acc_inacc_1, "label") <- "Inaccessible due to road conditions: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_inacc_1, na.rm = TRUE) # 0.008797654 report this is the reason

#### Lack of transport at night
attr(hbr$health_barriers_not_acc_lack__1, "label") <- "Lack of transport: 0 = No, 1 = Yes"
mean(hbr$health_barriers_not_acc_lack__1, na.rm = TRUE) 


# Filter out NAs
hbr_long <- hbr %>% 
  filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(hh_id = row_number()) %>% 
  pivot_longer(cols = -hh_id, names_to = "health_barrier", values_to = "value")

hbr_long <- hbr_long %>% 
  group_by(hh_id) %>% 
  mutate(total_barriers = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    many_barriers = ifelse(total_barriers > 0, 1, 0)
  )
  
  
summary(hbr_long$total_barriers) # 0 = 0, 1 = 1, 2 = 2, 3 = 3, 4 = 4, 5 = 5, 6 = 6, 7 = 7

####################### hosts ##############################
hbh <- H_hh_2021 %>%
  select(pseudo_parent_id, starts_with("health_barriers")) %>%
  mutate(across(2:ncol(.), ~ case_when(
    as.character(.) == "1" ~ 0,
    as.character(.) == "2" ~ 1,
    TRUE ~ NA_real_
  )))




#### no challenge?
attr(hbh$health_barriers_not_acc_no_ch_1, "label") <- "Any challenge in accessing healthcare? 1 = No, 0 = Yes"

sum(!is.na(hbh$health_barriers_not_acc_no_ch_1))/nrow(hbh) # only 341 observations, 90% is missing 
mean(hbh$health_barriers_not_acc_no_ch_1, na.rm = TRUE) # 0.3161034 reports no challenge ==> check if significantly more than refugees

#### Info constraints on where/how access health facility
attr(hbh$health_barriers_not_acc_dont__1, "label") <- "Did not know where/how access healthcare: 0 = No, 1 = Yes"
unique(hbh$health_barriers_not_acc_dont__1) # 0 = No, 1 = Yes

#how many not missing?
sum(!is.na(hbh$health_barriers_not_acc_no_ch_1))/nrow(hbh)
mean(hbh$health_barriers_not_acc_dont__1, na.rm = TRUE) # only 0.001988072 report this as a reason

#### No functional health facility nearby 
attr(hbh$health_barriers_not_acc_no_fu_1, "label") <- "No functional health facility nearby: 0 = No, 1 = Yes"
unique(hbh$health_barriers_not_acc_no_fu_1) # 0 = No, 1 = Yes
mean(hbh$health_barriers_not_acc_no_fu_1, na.rm = TRUE) # 0.07952286 12% report this is the reason

#### Specific treatment not available
attr(hbh$health_barriers_not_acc_treat_1, "label") <- "Specific treatment not available: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_treat_1, na.rm = TRUE) # 0.2166998 report this is the reason

#### Long wait/overcrowded 
attr(hbh$health_barriers_not_acc_long__1, "label") <- "Long wait/overcrowded: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_long__1, na.rm = TRUE) # 0.2624254 report this is the reason

#### No money to pay for treatment
attr(hbh$health_barriers_not_acc_canno_1, "label") <- "No money to pay for treatment: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_canno_1, na.rm = TRUE) # 0.07157058 report this is the reason

#### Health services are too far
attr(hbh$health_barriers_not_acc_healt_1, "label") <- "Health services are too far: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_healt_1, na.rm = TRUE) # 0.1351889 report this is the reason

#### Inaccessible due to road conditions
attr(hbh$health_barriers_not_acc_inacc_1, "label") <- "Inaccessible due to road conditions: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_inacc_1, na.rm = TRUE) # 0.02982107 report this is the reason

#### Lack of transport at night
attr(hbh$health_barriers_not_acc_lack__1, "label") <- "Lack of transport: 0 = No, 1 = Yes"
mean(hbh$health_barriers_not_acc_lack__1, na.rm = TRUE)  # 0.06361829


######### REFUGEE
##### Merge with individual data to know if the refugee household has children

children <- R_ind_2021 %>% 
  mutate(
    has_children = ifelse(individual_age < 4, 1, 0)
  ) %>% 
  select(pseudo_id, has_children)

length(unique(children$pseudo_id)) == nrow(children)


# merge ind and hh data 
R_health_child <- hbr %>% 
  left_join(children, by = "pseudo_id") %>% 
  filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(
    status = "refugee"
  )


# do household with children experience more difficulty in accessing health treatment? 
# no specific constraints for households with kids

reg_child <- lm(health_barriers_not_acc_no_ch_1 ~ has_children, data = R_health_child)
summary(reg_child) # household with seem to not experience more barriers 

reg_child1 <- lm(health_barriers_not_acc_long__1 ~ has_children, data = R_health_child)
summary(R_health_child$has_children)

reg_child2 <- lm(health_barriers_not_acc_treat_1 ~ has_children, data = R_health_child)
summary(reg_child2) 

reg_child3 <- lm(health_barriers_not_acc_no_fu_1 ~ has_children, data = R_health_child)
summary(reg_child3) 

reg_child4 <- lm(health_barriers_not_acc_healt_1 ~ has_children, data = R_health_child)
summary(reg_child4)



##### HOSTS
children_H <- H_ind_2021 %>% 
  mutate(
    has_children = ifelse(individual_age < 4, 1, 0)
  ) %>% 
  select(pseudo_parent_id, has_children)

length(unique(children_H$pseudo_id))== nrow(children_H)


# merge ind and hh data 
H_health_child <- hbh %>% 
  left_join(children_H, by = "pseudo_parent_id") %>% 
  filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(
    status = "host"
  )


# do household with children experience more difficulty in accessing health treatment? 
# no specific constraints for households with kids

reg_child_H <- lm(health_barriers_not_acc_no_ch_1 ~ has_children, data = H_health_child)
summary(reg_child_H) # household with seem to not experience more barriers 

reg_child1_H <- lm(health_barriers_not_acc_long__1 ~ has_children, data = H_health_child)
summary(reg_child1_H)

reg_child2_H <- lm(health_barriers_not_acc_treat_1 ~ has_children, data = H_health_child)
summary(reg_child2_H) 

reg_child3_H <- lm(health_barriers_not_acc_no_fu_1 ~ has_children, data = H_health_child)
summary(reg_child3_H) 

reg_child4_H <- lm(health_barriers_not_acc_healt_1 ~ has_children, data = H_health_child)
summary(reg_child4_H)


############ Merge host and refugee datasets ################
# Combine the two datasets
combined_data <- bind_rows(R_health_child, H_health_child)

combined_data <- combined_data %>% 
  mutate(
    refugee = ifelse(status == "refugee", 1, 0)
  )

reg_combined <- lm(health_barriers_not_acc_no_ch_1 ~ refugee + has_children, data = combined_data)
summary(reg_combined)

# filter for children only
combined_data_children <- combined_data %>% 
  filter(has_children == 1) %>% 
  mutate(
    refugee = ifelse(status == "refugee", 1, 0)
  )


####### regression and create graphs 

#### no challenge?
reg_combined <- lm(health_barriers_not_acc_no_ch_1 ~ refugee, data = combined_data_children)
summary(reg_combined) # household with seem to not experience more barriers

# Extract estimates and standard errors
est <- coef(summary(reg_combined))
mean_host <- est["(Intercept)", "Estimate"]
se_host <- est["(Intercept)", "Std. Error"]

mean_refugee <- mean_host + est["refugee", "Estimate"]
se_refugee <- sqrt(
  vcov(reg_combined)["(Intercept)", "(Intercept)"] +
    vcov(reg_combined)["refugee", "refugee"] +
    2 * vcov(reg_combined)["(Intercept)", "refugee"]
)

# 95% confidence intervals
ci_host <- c(mean_host - 1.96 * se_host, mean_host + 1.96 * se_host)
ci_refugee <- c(mean_refugee - 1.96 * se_refugee, mean_refugee + 1.96 * se_refugee)

# Create data frame
plot_data <- data.frame(
  group = factor(c("Host", "Refugee"), levels = c("Host", "Refugee")),
  mean = c(mean_host, mean_refugee),
  ci_lower = c(ci_host[1], ci_refugee[1]),
  ci_upper = c(ci_host[2], ci_refugee[2])
)



ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "black") +
  scale_fill_manual(values = c("Host" = "#9ecae1", "Refugee" = "#fc9272")) +
  labs(
    x = "Refugee Status",
    y = "Share Reporting No Challenge",
    title = "Access to Healthcare by Refugee Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  ylim(0, 1)



#### no challenge?
reg_combined <- lm(health_barriers_not_acc_no_ch_1 ~ refugee, data = combined_data_children)
summary(reg_combined) # household with seem to not experience more barriers

# Extract estimates and standard errors
est <- coef(summary(reg_combined))
mean_host <- est["(Intercept)", "Estimate"]
se_host <- est["(Intercept)", "Std. Error"]

mean_refugee <- mean_host + est["refugee", "Estimate"]
se_refugee <- sqrt(
  vcov(reg_combined)["(Intercept)", "(Intercept)"] +
    vcov(reg_combined)["refugee", "refugee"] +
    2 * vcov(reg_combined)["(Intercept)", "refugee"]
)

# 95% confidence intervals
ci_host <- c(mean_host - 1.96 * se_host, mean_host + 1.96 * se_host)
ci_refugee <- c(mean_refugee - 1.96 * se_refugee, mean_refugee + 1.96 * se_refugee)

# Create data frame
plot_data <- data.frame(
  group = factor(c("Host", "Refugee"), levels = c("Host", "Refugee")),
  mean = c(mean_host, mean_refugee),
  ci_lower = c(ci_host[1], ci_refugee[1]),
  ci_upper = c(ci_host[2], ci_refugee[2])
)



ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "black") +
  scale_fill_manual(values = c("Host" = "#9ecae1", "Refugee" = "#fc9272")) +
  labs(
    x = "Refugee Status",
    y = "Share Reporting No Challenge",
    title = "Access to Healthcare by Refugee Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  ylim(0, 1)



################################################################################
################################################################################
################################################################################

# Create plot for all dependent variables 

# # Define your dependent variables and labels
# dep_vars <- c("health_barriers_not_acc_no_ch_1", "health_barriers_not_acc_dont__1", "health_barriers_not_acc_no_fu_1", 
#               "health_barriers_not_acc_treat_1", "health_barriers_not_acc_long__1", "health_barriers_not_acc_canno_1", 
#               "health_barriers_not_acc_healt_1", "health_barriers_not_acc_inacc_1", "health_barriers_not_acc_lack__1")  # replace
# var_labels <- c(
#   health_barriers_not_acc_no_ch_1 = "No Challenge in Accessing Healthcare",
#   health_barriers_not_acc_dont__1 = "Did not know where/how access healthcare: 0 = No, 1 = Yes",
#   health_barriers_not_acc_no_fu_1 = "No functional health facility nearby: 0 = No, 1 = Yes",
#   health_barriers_not_acc_treat_1 = "Specific treatment not available: 0 = No, 1 = Yes",
#   health_barriers_not_acc_long__1 = "Long wait/overcrowded: 0 = No, 1 = Yes",
#   health_barriers_not_acc_canno_1 = "No money to pay for treatment: 0 = No, 1 = Yes",
#   health_barriers_not_acc_healt_1 = "Health services are too far: 0 = No, 1 = Yes",
#   health_barriers_not_acc_inacc_1 = "Inaccessible due to road conditions: 0 = No, 1 = Yes",
#   health_barriers_not_acc_lack__1 = "Lack of transport: 0 = No, 1 = Yes"
# )
# 
# # Loop over each variable
# for (dv in dep_vars) {
#   
#   # Run regression
#   formula <- as.formula(paste(dv, "~ refugee"))
#   reg <- lm(formula, data = combined_data_children)
#   est <- coef(summary(reg))
#   vcov_mat <- vcov(reg)
#   
#   # Compute means and standard errors
#   mean_host <- est["(Intercept)", "Estimate"]
#   se_host <- est["(Intercept)", "Std. Error"]
#   
#   mean_refugee <- mean_host + est["refugee", "Estimate"]
#   se_refugee <- sqrt(
#     vcov_mat["(Intercept)", "(Intercept)"] +
#       vcov_mat["refugee", "refugee"] +
#       2 * vcov_mat["(Intercept)", "refugee"]
#   )
#   
#   # 95% confidence intervals
#   ci_host <- c(mean_host - 1.96 * se_host, mean_host + 1.96 * se_host)
#   ci_refugee <- c(mean_refugee - 1.96 * se_refugee, mean_refugee + 1.96 * se_refugee)
#   
#   # Data for plot
#   plot_data <- data.frame(
#     group = factor(c("Host", "Refugee"), levels = c("Host", "Refugee")),
#     mean = c(mean_host, mean_refugee),
#     ci_lower = c(ci_host[1], ci_refugee[1]),
#     ci_upper = c(ci_host[2], ci_refugee[2])
#   )
#   
#   # Create plot
#   p <- ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
#     geom_col(width = 0.6, color = "black") +
#     geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "black") +
#     scale_fill_manual(values = c("Host" = "#9ecae1", "Refugee" = "#fc9272")) +
#     labs(
#       x = "Refugee Status",
#       y = "Share Reporting No Challenge",
#       title = var_labels[[dv]]
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       axis.title = element_text(color = "black"),
#       axis.text = element_text(color = "black"),
#       axis.line = element_line(color = "black"),
#       panel.grid.major.y = element_line(color = "grey90"),
#       panel.grid.minor = element_blank(),
#       panel.grid.major.x = element_blank(),
#       legend.position = "none",
#       plot.title = element_text(face = "bold", hjust = 0.5)
#     ) +
#     ylim(0, 1)
#   
#   # Save plot to file
#   filename <- paste0("Graphs/plot_", dv, ".png")
#   ggsave(filename, plot = p, width = 6, height = 5, dpi = 300, bg = "white")
# }
# 
# 
# 
# 
# 
reg_combined <- lm(health_barriers_not_acc_canno_1 ~ 1, data = combined_data_children %>% filter(refugee ==1))
summary(reg_combined) # household with seem to not experience more barriers

# Define your dependent variables and labels
dep_vars <- c("health_barriers_not_acc_no_ch_1", "health_barriers_not_acc_dont__1", "health_barriers_not_acc_no_fu_1", 
              "health_barriers_not_acc_treat_1", "health_barriers_not_acc_long__1", "health_barriers_not_acc_canno_1", 
              "health_barriers_not_acc_healt_1", "health_barriers_not_acc_inacc_1", "health_barriers_not_acc_lack__1")  # replace
var_labels <- c(
  health_barriers_not_acc_no_ch_1 = "No Challenge in Accessing Healthcare",
  health_barriers_not_acc_dont__1 = "Did not know where/how access healthcare",
  health_barriers_not_acc_no_fu_1 = "No functional health facility nearby",
  health_barriers_not_acc_treat_1 = "Specific treatment not available",
  health_barriers_not_acc_long__1 = "Long wait/overcrowded",
  health_barriers_not_acc_canno_1 = "No money to pay for treatment",
  health_barriers_not_acc_healt_1 = "Health services are too far",
  health_barriers_not_acc_inacc_1 = "Inaccessible due to road conditions",
  health_barriers_not_acc_lack__1 = "Lack of transport"
)

# Ensure the 'Graphs' directory exists
if (!dir.exists("Graphs/2021")) {
  dir.create("Graphs/2021")
}

# Loop over each dependent variable
for (dv in dep_vars) {
  
  # Subset data for hosts and refugees
  data_host <- subset(combined_data_children, refugee == 0)
  data_refugee <- subset(combined_data_children, refugee == 1)
  
  # Run regression for hosts
  reg_host <- lm(as.formula(paste(dv, "~ 1")), data = data_host)  # Intercept only
  est_host <- coef(summary(reg_host))
  vcov_host <- vcov(reg_host)
  
  # Run regression for refugees
  reg_refugee <- lm(as.formula(paste(dv, "~ 1")), data = data_refugee)  # Intercept only
  est_refugee <- coef(summary(reg_refugee))
  vcov_refugee <- vcov(reg_refugee)
  
  # Calculate means and standard errors for both groups
  mean_host <- est_host["(Intercept)", "Estimate"]
  se_host <- est_host["(Intercept)", "Std. Error"]
  
  mean_refugee <- est_refugee["(Intercept)", "Estimate"]
  se_refugee <- est_refugee["(Intercept)", "Std. Error"]
  
  # 95% confidence intervals for both groups
  ci_host <- c(mean_host - 1.96 * se_host, mean_host + 1.96 * se_host)
  ci_refugee <- c(mean_refugee - 1.96 * se_refugee, mean_refugee + 1.96 * se_refugee)
  
  # Combine results for plot
  plot_data <- data.frame(
    group = factor(c("Host", "Refugee"), levels = c("Host", "Refugee")),
    mean = c(mean_host, mean_refugee),
    ci_lower = c(ci_host[1], ci_refugee[1]),
    ci_upper = c(ci_host[2], ci_refugee[2])
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "black") +
    scale_fill_manual(values = c("Host" = "#9ecae1", "Refugee" = "#fc9272")) +
    labs(
      x = "Refugee Status",
      y = "Share of HH",
      title = var_labels[dv]  # Correct index for the title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(color = "black"),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    ylim(0, 1)
  
  # Save plot to file
  filename <- paste0("Graphs/2021/plot_", dv, ".png")
  ggsave(filename, plot = p, width = 6, height = 5, dpi = 300, bg = "white")
}



# any different between households with and without children?
reg_combined <- lm(health_barriers_not_acc_no_ch_1 ~ refugee + has_children, data = combined_data)
summary(reg_combined)



########## With and without children ########## 
# Loop over each dependent variable
for (dv in dep_vars) {
  
  # Define the four groups
  groups <- list(
    "Host - No Children" = subset(combined_data, refugee == 0 & has_children == 0),
    "Host - Has Children" = subset(combined_data, refugee == 0 & has_children == 1),
    "Refugee - No Children" = subset(combined_data, refugee == 1 & has_children == 0),
    "Refugee - Has Children" = subset(combined_data, refugee == 1 & has_children == 1)
  )
  
  # Initialize vectors for plot
  means <- c()
  ses <- c()
  ci_lowers <- c()
  ci_uppers <- c()
  labels <- names(groups)
  
  for (label in labels) {
    data_group <- groups[[label]]
    reg <- lm(as.formula(paste(dv, "~ 1")), data = data_group)
    est <- coef(summary(reg))
    se <- est["(Intercept)", "Std. Error"]
    mean <- est["(Intercept)", "Estimate"]
    ci_lower <- mean - 1.96 * se
    ci_upper <- mean + 1.96 * se
    
    means <- c(means, mean)
    ses <- c(ses, se)
    ci_lowers <- c(ci_lowers, ci_lower)
    ci_uppers <- c(ci_uppers, ci_upper)
  }
  
  # Combine into plot data
  plot_data <- data.frame(
    group = factor(labels, levels = labels),
    mean = means,
    ci_lower = ci_lowers,
    ci_upper = ci_uppers
  )
  
  # Plot
  p <- ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "black") +
    scale_fill_manual(values = c(
      "Host - No Children" = "#bdd7e7",
      "Host - Has Children" = "#6baed6",
      "Refugee - No Children" = "#fcae91",
      "Refugee - Has Children" = "#fb6a4a"
    )) +
    labs(
      x = "Group",
      y = "Share of HH",
      title = var_labels[dv]
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    ylim(0, 1)
  
  # Save
  filename <- paste0("Graphs/2021/Children/plot_refugee_children_", dv, ".png")
  ggsave(filename, plot = p, width = 7, height = 5, dpi = 300, bg = "white")
}



