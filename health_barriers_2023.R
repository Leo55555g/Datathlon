# Datathlon: preliminary analysis #
# 30/04/2025


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


################################################################################

############# Health barriers 2021 ##################


### Refugee ###

hbr <- R_hh_2023 %>% 
  select(pseudo_parent_id, starts_with("health_barriers")) %>%
  mutate(across(2:ncol(.), ~ case_when(
    as.character(.) == "1" ~ 0,
    as.character(.) == "2" ~ 1,
    TRUE ~ NA_real_
  )))


### Select hh with kids

children <- R_ind_2023 %>% 
  group_by(pseudo_parent_id) %>%
  mutate(
    has_children = if_else(any(ind_age < 4), 1, 0)
  ) %>% 
  ungroup() %>% 
  select(pseudo_parent_id, has_children) 


# merge ind and hh data 
R_health_child <- hbr %>% 
  left_join(children, by = "pseudo_parent_id") %>% 
  #filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(
    status = "refugee"
  ) %>% 
  distinct()

length(unique(R_health_child$pseudo_parent_id))==nrow(R_health_child) #true


### Host ###
hbh <- H_hh_2023 %>% 
  select(pseudo_parent_id, starts_with("health_barriers")) %>%
  mutate(across(2:ncol(.), ~ case_when(
    as.character(.) == "1" ~ 0,
    as.character(.) == "2" ~ 1,
    TRUE ~ NA_real_
  )))

### Select hh with kids
children <- H_ind_2023 %>% 
  group_by(pseudo_parent_id) %>%
  mutate(
    has_children = if_else(any(ind_age < 4), 1, 0)
  ) %>% 
  ungroup() %>% 
  select(pseudo_parent_id, has_children) 

# merge ind and hh data
H_health_child <- hbh %>% 
  left_join(children, by = "pseudo_parent_id") %>% 
  #filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(
    status = "host"
  ) %>% 
  distinct()
length(unique(H_health_child$pseudo_parent_id))==nrow(H_health_child) #true


############ Merge host and refugee datasets ################
# Combine the two datasets
combined_data <- bind_rows(R_health_child, H_health_child)%>% 
  mutate(
    refugee = ifelse(status == "refugee", 1, 0)
  )

# filter for children only
combined_data_children <- combined_data %>% 
  filter(has_children == 1) %>% 
  mutate(
    refugee = ifelse(status == "refugee", 1, 0)
  )


######## Graphs
reg_combined <- lm(health_barriers_no_issue ~ refugee, data = combined_data_children )
summary(reg_combined) # refugee households are less likely to report no issues in accessing healthcare

# Define your dependent variables and labels
dep_vars <- c("health_barriers_no_issue", "health_barriers_no_functional_1", "health_barriers_treatment_med_1", 
              "health_barriers_long_waiting", "health_barriers_health_servic_1", "health_barriers_cannot_afford_1", 
              "health_barriers_cannot_afford_2", "health_barriers_cannot_afford_3", "health_barriers_no_time")  # replace
var_labels <- c(
  health_barriers_no_issue = "No Challenge in Accessing Healthcare",
  health_barriers_no_functional_1 = "No functional health facility nearby",
  health_barriers_treatment_med_1 = "Specific treatment unavailable",
  health_barriers_long_waiting = "Long waiting time for the service / overcrowded",
  health_barriers_health_servic_1 = "Health facility is too far", 
  health_barriers_cannot_afford_2 = "Cannot afford cost of treatment", 
  health_barriers_cannot_afford_1 = "Cannot afford cost of consultation", 
  health_barriers_cannot_afford_3 = "Cannot afford cost of transportation", 
  health_barriers_no_time = "Could not take time off work"
)

# Ensure the 'Graphs' directory exists
if (!dir.exists("Graphs/2023")) {
  dir.create("Graphs/2023")
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
  filename <- paste0("Graphs/2023/plot_", dv, ".png")
  ggsave(filename, plot = p, width = 6, height = 5, dpi = 300, bg = "white")
}





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
  filename <- paste0("Graphs/2023/Children/plot_refugee_children_", dv, ".png")
  ggsave(filename, plot = p, width = 7, height = 5, dpi = 300, bg = "white")
}











