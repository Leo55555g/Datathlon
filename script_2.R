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

############# Health barriers 2023 ##################


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
# dep_vars <- c("health_barriers_no_issue", "health_barriers_no_functional_1", "health_barriers_treatment_med_1", 
#               "health_barriers_long_waiting", "health_barriers_health_servic_1", "health_barriers_cannot_afford_1", 
#               "health_barriers_cannot_afford_2", "health_barriers_cannot_afford_3", "health_barriers_no_time")  # replace

dep_vars <- c("health_barriers_no_issue")

var_labels <- c(
  health_barriers_no_issue = "No Challenge in Accessing Healthcare"
  # ,
  # health_barriers_no_functional_1 = "No functional health facility nearby",
  # health_barriers_treatment_med_1 = "Specific treatment unavailable",
  # health_barriers_long_waiting = "Long waiting time for the service / overcrowded",
  # health_barriers_health_servic_1 = "Health facility is too far", 
  # health_barriers_cannot_afford_2 = "Cannot afford cost of treatment", 
  # health_barriers_cannot_afford_1 = "Cannot afford cost of consultation", 
  # health_barriers_cannot_afford_3 = "Cannot afford cost of transportation", 
  # health_barriers_no_time = "Could not take time off work"
)





library(viridis)

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
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "red") +
    scale_fill_viridis_d(option = "D", begin = 0, end = 1) +  # Option "D" is default
    labs(
      x = "",
      y = "Share of HH"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    ) +
    ylim(0, 1)
  
  # Save
  filename <- paste0("Graphs/2023/Children/plot_refugee_children_", dv, ".png")
  ggsave(filename, plot = p, width = 7, height = 5, dpi = 300, bg = "white")
}


##### TOP-5 Health care barriers #####

combined_data_children2 <- combined_data_children %>% 
    mutate(barriers_type = sub("health_barriers_", "", barriers_type),
           barriers_existence = as.numeric(barriers_existence))



  sum_barriers <- combined_data_children %>% 
  group_by(refugee) %>%
  summarise(across(2:(ncol(.) - 3), ~ mean(.x, na.rm = TRUE)))%>%
  arrange(refugee) %>%
  pivot_longer(
    cols = starts_with("health_barriers_"),
    names_to = "barriers_type",
    values_to = "barriers_existence"
  ) %>%
  mutate(barriers_type = sub("health_barriers_", "", barriers_type),
         barriers_existence = as.numeric(barriers_existence)) %>% 
  mutate(barriers_type = case_when(
    barriers_type == "no_issue" ~ "No challenges accessing health care",
    barriers_type == "no_functional_1" ~ "No functional health facility nearby",
    barriers_type == "treatment_med_1" ~ "Specific medicine, treatment or service needed unavailable",
    barriers_type == "long_waiting" ~ "Long waiting time for the service/overcrowded",
    barriers_type == "cannot_afford_1" ~ "Could not afford cost of consultation",
    barriers_type == "cannot_afford_2" ~ "Could not afford cost of treatment",
    barriers_type == "cannot_afford_3" ~ "Could not afford cost of transportation to health facility",
    barriers_type == "health_servic_1" ~ "Health facility is too far away",
    barriers_type == "pwd_access" ~ "Disability prevents access to health facility",
    barriers_type == "lack_transp" ~ "No means of transport",
    barriers_type == "safety_concer_1" ~ "Not safe/ insecurity at health facility",
    barriers_type == "safety_concer_2" ~ "Not safe/insecurity while traveling to health facility",
    barriers_type == "incorrect_med_1" ~ "Did not receive correct medications",
    barriers_type == "lack_trained__1" ~ "Not trained staff at health facility",
    barriers_type == "lack_enough_s_1" ~ "Not enough staff at health facility",
    barriers_type == "wanted_wait" ~ "Wanted to wait and see if problem got better on its own",
    barriers_type == "distrust" ~ "Fear or distrust of health workers, examination or treatment",
    barriers_type == "no_time" ~ "Could not take time off work/from caring for children",
    barriers_type == "language_barr_1" ~ "Language barriers or issues at health facility",
    barriers_type == "no_female_sta_1" ~ "Lack of female staff at health facility",
    barriers_type == "facility_full" ~ "Turned away because facility was full",
    barriers_type == "facility_closed" ~ "Turned away because facility was closed",
    barriers_type == "facility_dest_1" ~ "Health facility is destroyed",
    TRUE ~ barriers_type)) %>% 
    arrange(refugee, desc(barriers_existence)) %>% 
    filter(barriers_type != "No challenges accessing health care") 
  
  
  barriers_refugee <- sum_barriers%>% 
  filter(refugee==1) %>%
    arrange(desc(barriers_existence), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    mutate(barriers_grouped = if_else(rank <= 5, barriers_type, "Other")) %>%
    group_by(refugee, barriers_grouped) %>%
    summarise(barriers_existence = sum(barriers_existence), .groups = "drop") %>%
    arrange(refugee, desc(barriers_existence))
  
  name_barriers_refugee <- barriers_refugee %>%
    filter(barriers_grouped != "Other") %>% 
    pull(barriers_grouped)
  

  barriers_host <- sum_barriers%>% 
    filter(refugee==0) %>%
    arrange(desc(barriers_existence), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    mutate(barriers_grouped = if_else(rank <= 5, barriers_type, "Other")) %>%
    group_by(refugee, barriers_grouped) %>%
    summarise(barriers_existence = sum(barriers_existence), .groups = "drop") %>%
    arrange(refugee, desc(barriers_existence))
  
  name_barriers_host <- barriers_host %>%
    filter(barriers_grouped != "Other") %>%
    pull(barriers_grouped)
  
  all_name1 <- unique(c(name_barriers_refugee, name_barriers_host))
  all_name2 <- unique(c(name_barriers_refugee, name_barriers_host, "Other"))
  
  
# top 5 refugee 
barriers_refugee <- sum_barriers %>% 
  filter(barriers_type %in% name_barriers_refugee)

# top 5 host
barriers_host <- sum_barriers %>% 
  filter(barriers_type %in% name_barriers_host)

# combine 
# Combine and process
barriers <- bind_rows(barriers_refugee, barriers_host) %>%
  mutate(refugee = ifelse(refugee == 1, "Refugee", "Host")) %>%
  arrange(refugee, desc(barriers_existence), .by_group = TRUE) %>%
  distinct()

# Corrected factor ordering for proper descending plot order
barriers_order <- barriers %>%
  filter(refugee == "Refugee") %>%
  arrange(desc(barriers_existence)) %>%
  pull(barriers_type) %>%
  unique() %>%
  rev()  

# Convert to factor with specified order (for plotting)
barriers <- barriers %>%
  mutate(barriers_type = factor(barriers_type, levels = barriers_order))

# Plot
graph <- ggplot(barriers, aes(x = barriers_type, 
                     y = barriers_existence, 
                     fill = refugee)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(
    x = "",
    y = "Share of Households",
    fill = ""
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "C", end = 0.9) +   # Use viridis color scale
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  coord_flip()


filename <- paste0("Graphs/health_barriers_.png")
ggsave(filename, plot = graph, width = 8, height = 5, dpi = 300, bg = "white")



## Household Priorities ---------------------------------------------------
### Refugee ###

### Select hh with kids

children <- R_ind_2023 %>% 
  group_by(pseudo_parent_id) %>%
  mutate(
    has_children = if_else(any(ind_age < 4), 1, 0)
  ) %>% 
  ungroup() %>% 
  select(pseudo_parent_id, has_children) 


# merge ind and hh data 
R_health_child <- R_hh_2023 %>% 
  left_join(children, by = "pseudo_parent_id") %>% 
  #filter(!is.na(health_barriers_not_acc_lack__1)) %>% 
  mutate(
    status = "refugee"
  ) %>% 
  distinct()

length(unique(R_health_child$pseudo_parent_id))==nrow(R_health_child) #true


### Host ###

### Select hh with kids
children <- H_ind_2023 %>% 
  group_by(pseudo_parent_id) %>%
  mutate(
    has_children = if_else(any(ind_age < 4), 1, 0)
  ) %>% 
  ungroup() %>% 
  select(pseudo_parent_id, has_children) 

# merge ind and hh data
H_health_child <- H_hh_2023 %>% 
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

##################

hh_priority_ref23 <- combined_data_children %>%
  filter(refugee == 1) %>%
  select(pseudo_parent_id, starts_with("hh_priority"), starts_with("rank_priority"), starts_with("priority"))%>%
  group_by(pseudo_parent_id) %>%
  slice(1)%>%
  ungroup() %>%
  mutate(n_hh = n()) %>%
  group_by(hh_priority_needs_top1, n_hh) %>%
  summarise(n_priority = n()) %>%
  mutate(share_ref = 100 * n_priority/n_hh,
         hh_priority_needs_top1 = case_when(
           hh_priority_needs_top1 == 1 ~ "Access to food",
           hh_priority_needs_top1 == 2 ~ "Shelter materials/upgrade",
           hh_priority_needs_top1 == 3 ~ "Access to clean drinking water",
           hh_priority_needs_top1 == 4 ~ "Access to safe and functional latrines",
           hh_priority_needs_top1 == 5 ~ "Access to hygiene items",
           hh_priority_needs_top1 == 6 ~ "Electricity/solar lamps/batteries",
           hh_priority_needs_top1 == 7 ~ "Household/cooking items",
           hh_priority_needs_top1 == 8 ~ "Clothing",
           hh_priority_needs_top1 == 9 ~ "Access to health services and/or medicine",
           hh_priority_needs_top1 == 10 ~ "Access to essential nutrition services (for children, PLW)",
           hh_priority_needs_top1 == 11 ~ "Access to protection services",
           hh_priority_needs_top1 == 12 ~ "Access to education for children",
           hh_priority_needs_top1 == 13 ~ "Registration services/documentation (ID cards) ",
           hh_priority_needs_top1 == 14 ~ "Cooking Fuel",
           hh_priority_needs_top1 == 15 ~ "Access to income-generating activities/employment",
           hh_priority_needs_top1 == 16 ~ "Access to information",
           hh_priority_needs_top1 == 17 ~ "Support with providing feedback on relief items or services received",
           hh_priority_needs_top1 == 18 ~ "Access to conditional cash (cash for work)",
           hh_priority_needs_top1 == 19 ~ "Increased/change in community representation ",
           hh_priority_needs_top1 == 20 ~ "Other (specify)",
           hh_priority_needs_top1 == 21 ~ "Don't know",
           hh_priority_needs_top1 == 22 ~ "Prefer not to answer",
           hh_priority_needs_top1 == 23 ~ "None (don't have priority needs)"
           
         ))%>%
  select(hh_priority_needs_top1, share_ref)


hh_priority_host23 <- combined_data_children %>%
  filter(refugee == 0) %>%
  select(pseudo_parent_id, starts_with("hh_priority"), starts_with("rank_priority"), starts_with("priority"))%>%
  group_by(pseudo_parent_id) %>%
  slice(1)%>%
  ungroup() %>%
  mutate(n_hh = n()) %>%
  group_by(hh_priority_needs_top1, n_hh) %>%
  summarise(n_priority = n()) %>%
  mutate(share_host = 100 * n_priority/n_hh,
         hh_priority_needs_top1 = case_when(
           hh_priority_needs_top1 == 1 ~ "Access to food",
           hh_priority_needs_top1 == 2 ~ "Shelter materials/upgrade",
           hh_priority_needs_top1 == 3 ~ "Access to clean drinking water",
           hh_priority_needs_top1 == 4 ~ "Access to safe and functional latrines",
           hh_priority_needs_top1 == 5 ~ "Access to hygiene items",
           hh_priority_needs_top1 == 6 ~ "Electricity/solar lamps/batteries",
           hh_priority_needs_top1 == 7 ~ "Household/cooking items",
           hh_priority_needs_top1 == 8 ~ "Clothing",
           hh_priority_needs_top1 == 9 ~ "Access to health services and/or medicine",
           hh_priority_needs_top1 == 10 ~ "Access to essential nutrition services (for children, PLW)",
           hh_priority_needs_top1 == 11 ~ "Access to protection services",
           hh_priority_needs_top1 == 12 ~ "Access to education for children",
           hh_priority_needs_top1 == 13 ~ "Registration services/documentation (ID cards) ",
           hh_priority_needs_top1 == 14 ~ "Cooking Fuel",
           hh_priority_needs_top1 == 15 ~ "Access to income-generating activities/employment",
           hh_priority_needs_top1 == 16 ~ "Access to information",
           hh_priority_needs_top1 == 17 ~ "Support with providing feedback on relief items or services received",
           hh_priority_needs_top1 == 18 ~ "Access to conditional cash (cash for work)",
           hh_priority_needs_top1 == 19 ~ "Increased/change in community representation ",
           hh_priority_needs_top1 == 20 ~ "Other (specify)",
           hh_priority_needs_top1 == 21 ~ "Don't know",
           hh_priority_needs_top1 == 22 ~ "Prefer not to answer",
           hh_priority_needs_top1 == 23 ~ "None (don't have priority needs)"
           
         ))%>%
  select(hh_priority_needs_top1, share_host)


# Pivot and label
top1_data <- merge(hh_priority_ref23, hh_priority_host23) %>%
  pivot_longer(cols = starts_with("share_"), 
               names_to = "status", 
               values_to = "value") %>%
  mutate(status = case_when(
    status == "share_host" ~ "Host",
    status == "share_ref" ~ "Refugee"
  ))


# Top 5 priorities for each group
top_ref <- top1_data %>%
  filter(status == "Refugee") %>%
  arrange(desc(value)) %>%
  slice_head(n = 5) %>%
  pull(hh_priority_needs_top1)

top_host <- top1_data %>%
  filter(status == "Host") %>%
  arrange(desc(value)) %>%
  slice_head(n = 5) %>%
  pull(hh_priority_needs_top1)

# Combine unique top priorities
top_priorities <- unique(c(top_ref, top_host))

# Filter to only top priorities
top1_filtered <- top1_data %>%
  filter(hh_priority_needs_top1 %in% top_priorities)

# Order by refugee share
priority_order <- top1_filtered %>%
  filter(status == "Refugee") %>%
  arrange(desc(value)) %>%
  pull(hh_priority_needs_top1) %>%
  unique() %>%
  rev()  # Reverse for coord_flip()

top1_filtered <- top1_filtered %>%
  mutate(hh_priority_needs_top1 = factor(hh_priority_needs_top1, levels = priority_order))

# Plot
fig2 <- ggplot(top1_filtered, aes(fill = status, y = value, x = hh_priority_needs_top1)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(x = "", y = "Share of Households", fill= "") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) 
ggsave("Graphs/top1_rank.png",fig2,  width = 8, height = 5, dpi = 300)


