rm(list=ls())
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, data.table, tidyverse, stargazer, ggplot2, 
  lmtest, fixest, readxl, sf, skimr, openxlsx, tiff, raster, ggspatial,
  terra, patchwork, ggpattern, stringr
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(12)

## change the working directory
setwd("C:/Users/iddo2/Downloads/unhcr oecd/bangladesh")
# Data preparation ----- 

## Import data -----  

## 2020 data
bang_ref_ind20 <- read_dta("2020/UNHCR BGD 2020 JMSNA Refugees Individuals.dta")
bang_ref_hh20 <- read_dta("2020/UNHCR BGD 2020 JMSNA Refugees Households.dta")

bang_host_ind20 <- read_dta("2020/UNHCR BGD 2020 JMSN Host Individuals.dta")
bang_host_hh20 <- read_dta("2020/UNHCR BGD 2020 JMSN Host Households.dta")

## 2021 data
bang_ref_ind21 <- read_dta("2021/UNHCR_BGD_2021_jmsna_ref_datai_anon.dta")                        
bang_ref_hh21 <- read_dta("2021/UNHCR_BGD_2021_jmsna_ref_datam_anon.dta")                        

bang_host_ind21 <- read_dta("2021/UNHCR_BGD_2021_msnahost_datai_anon.dta")
bang_host_hh21 <- read_dta("2021/UNHCR_BGD_2021_msnahost_datam_anon.dta")

## 2023 data
bang_ref_hh23 <- read_dta("2023/UNHCR_BGD_2023_msnaref_datam_anon.dta")
bang_ref_ind23 <- read_dta("2023/UNHCR_BGD_2023_msnaref_datai_anon.dta")

bang_host_hh23 <- read_dta("2023/UNHCR_BGD_2023_msnahost_datam_anon.dta")
bang_host_ind23 <- read_dta("2023/UNHCR_BGD_2023_msnahost_datai_anon.dta")

## Add household income quantile indicator for 2023 households ----

add_income_quantile_column <- function(data, variable_name) {
  # Extract numeric values from the labelled variable
  values <- as.numeric(data[[variable_name]])
  
  # Calculate decile cutpoints
  quantile_breaks <- quantile(values, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  
  # Handle ties by using type=1 in the cut function
  # Add a small amount to the max value to ensure the highest value is included
  max_val <- max(values, na.rm = TRUE)
  quantile_breaks[length(quantile_breaks)] <- max_val + 0.000001
  
  # Create a new column with the decile (1-10)
  data[[paste0(variable_name, "_quantile")]] <- cut(
    values,
    breaks = quantile_breaks,
    labels = 1:4,
    include.lowest = TRUE
  )
  
  return(data)
}

# Add income decile columns to both datasets
bang_ref_hh23 <- add_income_quantile_column(bang_ref_hh23, "income_v1_total_30_days")
bang_host_hh23 <- add_income_quantile_column(bang_host_hh23, "income_v1_total_30_days")

## Merge household datasets with individual datasets and only keep children individuals and households with children -----  

## 2020 data
bang_ref_hh_w_child20 <- bang_ref_ind20 %>%
  filter(over_18_hh == 0) %>%
  rename(id = household_id)%>%
  left_join(bang_ref_hh20, by = "id")%>%
  rename(household_id = id)

bang_host_hh_w_child20 <- bang_host_ind20 %>%
  filter(over_18_HH == 0) %>%
  left_join(bang_host_hh20, by = "id")%>%
  rename(household_id = id)

## 2021 data
bang_ref_hh_w_child21 <- bang_ref_ind21 %>%
  filter(individual_age < 4) %>%
  left_join(bang_ref_hh21, by = "pseudo_id")%>%
  rename(ind_id = pseudo_id,
         household_id = pseudo_parent_id)


bang_host_hh_w_child21 <- bang_host_ind21 %>%
  filter(individual_age < 4) %>%
  left_join(bang_host_hh21, by = "pseudo_parent_id")%>%
  rename(household_id = pseudo_parent_id,
         ind_id = pseudo_id)

## 2023 data
bang_ref_hh_w_child23 <- bang_ref_ind23 %>%
  filter(ind_age < 4) %>%
  left_join(bang_ref_hh23, by = "pseudo_parent_id")%>%
  rename(household_id = pseudo_parent_id,
         ind_id = pseudo_id)

bang_host_hh_w_child23 <- bang_host_ind23 %>%
  filter(ind_age < 4) %>%
  left_join(bang_host_hh23, by = "pseudo_parent_id")%>%
  rename(household_id = pseudo_parent_id,
         ind_id = pseudo_id)

## remove unnecessary loaded datasets
rm(bang_ref_ind23, bang_ref_ind21, bang_ref_ind20,
   bang_ref_hh23, bang_ref_hh21, bang_ref_hh20,
   bang_host_ind23, bang_host_ind21, bang_host_ind20,
   bang_host_hh23, bang_host_hh21, bang_host_hh20)


# Analysis ----------------------------------------------------------------



## External validation: Distance and Density: Geography as Destiny ------------------------------
healthsites <- read_sf("Healthsites_shapefiles/bangladesh.shp")%>%
  filter(!(amenity == "*")) %>%
  filter(!(is.na(amenity)))


camp_A3 <- read_sf("20240519_A3_Camp_SubBlock_Outlines/20240519_A3_Camp_SubBlock_Outlines.shp")
camp_A2 <- read_sf("20240519_a2_camp_block_outlines/20240519_A2_Camp_Block_Outlines.shp")
camp_A1 <- read_sf("20230412_a1_camp_outlines/20230412_A1_Camp_Outlines.shp")
view(camp_A1)

camp_A1_Teknaf <- camp_A1 %>%
  filter(Upazila == "Teknaf")

camp_A1_Ukhia<- camp_A1 %>%
  filter(Upazila == "Ukhia")

admin3_Teknaf <- read_sf("bgd_adm_bbs_20201113_shp/bgd_admbnda_adm3_bbs_20201113.shp")%>% 
  filter(
    ADM2_EN == "Cox's Bazar", # actually unnecessary as the next line overwrites it
    ADM3_EN %in% c("Teknaf")  ) %>% 
  select(ADM3_EN)

admin3_Ukhia <- read_sf("bgd_adm_bbs_20201113_shp/bgd_admbnda_adm3_bbs_20201113.shp")%>% 
  filter(
    ADM2_EN == "Cox's Bazar", # actually unnecessary as the next line overwrites it
    ADM3_EN %in% c( "Ukhia")  ) %>% 
  select(ADM3_EN)

bbox_Teknaf <- st_bbox(admin3_Teknaf)

bbox_Ukhia <- st_bbox(admin3_Ukhia)

healthsites_Teknaf <- ggplot() + 
  geom_sf(data = admin3_Teknaf, fill= "antiquewhite" )+ 
  geom_sf(data = camp_A1_Teknaf , fill= "white") +
  geom_sf(data = healthsites, aes(color = amenity) )+ 
  coord_sf(
    xlim = c(bbox_Teknaf["xmin"], bbox_Teknaf["xmax"]),
    ylim = c(bbox_Teknaf["ymin"], bbox_Teknaf["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Geographic distribution of the healthsites in Teknaf") + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

ggsave("healthsites_Teknaf.pdf")


healthsites_Ukhia <- ggplot() + 
  geom_sf(data = admin3_Ukhia, fill= "antiquewhite")+ 
  geom_sf(data = camp_A1_Ukhia, fill= "white") +
  geom_sf(data = healthsites, aes(color = amenity) )+ 
  coord_sf(
    xlim = c(bbox_Ukhia["xmin"], bbox_Ukhia["xmax"]),
    ylim = c(bbox_Ukhia["ymin"], bbox_Ukhia["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Geographic distribution of the healthsites in Ukhia") + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

ggsave("healthsites_Ukhia.png", width = 6, height = 6, dpi = 300)


pop_density <- raster("bgd_general_2020_geotiff/bgd_general_2020.tif")
plot(pop_density)

pop_df <- as.data.frame(pop_density, xy = TRUE)
colnames(pop_df)[3] <- "density"

area_Teknaf <- vect(admin3_Teknaf)
pop_density_terra <- rast(pop_density)
raster_Teknaf <- crop(pop_density_terra, area_Teknaf)

area_Ukhia <- vect(admin3_Ukhia)
pop_density_terra <- rast(pop_density)
raster_Ukhia <- crop(pop_density_terra, area_Ukhia)


raster_df_Teknaf <- as.data.frame(raster_Teknaf, xy = TRUE)
colnames(raster_df_Teknaf)[3] <- "density"
raster_df_Ukhia <- as.data.frame(raster_Ukhia, xy = TRUE)
colnames(raster_df_Ukhia)[3] <- "density"

combined_range <- range(c(raster_df_Teknaf$density, raster_df_Ukhia$density), na.rm = TRUE)


Pop_density_Teknaf <- ggplot() +
  geom_sf(data = admin3_Teknaf, fill = "antiquewhite", color = "black", alpha = 0.4) +
  geom_sf(data = camp_A1_Teknaf, fill = "white", color = "gray40") +
  geom_raster(data = raster_df_Teknaf, aes(x = x, y = y, fill = density)) +
  scale_fill_viridis(name = "Population Density", 
                     na.value = "transparent", 
                     limits = combined_range)+
  coord_sf(
    xlim = c(bbox_Teknaf["xmin"], bbox_Teknaf["xmax"]),
    ylim = c(bbox_Teknaf["ymin"], bbox_Teknaf["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Population Density in Teknaf") + 
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

ggsave("Pop_density_Teknaf.png", width = 6, height = 6, dpi = 300)


Pop_density_Ukhia <- ggplot() +
  geom_sf(data = admin3_Ukhia, fill = "antiquewhite", color = "black", alpha = 0.4) +
  geom_sf(data = camp_A1_Ukhia, fill = "white", color = "gray40") +
  geom_raster(data = raster_df_Ukhia, aes(x = x, y = y, fill = density)) +
  scale_fill_viridis(name = "Population Density", 
                     na.value = "transparent", 
                     limits = combined_range) +
  coord_sf(
    xlim = c(bbox_Ukhia["xmin"], bbox_Ukhia["xmax"]),
    ylim = c(bbox_Ukhia["ymin"], bbox_Ukhia["ymax"]),
    expand = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Population Density in Ukhias") + 
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

ggsave("Pop_density_Ukhia.png", width = 6, height = 6, dpi = 300)




## Treatment type needed ---------------------------------------------------

vars_ind_demo <- c("household_id", "ind_id")

bang_ref_child_treatment2023 <- bang_ref_hh_w_child23%>%
  select(starts_with("health"), all_of(vars_ind_demo), income_v1_total_30_days_quantile) %>%
  select(all_of(vars_ind_demo), "health_needed_healthcare", "health_received_healthcare", starts_with("health_tmnt_needed"), income_v1_total_30_days_quantile)%>%
  filter(health_needed_healthcare == "yes") %>%
  pivot_longer(
    cols = starts_with("health_tmnt_needed_"),
    names_to = "treatment_type",
    values_to = "treatment_needed"
  )%>%
  mutate(treatment_type = sub("health_tmnt_needed_", "", treatment_type),
         treatment_needed = as.numeric(treatment_needed))

child_health_lollipop <- bang_ref_child_treatment2023 %>%
  group_by(treatment_type) %>%
  summarise(num_needed = sum(treatment_needed ==1))%>%
  ungroup() %>%
  mutate(share_children = num_needed/5477)%>%
  filter(!(treatment_type %in% c("oth","dont_know","decline_an_1" )) ) %>%
  mutate(treatment_type = case_when(
    treatment_type == "preventati_1" ~ "Preventative consultation / check-up",
    treatment_type == "consultati_1" ~ "Consultation or drugs for acute illness",
    treatment_type == "consultati_2" ~ "Consultation or drugs for chronic illness",
    treatment_type == "trauma_care" ~ "Trauma care",
    treatment_type == "elective" ~ "Elective, non-life saving surgery",
    treatment_type == "emergency" ~ "Emergency, life saving surgery",
    treatment_type == "antenatal" ~ "Ante-natal or post-natal services",
    treatment_type == "safe_delivery" ~ "Safe delivery services",
    treatment_type == "laboratory" ~ "Laboratory services",
    treatment_type == "gbv_services" ~ "GBV services",
    treatment_type == "mhpss_serv_1" ~ "MHPSS services",
    treatment_type == "vaccinatio_1" ~ "Vaccination services",
    treatment_type == "dental_ser_1" ~ "Dental services",
    treatment_type == "family_pla_1" ~ "Family planning",
    treatment_type == "oth" ~ "Other (specify)",
    treatment_type == "dont_know" ~ "Don't know",
    treatment_type == "decline_an_1" ~ "Prefer not to answer",
    TRUE ~ treatment_type)) %>%
  arrange(desc(num_needed)) %>%
  ggplot(aes(x = reorder(treatment_type, num_needed), y = share_children)) +
  geom_segment(aes(xend = treatment_type, y = 0, yend = share_children), color = "skyblue") +
  geom_point(size = 3, color = "#238A8Dff") +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal() 

child_health_lollipop

ggsave("treatment_type.png", width = 8, height = 4, dpi = 300)

## Preventive treatment and vaccinations by income --------------------------
vars_ind_demo <- c("household_id", "ind_id")

bang_host_child_treatment2023 <- bang_host_hh_w_child23 %>% 
  select(starts_with("health"), all_of(vars_ind_demo), income_v1_total_30_days_quantile) %>%
  select(all_of(vars_ind_demo), "health_needed_healthcare", "health_received_healthcare", starts_with("health_tmnt_needed"), income_v1_total_30_days_quantile)%>%
  filter(health_needed_healthcare == "yes") %>% 
  # rmeove specific reason (character)
  select(-health_tmnt_needed_spec) %>% 
  pivot_longer(
    cols = starts_with("health_tmnt_needed_"),
    names_to = "treatment_type",
    values_to = "treatment_needed"
  )%>%
  mutate(treatment_type = sub("health_tmnt_needed_", "", treatment_type),
         treatment_needed = as.numeric(treatment_needed)) 

bang_ref_child_treatment2023$group <- "Refugee"
bang_host_child_treatment2023$group <- "Host"

treatment_by_group_income <- bind_rows(
  bang_host_child_treatment2023, 
  bang_ref_child_treatment2023
) %>%
  mutate(
    income_v1_total_30_days_quantile = as.numeric(income_v1_total_30_days_quantile),
    treatment_type = case_when(
      treatment_type == "preventati_1" ~ "Preventive care",
      treatment_type == "vaccinatio_1" ~ "Vaccinations",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(group, income_v1_total_30_days_quantile, treatment_type) %>%
  summarize(
    total_children = n(),                           # All children in group/income quantile
    needed_treatment = sum(treatment_needed == 1, na.rm = TRUE),
    proportion = needed_treatment / total_children  # Share out of all children
  ) %>%
  ungroup() %>% 
  filter(
    !is.na(treatment_type),
    !is.na(income_v1_total_30_days_quantile)
  ) %>% 
  as.data.frame() 

treatment_income <- ggplot(treatment_by_group_income, 
                           aes(x = treatment_type, 
                               y = proportion, 
                               fill = factor(income_v1_total_30_days_quantile)
                           )) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ group) +
  scale_fill_viridis_d(
    name = "Income Quantile among the group (1 = poorest, 4 = richest)",
    guide = guide_legend(reverse = FALSE),
    option = "D",
    direction = -1
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Income Quantile (within each group)",
    fill = "Income Quantile"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    legend.position = "bottom",
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_blank(), #element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 15),  # This controls facet label size
    strip.background = element_rect(fill = "gray90"), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

treatment_income

ggsave("treatment_income.png", width = 8, height = 4, dpi = 300)


## Clinic types -----
bang_ref_hh_w_child20_clinic <- bang_ref_hh_w_child20 %>% 
  select(
    ind_ID, household_id, 
    #ind_need_treatment, treatment_sought,
    starts_with("treatment_location")
  ) %>% 
  rename(
    ind_id = ind_ID,
    ngo = treatment_location1,
    government = treatment_location2,
    private = treatment_location3,
    pharmacy = treatment_location4,
    traditional = treatment_location5,
    remote = treatment_location6,
    other = treatment_location77
  ) %>% 
  mutate(
    group = "Refugees",
    year = "2020"
  )

bang_host_hh_w_child20_clinic <- bang_host_hh_w_child20 %>% 
  select(
    ind_ID, household_id, 
    #ind_need_treatment, treatment_sought,
    starts_with("treatment_location")
  ) %>% 
  rename(
    ind_id = ind_ID,
    ngo = treatment_location1,
    government = treatment_location2,
    private = treatment_location3,
    pharmacy = treatment_location4,
    traditional = treatment_location5,
    remote = treatment_location6,
    other = treatment_location77
  ) %>% 
  mutate(
    group = "Hosts",
    year = "2020"
  )


bang_ref_hh_w_child21_clinic <- bang_ref_hh_w_child21 %>% 
  select(
    ind_id, household_id, 
    #indv_needed_treatment, indv_sought_treatment,
    starts_with("clinics")
  ) %>% 
  rename(
    ngo = clinics_ngo_clinic,
    government = clinics_government_clinic,
    private = clinics_private_clinic,
    pharmacy = clinics_pharmacy_drug_market,
    traditional = clinics_traditional,
    remote = clinics_remotely_doctor,
    other = clinics_oth
  ) %>% 
  mutate(
    group = "Refugees",
    year = "2021"
  )


bang_host_hh_w_child21_clinic <- bang_host_hh_w_child21 %>% 
  select(
    ind_id, household_id, 
    #indv_needed_treatment, indv_sought_treatment,
    starts_with("clinics")
  ) %>% 
  rename(
    ngo = clinics_ngo_clinic,
    government = clinics_government_clinic,
    private = clinics_private_clinic,
    pharmacy = clinics_pharmacy_drug_market,
    traditional = clinics_traditional,
    remote = clinics_remotely_doctor,
    other = clinics_oth
  ) %>% 
  mutate(
    group = "Hosts",
    year = "2021"
  )


kids_20_21 <- bind_rows(
  bang_ref_hh_w_child20_clinic, 
  bang_ref_hh_w_child21_clinic, 
  bang_host_hh_w_child20_clinic, 
  bang_host_hh_w_child21_clinic
) %>% 
  select(
    -c(
      "clinics_nowhere", "clinics_dont_know",
      "treatment_location", "treatment_location99"
    )
  ) 


clinic_cols <- c("ngo", "government", "private", "pharmacy")

clinic_shares <- kids_20_21 %>%
  pivot_longer(
    cols = c(government, ngo, pharmacy, private, traditional, remote, other),
    names_to = "clinic_type",
    values_to = "visited"
  ) %>%
  filter(!is.na(visited)) %>%
  group_by(group, year) %>%
  mutate(total_visits = sum(visited, na.rm = TRUE)) %>%
  group_by(group, year, clinic_type) %>%
  summarise(
    clinic_visits = sum(visited, na.rm = TRUE),
    total_visits = unique(total_visits),
    share = clinic_visits / total_visits,
    .groups = "drop"
  ) %>% 
  filter(!clinic_type %in% c("traditional", "other", "remote")) %>% 
  mutate(
    group_year = paste(group, year, sep = "_"),
    clinic_type = case_when(
      clinic_type == "ngo" ~ "NGO", 
      TRUE ~ stringr::str_to_title(clinic_type)
    ),
    # Format share as percentage
    share_pct = share * 100
  )

group_colors <- c(
  "Hosts" = "#0D0887FF",     # Viridis purple
  "Refugees" = "#fccc24"   # Viridis teal
)

# 2. Create the plot
clinic_types <- ggplot(clinic_shares, 
                       aes(x = clinic_type, y = share_pct, 
                           fill = group,          # Color by group
                           pattern = year)) +    # Pattern by year
  geom_col_pattern(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",             # Add thin border for clarity
    pattern_fill = "white",      # Stripe color
    pattern_density = 0.3,       # More visible stripes
    pattern_spacing = 0.03,
    pattern_alpha = 0.5          # Semi-transparent stripes
  ) +
  # Color scale (groups)
  scale_fill_manual(values = group_colors, name = "Group") +
  # Pattern scale (years)
  scale_pattern_manual(
    values = c("2020" = "stripe", "2021" = "none"),
    name = "Year", 
    labels = c("2020", "2021")
  ) +
  # Y-axis as percentage
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",   # Place legends side by side
    legend.margin = margin(t = 10),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10, color = "black")
  )

clinic_types

ggsave("clinic_types.png", width = 8, height = 4, dpi = 300)
