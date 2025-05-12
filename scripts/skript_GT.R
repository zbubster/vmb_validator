# skript grasslandtype

library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)

vmb <- read.csv("data/VMB_intersect.csv", header = T)
str(vmb)
vmb$BIOTOP_CODES <- as.factor(gsub(" \\(\\d+\\)", "", vmb$BIOTOP_SEZ))
vmb$HABIT_CODES <- as.factor(gsub(" \\(\\d+\\)", "", vmb$HABIT_SEZ))
str(vmb)
head(vmb)

vmb$GT_22_pc <- (vmb$GT_22/vmb$SHAPE_Area)*100
vmb$GT_23_pc <- (vmb$GT_23/vmb$SHAPE_Area)*100
vmb$GT_25_pc <- (vmb$GT_25/vmb$SHAPE_Area)*100
vmb$GT_27_pc <- (vmb$GT_27/vmb$SHAPE_Area)*100

vmb2 <- vmb %>%
  select(SEGMENT_ID, FSB, BIOTOP_CODES, HABIT_CODES, GT_22:GT_27_pc, SHAPE_Area, DATUM) %>%
  filter(FSB != "moz." & FSB != "-" & FSB != "S")
  
vmb2$FSB <- as.factor(vmb2$FSB)
head(vmb2, 30)
str(vmb2)

gt_names <- c("GT_22_pc" = "R2-Mesic grassland", 
              "GT_23_pc" = "R3-Wet and temporarily wet grassland", 
              "GT_25_pc" = "R5-Forest clearings",
              "GT_27_pc" = "R7-Sparsely wooded grassland")

vmb2[vmb2$FSB == "A",]
vmb2[vmb2$BIOTOP_CODES == "T4.2",]

################################################################################
################################## COVER #######################################
################################################################################

# Select relevant columns
vmb2_long <- vmb2 %>%
  pivot_longer(cols = starts_with("GT_"), 
               names_to = "GT_type", 
               values_to = "percentage_cover") %>%
  filter(grepl("_pc$", GT_type))  # Keep only percentage cover columns
vmb2_long

# Calculate number of segments per FSB category
fsb_counts <- vmb2_long %>%
  group_by(FSB) %>%
  summarise(n = sum(!is.na(percentage_cover)))

# Compute the number of observations per GT_type and FSB
n_counts <- vmb2_long %>%
  group_by(GT_type, FSB) %>%
  summarise(n = n(), .groups = "drop")

# Compute number of observations per FSB category
n_fsb <- vmb2_long %>%
  group_by(FSB) %>%
  summarise(n = n(), .groups = "drop")

# Compute number of observations per BIOTOP_CODES category
n_biotop <- vmb2_long %>%
  group_by(BIOTOP_CODES) %>%
  summarise(n = n(), .groups = "drop")

############################
# gt:fsb
#########################

# Generate the plots for each GT type
pdf("pic_out/cover/FSB_GT_boxplots.pdf", width = 10, height = 8)

# Loop over each GT type and generate separate plots
for (gt_type in unique(vmb2_long$GT_type)) {
  
  # Subset the data for the current GT type
  vmb2_gt <- vmb2_long %>% filter(GT_type == gt_type)
  
  # Remove rows where `percentage_cover` is NA
  vmb2_gt_filtered <- vmb2_gt %>% filter(!is.na(percentage_cover))
  
  # Check if there is any remaining data, if not, skip to next iteration
  if (nrow(vmb2_gt_filtered) == 0) next
  
  # Calculate counts for each FSB category after filtering
  fsb_counts <- vmb2_gt_filtered %>%
    group_by(FSB) %>%
    summarise(n = n())
  
  gt_names <- c("GT_22_pc" = "R2-Mesic grassland", 
                "GT_23_pc" = "R3-Wet and temporarily wet grassland", 
                "GT_25_pc" = "R5-Forest clearings",
                "GT_27_pc" = "R7-Sparsely wooded grassland")
  
  # Create the boxplot
  p <- ggplot(vmb2_gt_filtered, aes(x = FSB, y = percentage_cover, fill = FSB)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Percentage Cover by FSB Category |", gt_names[[gt_type]]),
         x = "FSB Category",
         y = "Percentage Cover (%)") +
    theme(legend.position = "none") +
    scale_x_discrete(labels = function(x) {
      paste0(x, "\n(n=", fsb_counts$n[match(x, fsb_counts$FSB)], ")")
    })
  
  # Print the plot (saves it to PDF)
  print(p)
}

# Close the PDF file
dev.off()

################################################################################

eunis_biotop <- list("GT_22_pc" = c("T1.1", "T1.2", "T1.3"),
                     "GT_23_pc" = c("T1.4", "T1.5", "T1.7", "T1.9", "T1.10"),
                     "GT_25_pc" = c("M5", "M7", "A4.1", "A4.2", "A4.3", "T1.6", "T1.8", "T4.1", "T4.2"),
                     "GT_27_pc" = character(0))

pdf("pic_out/cover/FSB_BIOTOP_GT_boxplots_cover.pdf", width = 10, height = 6)

for (fsb_category in unique(vmb2_long$FSB)) {
  
  vmb2_fsb <- vmb2_long %>% filter(FSB == fsb_category)
  
  for (gt_variable in unique(vmb2_fsb$GT_type)) {
    
    vmb2_gt <- vmb2_fsb %>% filter(GT_type == gt_variable)
    vmb2_gt_filtered <- vmb2_gt %>% filter(!is.na(percentage_cover))
    if (nrow(vmb2_gt_filtered) == 0) next
    
    # Keep only BIOTOPs with >10 observations
    n_counts <- vmb2_gt_filtered %>%
      group_by(BIOTOP_CODES) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 10)
    
    vmb2_gt_filtered <- vmb2_gt_filtered %>%
      filter(BIOTOP_CODES %in% n_counts$BIOTOP_CODES)
    if (nrow(vmb2_gt_filtered) == 0) next
    
    # === Get expected EUNIS biotop codes for this GT (e.g. GT_22) ===
    eunis_codes <- eunis_biotop[[gt_variable]]
    
    # === Create annotation (Y / blank) ===
    symbol_df <- n_counts %>%
      mutate(symbol = ifelse(BIOTOP_CODES %in% eunis_codes, "Y", ""),
             y = max(vmb2_gt_filtered$percentage_cover, na.rm = TRUE) + 10)
    
    # === Plot ===
    p <- ggplot(vmb2_gt_filtered, aes(x = BIOTOP_CODES, y = percentage_cover, fill = BIOTOP_CODES)) +
      geom_boxplot() +
      geom_text(data = n_counts,
                aes(x = BIOTOP_CODES,
                    y = max(vmb2_gt_filtered$percentage_cover, na.rm = TRUE) + 5,
                    label = paste("n =", n)),
                size = 4, inherit.aes = FALSE) +
      geom_text(data = symbol_df,
                aes(x = BIOTOP_CODES, y = y, label = symbol),
                size = 6, fontface = "bold", color = "red", inherit.aes = FALSE) +
      theme_minimal() +
      labs(title = paste("FSB:", fsb_category, "| GT:", gt_names[[gt_variable]]),
           x = "BIOTOP_CODES",
           y = "Percentage Cover (%)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "none")
    
    print(p)
  }
}

dev.off()

################################################################################
############################ ABSOLUTNI #########################################
################################################################################

# Categorize each value as zero or nonzero for percentage cover
vmb2_long <- vmb2_long %>%
  #mutate(found_not_found = ifelse(is.na(percentage_cover), "Not found", "Found"))
  mutate(found_not_found = ifelse(percentage_cover < 10 | is.na(percentage_cover), "Not found", "Found"))
  
# Calculate the number of zero and nonzero values within each FSB category
fsb_counts <- vmb2_long %>%
  group_by(FSB, found_not_found) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the relative proportions of Zero and Nonzero for each FSB category
fsb_proportions <- fsb_counts %>%
  group_by(FSB) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

################################################################################

png("pic_out/relative_prop/RP_FSB_GT_all.png")
# Plot the relative proportions for each FSB category
ggplot(fsb_proportions, aes(x = FSB, y = prop, fill = found_not_found)) +
  geom_bar(stat = "identity", position = "fill") +  # Position "fill" stacks the bars horizontally and normalizes them
  geom_text(data = n_fsb, aes(x = FSB, y = 1.05, label = paste("n =", n)), size = 4, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Relative Proportions of Found/Not found by FSB",
       x = "FSB Category",
       y = "Proportion") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Not found" = "steelblue", "Found" = "orange"))
dev.off()


##################################################

# Loop through each GT type to generate separate plots
gt_types <- unique(vmb2_long$GT_type)

pdf("pic_out/relative_prop/RP_FSB_GT.pdf")

for (gt in gt_types) {
  # Filter data for the current GT type
  vmb2_gt <- vmb2_long %>% filter(GT_type == gt)
  
  # Calculate the number of zero and nonzero values within each FSB category
  fsb_counts <- vmb2_gt %>%
    group_by(FSB, found_not_found) %>%
    summarise(count = n(), .groups = "drop")
  
  # Calculate the relative proportions of Zero and Nonzero for each FSB category
  fsb_proportions <- fsb_counts %>%
    group_by(FSB) %>%
    mutate(prop = count / sum(count)) %>%
    ungroup()
  
  # Compute the number of observations per FSB
  n_fsb <- vmb2_gt %>%
    group_by(FSB) %>%
    summarise(n = n(), .groups = "drop")
  
  # Plot
  p <- ggplot(fsb_proportions, aes(x = FSB, y = prop, fill = found_not_found)) +
    geom_bar(stat = "identity", position = "fill") +  
    geom_text(data = n_fsb, aes(x = FSB, y = 1.02, label = paste("n =", n)), 
              size = 4, inherit.aes = FALSE) +  
    theme_minimal() +
    labs(title = paste("Relative Proportions of Not found vs Found", gt_names[[gt]]),
         x = "FSB Category",
         y = "Proportion") +
    theme(legend.position = "top") +
    scale_fill_manual(values = c("Not found" = "steelblue", "Found" = "orange"))
  
  print(p)
}

dev.off()

################################################################################

pdf("pic_out/relative_prop/RP_BIOTOP_GT.pdf", width = 10, height = 6)

for (fsb_category in unique(vmb2_long$FSB)) {
  
  vmb2_fsb <- vmb2_long %>% filter(FSB == fsb_category)
  
  for (gt_variable in unique(vmb2_fsb$GT_type)) {
    
    vmb2_gt <- vmb2_fsb %>% filter(GT_type == gt_variable) %>%
      mutate(found_not_found = ifelse(is.na(percentage_cover), "Not found", "Found"))
    
    # Count zero/nonzero values per BIOTOP
    fsb_biotop_counts <- vmb2_gt %>%
      group_by(BIOTOP_CODES, found_not_found) %>%
      summarise(count = n(), .groups = "drop")
    
    # Compute relative proportions
    fsb_biotop_proportions <- fsb_biotop_counts %>%
      group_by(BIOTOP_CODES) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
    
    # Filter biotopes with >10 obs
    n_biotop <- vmb2_gt %>%
      group_by(BIOTOP_CODES) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 10)
    
    fsb_biotop_proportions <- fsb_biotop_proportions %>%
      filter(BIOTOP_CODES %in% n_biotop$BIOTOP_CODES)
    
    if (nrow(fsb_biotop_proportions) == 0) next
    
    # === Add "Y" symbol for matching biotopes in EUNIS list ===
    eunis_codes <- eunis_biotop[[gt_variable]]
    
    symbol_df <- n_biotop %>%
      mutate(symbol = ifelse(BIOTOP_CODES %in% eunis_codes, "Y", ""),
             y = 1.1)  # y position above bar
    
    # === Plot ===
    p <- ggplot(fsb_biotop_proportions, aes(x = BIOTOP_CODES, y = prop, fill = found_not_found)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(data = n_biotop,
                aes(x = BIOTOP_CODES, y = 1.05, label = paste0("n=", n)),
                size = 4, inherit.aes = FALSE) +
      geom_text(data = symbol_df,
                aes(x = BIOTOP_CODES, y = y, label = symbol),
                size = 6, fontface = "bold", color = "red", inherit.aes = FALSE) +
      theme_minimal() +
      labs(title = paste("FSB:", fsb_category, "| GT:", gt_names[[gt_variable]]),
           x = "BIOTOP_CODES",
           y = "Proportion") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "top") +
      scale_fill_manual(values = c("Not found" = "steelblue", "Found" = "orange"))
    
    print(p)
  }
}

dev.off()

################################################################################
############################ Natura & EUNIS ####################################
################################################################################
R2 <- c(6270, 6510, 6520)
R3 <- c(6420, 6460, 6540, 6440, 6450, 6510, 6410)
R5 <- c(6430)
R7 <- c(6530, 9070, 6310)

# Define the corresponding GT variables
GT_list <- list(R2 = "GT_22_pc", R3 = "GT_23_pc", R5 = "GT_25_pc", R7 = "GT_27_pc")
Habitat_list <- list(R2 = R2, R3 = R3, R5 = R5, R7 = R7)

# Define output PDF file
pdf("pic_out/cover_Natura/GT_HABIT_CODES_VALID.pdf", width = 10, height = 6)

# Loop through each GT category
for (GT_name in names(GT_list)) {
  # Get corresponding habitat codes and GT variable
  GT_variable <- GT_list[[GT_name]]
  GT_codes <- Habitat_list[[GT_name]]
  
  # Filter data for the current GT and habitat codes
  vmb2_GT <- vmb2 %>%
    filter(HABIT_CODES %in% GT_codes) %>%
    select(HABIT_CODES, all_of(GT_variable)) %>%
    rename(GT_num_pc = all_of(GT_variable)) %>%
    drop_na(GT_num_pc)  # Remove missing values
  
  # Check if data is completely missing, if so, skip iteration
  if (nrow(vmb2_GT) == 0) next
  
  # Count observations per habitat code
  count_data <- vmb2_GT %>%
    group_by(HABIT_CODES) %>%
    summarise(n = n())
  
  # Generate the boxplot
  p <- ggplot(vmb2_GT, aes(x = as.factor(HABIT_CODES), y = GT_num_pc, fill = as.factor(HABIT_CODES))) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("GT:", GT_name, "| Variable:", gt_names[[GT_variable]]),
         x = "HABIT_CODES",
         y = "Percentage Cover (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")
  
  # Add observation counts as text labels (only if count_data is not empty)
  if (nrow(count_data) > 0) {
    p <- p + geom_text(data = count_data, 
                       aes(x = as.factor(HABIT_CODES), y = max(vmb2_GT$GT_num_pc, na.rm = TRUE) * 1.05, 
                           label = paste("n =", n)), 
                       size = 3, color = "black", vjust = 0)
  }
  
  # Print the plot (save it to PDF)
  print(p)
}

# Close the PDF file
dev.off()


######################################################

GT_list <- list(R2 = "GT_22_pc", R3 = "GT_23_pc", R5 = "GT_25_pc", R7 = "GT_27_pc")
Habitat_list <- list(R2 = c(6270, 6510, 6520),
                     R3 = c(6420, 6460, 6540, 6440, 6450, 6510, 6410),
                     R5 = c(6430),
                     R7 = c(6530, 9070, 6310))

vmb2 <- vmb2 %>%
  filter(HABIT_CODES != "")

# Open a PDF to save plots
pdf("pic_out/cover_Natura/GT_HABIT_CODES_ALL.pdf", width = 12, height = 6)

for (GT_name in names(GT_list)) {
  GT_variable <- GT_list[[GT_name]]
  GT_codes <- Habitat_list[[GT_name]]
  
  # Filter and clean data
  vmb2_filtered <- vmb2 %>%
    select(HABIT_CODES, all_of(GT_variable)) %>%
    rename(GT_num_pc = all_of(GT_variable)) %>%
    drop_na(GT_num_pc)
  
  if (nrow(vmb2_filtered) == 0) next
  
  # Count and keep only HABIT_CODES with at least 10 observations
  habit_counts <- vmb2_filtered %>%
    group_by(HABIT_CODES) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n >= 10)
  
  # If no valid habitats, skip this GT
  if (nrow(habit_counts) == 0) next
  
  # Filter the main data
  vmb2_filtered <- vmb2_filtered %>%
    filter(HABIT_CODES %in% habit_counts$HABIT_CODES) %>%
    mutate(symbol = ifelse(HABIT_CODES %in% GT_codes, "Y", "N"))
  
  # Prepare annotation data
  annotation_df <- vmb2_filtered %>%
    group_by(HABIT_CODES, symbol) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      y1 = max(vmb2_filtered$GT_num_pc, na.rm = TRUE) * 1.05,  # Y/N position
      y2 = max(vmb2_filtered$GT_num_pc, na.rm = TRUE) * 1.03   # n position
    )
  
  # Plot
  p <- ggplot(vmb2_filtered, aes(x = as.factor(HABIT_CODES), y = GT_num_pc, fill = as.factor(HABIT_CODES))) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("GT:", gt_names[[GT_variable]]),
         x = "HABIT_CODES",
         y = "Percentage Cover (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none") +
    geom_text(data = annotation_df, aes(x = HABIT_CODES, y = y1, label = symbol),
              size = 5, color = "black", vjust = 0, inherit.aes = FALSE) +
    geom_text(data = annotation_df, aes(x = HABIT_CODES, y = y2, label = paste0("n=", n)),
              size = 4, color = "black", vjust = 1, inherit.aes = FALSE)
  
  print(p)
}

dev.off()
