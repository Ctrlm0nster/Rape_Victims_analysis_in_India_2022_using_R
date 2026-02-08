library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# ============================================
# 1. READ CSV FILE
# ============================================

# File path: "Data/NCRB_DATA.3.csv
df_raw <- read.csv("Data/NCRB_DATA.3.csv", stringsAsFactors = FALSE)

# View structure
print("Dataset Structure:")
str(df_raw)

# View first few rows
print("First 10 rows:")
print(head(df_raw, 10))

# View column names
print("Column Names:")
print(names(df_raw))

# Check dimensions
cat("\nDataset has", nrow(df_raw), "rows and", ncol(df_raw), "columns\n")

# ============================================
# 2. DATA INSPECTION
# ============================================

# Check for missing values
print("\nMissing Values:")
print(colSums(is.na(df_raw)))

# Summary statistics
print("\nSummary Statistics:")
summary(df_raw)

# ============================================
# 3. DATA CLEANING & TRANSFORMATION
# ============================================

# Assuming the CSV has these columns based on the XML structure
# If column names are different, adjust accordingly

# Clean column names (remove special characters if needed)
names(df_raw) <- gsub("\\.", "_", names(df_raw))
names(df_raw) <- tolower(names(df_raw))

# Create cleaned dataset with meaningful names
df_clean <- df_raw %>%
    # Rename columns for easier analysis
    rename(
        sl_no = sl__no_,
        state_ut = state_ut,
        cases_reported = cases_reported___col__3_,
        child_below_6 = child_victims_of_rape__below_18_yrs____below_6_years___col__4_,
        child_6_to_12 = child_victims_of_rape__below_18_yrs____6_years_and_above___below_12_years___col__5_,
        child_12_to_16 = child_victims_of_rape__below_18_yrs____12_years_and_above___below_16_years___col__6_,
        child_16_to_18 = child_victims_of_rape__below_18_yrs____16_years_and_above___below_18_years___col__7_,
        total_child_victims = child_victims_of_rape__below_18_yrs____total_girl___child_victims___col__8_,
        women_18_to_30 = women_victims_of_rape__above_18_years____18_years_and_above___below_30_years___col__9_,
        women_30_to_45 = women_victims_of_rape__above_18_years____30_years_and_above___below_45_years___col__10_,
        women_45_to_60 = women_victims_of_rape__above_18_years____45_years_and_above___below_60_years___col__11_,
        women_above_60 = women_victims_of_rape__above_18_years____60_years_and_above___col__12_,
        total_women_victims = women_victims_of_rape__above_18_years____total_women___adult_victims___col__13_,
        total_victims = total_victims__col_8_col_13____col__14_
    ) %>%
    # Convert numeric columns
    mutate(across(c(cases_reported:total_victims), as.numeric)) %>%
    # Calculate derived metrics
    mutate(
        # Percentages
        child_percentage = round((total_child_victims / total_victims) * 100, 2),
        women_percentage = round((total_women_victims / total_victims) * 100, 2),

        # Replace NA percentages with 0
        child_percentage = ifelse(is.na(child_percentage) | is.infinite(child_percentage), 0, child_percentage),
        women_percentage = ifelse(is.na(women_percentage) | is.infinite(women_percentage), 0, women_percentage),

        # Categorize states by case volume
        case_category = case_when(
            cases_reported < 100 ~ "Low (<100)",
            cases_reported < 500 ~ "Medium (100-500)",
            cases_reported < 1000 ~ "High (500-1000)",
            TRUE ~ "Very High (>1000)"
        ),

        # Most vulnerable age group for children
        most_vulnerable_child_age = case_when(
            total_child_victims == 0 ~ "No Child Victims",
            child_below_6 == pmax(child_below_6, child_6_to_12, child_12_to_16, child_16_to_18, na.rm = TRUE) ~ "Below 6",
            child_6_to_12 == pmax(child_below_6, child_6_to_12, child_12_to_16, child_16_to_18, na.rm = TRUE) ~ "6-12",
            child_12_to_16 == pmax(child_below_6, child_6_to_12, child_12_to_16, child_16_to_18, na.rm = TRUE) ~ "12-16",
            child_16_to_18 == pmax(child_below_6, child_6_to_12, child_12_to_16, child_16_to_18, na.rm = TRUE) ~ "16-18",
            TRUE ~ "Multiple Equal"
        ),

        # Most vulnerable age group for women
        most_vulnerable_women_age = case_when(
            women_18_to_30 == pmax(women_18_to_30, women_30_to_45, women_45_to_60, women_above_60, na.rm = TRUE) ~ "18-30",
            women_30_to_45 == pmax(women_18_to_30, women_30_to_45, women_45_to_60, women_above_60, na.rm = TRUE) ~ "30-45",
            women_45_to_60 == pmax(women_18_to_30, women_30_to_45, women_45_to_60, women_above_60, na.rm = TRUE) ~ "45-60",
            women_above_60 == pmax(women_18_to_30, women_30_to_45, women_45_to_60, women_above_60, na.rm = TRUE) ~ "60+",
            TRUE ~ "Multiple Equal"
        )
    ) %>%
    # Sort by cases reported
    arrange(desc(cases_reported))

print("\nCleaned Data Sample:")
print(head(df_clean, 5))

# ============================================
# 4. COMPREHENSIVE ANALYSIS
# ============================================

# Overall Statistics
cat("\n========================================\n")
cat("OVERALL STATISTICS - ALL INDIA (2022)\n")
cat("========================================\n")

total_stats <- df_clean |>
    summarise(
        total_states = n(),
        total_cases = sum(cases_reported, na.rm = TRUE),
        total_child_victims = sum(total_child_victims, na.rm = TRUE),
        total_women_victims = sum(total_women_victims, na.rm = TRUE),
        total_all_victims = sum(total_victims, na.rm = TRUE),
        avg_cases_per_state = round(mean(cases_reported, na.rm = TRUE), 2),
        median_cases = median(cases_reported, na.rm = TRUE),
        max_cases = max(cases_reported, na.rm = TRUE),
        min_cases = min(cases_reported, na.rm = TRUE),
        child_victim_percentage = round((sum(total_child_victims, na.rm = TRUE) / sum(total_victims, na.rm = TRUE)) * 100, 2),
        women_victim_percentage = round((sum(total_women_victims, na.rm = TRUE) / sum(total_victims, na.rm = TRUE)) * 100, 2)
    )

print(total_stats)

# Top 10 States by Cases Reported
cat("\n========================================\n")
cat("TOP 10 STATES BY CASES REPORTED\n")
cat("========================================\n")

top_10_states <- df_clean |>
    select(state_ut, cases_reported, total_child_victims, total_women_victims, total_victims) |>
    head(10)

print(top_10_states)

# Bottom 10 States by Cases Reported
cat("\n========================================\n")
cat("BOTTOM 10 STATES BY CASES REPORTED\n")
cat("========================================\n")

bottom_10_states <- df_clean |>
    select(state_ut, cases_reported, total_child_victims, total_women_victims) |>
    tail(10)

print(bottom_10_states)

# States with Highest Child Victim Percentage
cat("\n========================================\n")
cat("TOP 10 STATES BY CHILD VICTIM PERCENTAGE\n")
cat("========================================\n")

top_child_percentage <- df_clean |>
    filter(total_victims > 0) |>
    select(state_ut, child_percentage, total_child_victims, total_victims) |>
    arrange(desc(child_percentage)) |>
    head(10)

print(top_child_percentage)

# Age Distribution Analysis (All India)
cat("\n========================================\n")
cat("AGE-WISE DISTRIBUTION (ALL INDIA)\n")
cat("========================================\n")

age_distribution <- df_clean |>
    summarise(
        below_6 = sum(child_below_6, na.rm = TRUE),
        age_6_12 = sum(child_6_to_12, na.rm = TRUE),
        age_12_16 = sum(child_12_to_16, na.rm = TRUE),
        age_16_18 = sum(child_16_to_18, na.rm = TRUE),
        age_18_30 = sum(women_18_to_30, na.rm = TRUE),
        age_30_45 = sum(women_30_to_45, na.rm = TRUE),
        age_45_60 = sum(women_45_to_60, na.rm = TRUE),
        above_60 = sum(women_above_60, na.rm = TRUE)
    )

print(age_distribution)

# Calculate percentage for each age group
age_dist_pct <- age_distribution |>
    mutate(
        total = sum(c_across(everything())),
        across(everything(), ~ round((.x / total) * 100, 2))
    ) |>
    select(-total)

cat("\nAge Distribution (Percentage):\n")
print(age_dist_pct)

# Case Category Distribution
cat("\n========================================\n")
cat("STATES BY CASE VOLUME CATEGORY\n")
cat("========================================\n")

category_summary <- df_clean |>
    group_by(case_category) |>
    summarise(
        num_states = n(),
        total_cases = sum(cases_reported, na.rm = TRUE),
        avg_cases = round(mean(cases_reported, na.rm = TRUE), 2)
    ) |>
    arrange(desc(total_cases))

print(category_summary)

# ============================================
# 5. VISUALIZATIONS
# ============================================

# Saving Plots
pdf("Output/CSV/CSV_Generated_Plots.pdf", width = 10, height = 8)

# Set theme for all plots
theme_set(theme_minimal())

# Plot 1: Top 15 States by Cases Reported
cat("\nCreating visualizations...\n")

p1 <- df_clean |>
    head(15) |>
    ggplot(aes(x = reorder(state_ut, cases_reported), y = cases_reported)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = cases_reported), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
        title = "Top 15 States/UTs by Rape Cases Reported (2022)",
        subtitle = "Source: National Crime Records Bureau (NCRB)",
        x = "State/UT",
        y = "Number of Cases Reported"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text = element_text(size = 9)
    )

print(p1)

# Plot 2: Child vs Adult Victims (Stacked Bar)
p2 <- df_clean |>
    head(15) |>
    select(state_ut, total_child_victims, total_women_victims) |>
    pivot_longer(
        cols = c(total_child_victims, total_women_victims),
        names_to = "victim_type",
        values_to = "count"
    ) |>
    ggplot(aes(x = reorder(state_ut, count), y = count, fill = victim_type)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    labs(
        title = "Child vs Adult Victims by State/UT (Top 15 States)",
        subtitle = "Comparison of victim demographics",
        x = "State/UT",
        y = "Number of Victims",
        fill = "Victim Type"
    ) +
    scale_fill_manual(
        values = c("total_child_victims" = "#E74C3C", "total_women_victims" = "#3498DB"),
        labels = c("Child Victims (Below 18)", "Adult Victims (18+)")
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom"
    )

print(p2)

# Plot 3: Age Distribution (All India)
age_dist_df <- data.frame(
    age_group = c("<6", "6-12", "12-16", "16-18", "18-30", "30-45", "45-60", "60+"),
    count = as.numeric(age_distribution),
    category = c(rep("Children", 4), rep("Adults", 4))
)

p3 <- ggplot(age_dist_df, aes(x = factor(age_group, levels = age_group), y = count, fill = category)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
    labs(
        title = "Age-wise Distribution of Victims Across India (2022)",
        subtitle = "Total victims across all states/UTs",
        x = "Age Group (Years)",
        y = "Number of Victims",
        fill = "Category"
    ) +
    scale_fill_manual(values = c("Children" = "#E74C3C", "Adults" = "#3498DB")) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5)
    )

print(p3)

# Plot 4: Child Victim Percentage by State
p4 <- df_clean |>
    filter(total_victims > 0) |>
    arrange(desc(child_percentage)) |>
    head(15) |>
    ggplot(aes(x = reorder(state_ut, child_percentage), y = child_percentage)) +
    geom_bar(stat = "identity", fill = "#E67E22", alpha = 0.8) +
    geom_text(aes(label = paste0(child_percentage, "%")), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
        title = "States with Highest Child Victim Percentage (2022)",
        subtitle = "Top 15 states by proportion of child victims",
        x = "State/UT",
        y = "Child Victims (%)"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40")
    )

print(p4)

# Plot 5: Case Volume Categories (Pie Chart)
category_data <- df_clean |>
    count(case_category) |>
    mutate(percentage = round((n / sum(n)) * 100, 1))

p5 <- ggplot(category_data, aes(x = "", y = n, fill = case_category)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(n, " states\n(", percentage, "%)")),
        position = position_stack(vjust = 0.5), size = 3.5
    ) +
    labs(
        title = "Distribution of States by Case Volume Category",
        fill = "Category"
    ) +
    theme_void() +
    theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "right"
    ) +
    scale_fill_brewer(palette = "Set2")

print(p5)

# Plot 6: Comparison of Top 10 States - Detailed Age Breakdown
p6 <- df_clean |>
    head(10) |>
    select(
        state_ut, child_below_6, child_6_to_12, child_12_to_16,
        child_16_to_18, women_18_to_30, women_30_to_45,
        women_45_to_60, women_above_60
    ) |>
    pivot_longer(cols = -state_ut, names_to = "age_group", values_to = "count") |>
    mutate(
        age_group = factor(age_group,
            levels = c(
                "child_below_6", "child_6_to_12", "child_12_to_16",
                "child_16_to_18", "women_18_to_30", "women_30_to_45",
                "women_45_to_60", "women_above_60"
            ),
            labels = c(
                "<6", "6-12", "12-16", "16-18",
                "18-30", "30-45", "45-60", "60+"
            )
        )
    ) |>
    ggplot(aes(x = state_ut, y = count, fill = age_group)) +
    geom_bar(stat = "identity", position = "fill") +
    coord_flip() +
    labs(
        title = "Age-wise Victim Distribution (Top 10 States)",
        subtitle = "Proportional breakdown by age group",
        x = "State/UT",
        y = "Proportion",
        fill = "Age Group"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Spectral") +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "right"
    )

print(p6)

# ============================================
# 6. SAVE ALL OUTPUTS
# ============================================

cat("\nSaving outputs...\n")

# Save cleaned data
write.csv(df_clean, "Output/CSV/ncrb_rape_data_2022_cleaned.csv", row.names = FALSE)

# Save analysis results
sink("Output/CSV//ncrb_analysis_report_2022.txt")
cat("=", rep("=", 60), "\n", sep = "")
cat("NCRB RAPE VICTIMS DATA ANALYSIS REPORT - 2022\n")
cat("=", rep("=", 60), "\n\n", sep = "")

cat("OVERALL STATISTICS - ALL INDIA\n")
cat("-", rep("-", 60), "\n", sep = "")
print(total_stats)

cat("\n\nTOP 10 STATES BY CASES REPORTED\n")
cat("-", rep("-", 60), "\n", sep = "")
print(top_10_states)

cat("\n\nBOTTOM 10 STATES BY CASES REPORTED\n")
cat("-", rep("-", 60), "\n", sep = "")
print(bottom_10_states)

cat("\n\nTOP 10 STATES BY CHILD VICTIM PERCENTAGE\n")
cat("-", rep("-", 60), "\n", sep = "")
print(top_child_percentage)

cat("\n\nAGE-WISE DISTRIBUTION (ALL INDIA)\n")
cat("-", rep("-", 60), "\n", sep = "")
cat("Absolute Numbers:\n")
print(age_distribution)
cat("\nPercentage Distribution:\n")
print(age_dist_pct)

cat("\n\nSTATES BY CASE VOLUME CATEGORY\n")
cat("-", rep("-", 60), "\n", sep = "")
print(category_summary)

cat("\n\nREPORT GENERATED ON:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
sink()

# Save plots
ggsave("Output/CSV//plot1_top15_states_cases.png", p1, width = 12, height = 8, dpi = 300)
ggsave("Output/CSV/plot2_child_vs_adult_victims.png", p2, width = 12, height = 8, dpi = 300)
ggsave("Output/CSV/plot3_age_distribution_india.png", p3, width = 12, height = 7, dpi = 300)
ggsave("Output/CSV/plot4_child_victim_percentage.png", p4, width = 12, height = 8, dpi = 300)
ggsave("Output/CSV/plot5_case_volume_categories.png", p5, width = 10, height = 8, dpi = 300)
ggsave("Output/CSV/plot6_age_breakdown_top10.png", p6, width = 12, height = 8, dpi = 300)

# ============================================
# 7. CREATE SUMMARY DASHBOARD DATA
# ============================================

# Create a summary for quick reference
dashboard_summary <- list(
    report_title = "NCRB Rape Victims Analysis 2022",
    report_date = Sys.Date(),
    key_metrics = total_stats,
    top_5_states = head(top_10_states, 5),
    age_distribution = age_distribution,
    category_summary = category_summary
)

# Save as RDS for later use
saveRDS(dashboard_summary, "Output/CSV/dashboard_summary.rds")

# ============================================
# 8. FINAL SUMMARY
# ============================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Files Created:\n")
cat("  ✓ ncrb_rape_data_2022_cleaned.csv - Cleaned dataset\n")
cat("  ✓ ncrb_analysis_report_2022.txt - Detailed text report\n")
cat("  ✓ dashboard_summary.rds - Summary data for dashboard\n")
cat("  ✓ plot1_top15_states_cases.png\n")
cat("  ✓ plot2_child_vs_adult_victims.png\n")
cat("  ✓ plot3_age_distribution_india.png\n")
cat("  ✓ plot4_child_victim_percentage.png\n")
cat("  ✓ plot5_case_volume_categories.png\n")
cat("  ✓ plot6_age_breakdown_top10.png\n")

cat("\nKey Findings:\n")
cat("  • Total Cases Reported:", total_stats$total_cases, "\n")
cat("  • Total Victims:", total_stats$total_all_victims, "\n")
cat(
    "  • Child Victims:", total_stats$total_child_victims,
    "(", total_stats$child_victim_percentage, "%)\n"
)
cat(
    "  • Adult Victims:", total_stats$total_women_victims,
    "(", total_stats$women_victim_percentage, "%)\n"
)
cat(
    "  • State with Most Cases:", top_10_states$state_ut[1],
    "(", top_10_states$cases_reported[1], "cases)\n"
)

cat("\n", rep("=", 70), "\n", sep = "")
