library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)

# ============================================
# 1. LOAD CSV DATA
# ============================================
cat("\nLoading CSV data...\n")
input_file <- "Output/XML/rape_victims_data_2022.csv"
df_clean <- read_csv(input_file, show_col_types = FALSE)

# Ensure output directory exists
dir.create("Output/CSV", recursive = TRUE, showWarnings = FALSE)

# ============================================
# 2. RECOMPUTE SUMMARY METRICS
# ============================================
cat("Computing summary statistics...\n")

top_10_states <- df_clean |>
    arrange(desc(cases_reported)) |>
    slice_head(n = 10) |>
    select(state_ut, cases_reported, total_child_victims, total_women_victims)

bottom_10_states <- df_clean |>
    arrange(cases_reported) |>
    slice_head(n = 10) |>
    select(state_ut, cases_reported, total_child_victims, total_women_victims)

total_stats <- df_clean |>
    summarise(
        total_cases = sum(cases_reported, na.rm = TRUE),
        total_child_victims = sum(total_child_victims, na.rm = TRUE),
        total_women_victims = sum(total_women_victims, na.rm = TRUE),
        total_all_victims = sum(total_victims, na.rm = TRUE),
        avg_cases_per_state = mean(cases_reported, na.rm = TRUE),
        child_victim_percentage = round((total_child_victims / total_all_victims) * 100, 2),
        women_victim_percentage = round((total_women_victims / total_all_victims) * 100, 2)
    )

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

age_dist_pct <- round((age_distribution / sum(age_distribution)) * 100, 2)

top_child_percentage <- df_clean |>
    filter(total_victims > 0) |>
    arrange(desc(child_percentage)) |>
    slice_head(n = 10) |>
    select(state_ut, child_percentage, total_victims)

category_summary <- df_clean |>
    count(case_category) |>
    mutate(percentage = round((n / sum(n)) * 100, 1))

# ============================================
# 3. VISUALIZATIONS
# ============================================
cat("\nCreating visualizations...\n")

theme_set(theme_minimal())

# ---- Plot 1: Top 15 States by Cases ----
cat("  Creating Plot 1: Top 15 States by Cases...\n")
p1 <- df_clean |>
    arrange(desc(cases_reported)) |>
    slice_head(n = 15) |>
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

ggsave("Output/CSV/plot1_top15_states_cases.png", p1, width = 12, height = 8, dpi = 300)

# ---- Plot 2: Child vs Adult Victims ----
cat("  Creating Plot 2: Child vs Adult Victims...\n")
p2 <- df_clean |>
    arrange(desc(total_victims)) |>
    slice_head(n = 15) |>
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

ggsave("Output/CSV/plot2_child_vs_adult_victims.png", p2, width = 12, height = 8, dpi = 300)

# ---- Plot 3: Age Distribution (All India) ----
cat("  Creating Plot 3: Age Distribution...\n")
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

ggsave("Output/CSV/plot3_age_distribution_india.png", p3, width = 12, height = 7, dpi = 300)

# ---- Plot 4: Child Victim Percentage by State ----
cat("  Creating Plot 4: Child Victim Percentage...\n")
p4 <- df_clean |>
    filter(total_victims > 0) |>
    arrange(desc(child_percentage)) |>
    slice_head(n = 15) |>
    ggplot(aes(x = reorder(state_ut, child_percentage), y = child_percentage)) +
    geom_bar(stat = "identity", fill = "#E67E22", alpha = 0.8) +
    geom_text(aes(label = paste0(round(child_percentage, 1), "%")), hjust = -0.2, size = 3) +
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

ggsave("Output/CSV/plot4_child_victim_percentage.png", p4, width = 12, height = 8, dpi = 300)

# ---- Plot 5: Case Volume Categories (Pie Chart) ----
cat("  Creating Plot 5: Case Volume Categories...\n")
p5 <- ggplot(category_summary, aes(x = "", y = n, fill = case_category)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(n, " states\n(", percentage, "%)")),
        position = position_stack(vjust = 0.5),
        size = 3.5
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

ggsave("Output/CSV/plot5_case_volume_categories.png", p5, width = 10, height = 8, dpi = 300)

# ---- Plot 6: Age Breakdown for Top 10 States ----
cat("  Creating Plot 6: Age Breakdown for Top 10 States...\n")
p6 <- df_clean |>
    arrange(desc(cases_reported)) |>
    slice_head(n = 10) |>
    select(
        state_ut,
        child_below_6, child_6_to_12, child_12_to_16, child_16_to_18,
        women_18_to_30, women_30_to_45, women_45_to_60, women_above_60
    ) |>
    pivot_longer(cols = -state_ut, names_to = "age_group", values_to = "count") |>
    mutate(
        age_group = factor(age_group,
            levels = c(
                "child_below_6", "child_6_to_12", "child_12_to_16", "child_16_to_18",
                "women_18_to_30", "women_30_to_45", "women_45_to_60", "women_above_60"
            ),
            labels = c("<6", "6-12", "12-16", "16-18", "18-30", "30-45", "45-60", "60+")
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
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Spectral") +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "right"
    )

ggsave("Output/CSV/plot6_age_breakdown_top10.png", p6, width = 12, height = 8, dpi = 300)

# ---- Create PDF with all plots ----
cat("  Creating PDF with all plots...\n")
pdf("Output/CSV/CSV_Generated_Plots.pdf", width = 10, height = 8)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

# ============================================
# 4. SAVE ALL OUTPUTS
# ============================================
cat("\nSaving outputs...\n")

# Save cleaned data copy
write_csv(df_clean, "Output/CSV/ncrb_rape_data_2022_cleaned.csv")

# Save analysis report
sink("Output/CSV/ncrb_analysis_report_2022.txt")
cat("============================================================\n")
cat("NCRB RAPE VICTIMS DATA ANALYSIS REPORT - 2022\n")
cat("============================================================\n\n")

cat("OVERALL STATISTICS - ALL INDIA\n")
cat("------------------------------------------------------------\n")
print(total_stats)

cat("\n\nTOP 10 STATES BY CASES REPORTED\n")
cat("------------------------------------------------------------\n")
print(top_10_states)

cat("\n\nBOTTOM 10 STATES BY CASES REPORTED\n")
cat("------------------------------------------------------------\n")
print(bottom_10_states)

cat("\n\nTOP 10 STATES BY CHILD VICTIM PERCENTAGE\n")
cat("------------------------------------------------------------\n")
print(top_child_percentage)

cat("\n\nAGE-WISE DISTRIBUTION (ALL INDIA)\n")
cat("------------------------------------------------------------\n")
cat("Absolute Numbers:\n")
print(age_distribution)
cat("\nPercentage Distribution:\n")
print(age_dist_pct)

cat("\n\nSTATES BY CASE VOLUME CATEGORY\n")
cat("------------------------------------------------------------\n")
print(category_summary)

cat("\n\nREPORT GENERATED ON: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
sink()

# ============================================
# 5. CREATE DASHBOARD SUMMARY OBJECT
# ============================================
dashboard_summary <- list(
    report_title = "NCRB Rape Victims Analysis 2022",
    report_date = Sys.Date(),
    key_metrics = total_stats,
    top_5_states = head(top_10_states, 5),
    age_distribution = age_distribution,
    category_summary = category_summary
)

saveRDS(dashboard_summary, "Output/CSV/dashboard_summary.rds")

# ============================================
# 6. FINAL SUMMARY
# ============================================
cat("\n======================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("======================================================================\n\n")
cat("Files Created:\n")
cat(" ✓ ncrb_rape_data_2022_cleaned.csv\n")
cat(" ✓ ncrb_analysis_report_2022.txt\n")
cat(" ✓ dashboard_summary.rds\n")
cat(" ✓ CSV_Generated_Plots.pdf\n")
cat(" ✓ plot1_top15_states_cases.png\n")
cat(" ✓ plot2_child_vs_adult_victims.png\n")
cat(" ✓ plot3_age_distribution_india.png\n")
cat(" ✓ plot4_child_victim_percentage.png\n")
cat(" ✓ plot5_case_volume_categories.png\n")
cat(" ✓ plot6_age_breakdown_top10.png\n\n")

cat("Key Findings:\n")
cat(" • Total Cases Reported:", total_stats$total_cases, "\n")
cat(" • Total Victims:", total_stats$total_all_victims, "\n")
cat(
    " • Child Victims:", total_stats$total_child_victims,
    "(", total_stats$child_victim_percentage, "%)\n"
)
cat(
    " • Adult Victims:", total_stats$total_women_victims,
    "(", total_stats$women_victim_percentage, "%)\n"
)
cat(
    " • State with Most Cases:", top_10_states$state_ut[1],
    "(", top_10_states$cases_reported[1], "cases)\n"
)

cat("\n======================================================================\n")
