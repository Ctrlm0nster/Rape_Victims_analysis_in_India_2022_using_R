library(httr)
library(xml2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ============================================
# 1. FETCH AND PARSE DATA
# ============================================

# Fetch data from API
url <- "https://api.data.gov.in/resource/0a096e81-5a1b-4e23-9f28-1abd1db76d16"
response <- GET(
    url = url,
    query = list(
        `api-key` = "579b464db66ec23bdd000001cdd3946e44ce4aad7209ff7b23ac571b",
        format = "xml",
        limit = 50 # Increase limit to get all 40 records
    )
)

# Parse XML
xml_content <- content(response, "text", encoding = "UTF-8")
xml_doc <- read_xml(xml_content)

# ============================================
# 2. EXTRACT METADATA
# ============================================

metadata <- list(
    title = xml_text(xml_find_first(xml_doc, "//title")),
    description = xml_text(xml_find_first(xml_doc, "//desc")),
    source = xml_text(xml_find_first(xml_doc, "//source")),
    total_records = as.numeric(xml_text(xml_find_first(xml_doc, "//total"))),
    updated_date = xml_text(xml_find_first(xml_doc, "//updated_date"))
)

print(metadata)

# ============================================
# 3. EXTRACT RECORDS TO DATA FRAME
# ============================================

# Get all record items
records <- xml_find_all(xml_doc, "//records/item")

# Extract data from each record
df <- map_df(records, function(record) {
    data.frame(
        sl_no = as.numeric(xml_text(xml_find_first(record, ".//sl__no_"))),
        state_ut = xml_text(xml_find_first(record, ".//state_ut")),
        cases_reported = as.numeric(xml_text(xml_find_first(record, ".//cases_reported___col__3_"))),

        # Child victims by age group
        child_below_6 = as.numeric(xml_text(xml_find_first(record, ".//child_victims_of_rape__below_18_yrs____below_6_years___col__4_"))),
        child_6_to_12 = as.numeric(xml_text(xml_find_first(record, ".//child_victims_of_rape__below_18_yrs____6_years_and_above___below_12_years___col__5_"))),
        child_12_to_16 = as.numeric(xml_text(xml_find_first(record, ".//child_victims_of_rape__below_18_yrs____12_years_and_above___below_16_years___col__6_"))),
        child_16_to_18 = as.numeric(xml_text(xml_find_first(record, ".//child_victims_of_rape__below_18_yrs____16_years_and_above___below_18_years___col__7_"))),
        total_child_victims = as.numeric(xml_text(xml_find_first(record, ".//child_victims_of_rape__below_18_yrs____total_girl___child_victims___col__8_"))),

        # Women victims by age group
        women_18_to_30 = as.numeric(xml_text(xml_find_first(record, ".//women_victims_of_rape__above_18_years____18_years_and_above___below_30_years___col__9_"))),
        women_30_to_45 = as.numeric(xml_text(xml_find_first(record, ".//women_victims_of_rape__above_18_years____30_years_and_above___below_45_years___col__10_"))),
        women_45_to_60 = as.numeric(xml_text(xml_find_first(record, ".//women_victims_of_rape__above_18_years____45_years_and_above___below_60_years___col__11_"))),
        women_above_60 = as.numeric(xml_text(xml_find_first(record, ".//women_victims_of_rape__above_18_years____60_years_and_above___col__12_"))),
        total_women_victims = as.numeric(xml_text(xml_find_first(record, ".//women_victims_of_rape__above_18_years____total_women___adult_victims___col__13_"))),
        total_victims = as.numeric(xml_text(xml_find_first(record, ".//total_victims__col_8_col_13____col__14_"))),
        stringsAsFactors = FALSE
    )
})

# ============================================
# 4. DATA CLEANING & TRANSFORMATION
# ============================================

# Clean data
df_clean <- df |>
    mutate(
        # Calculate percentages
        child_percentage = (total_child_victims / total_victims) * 100,
        women_percentage = (total_women_victims / total_victims) * 100,

        # Categorize states by case volume
        case_category = case_when(
            cases_reported < 100 ~ "Low (<100)",
            cases_reported < 500 ~ "Medium (100-500)",
            cases_reported < 1000 ~ "High (500-1000)",
            TRUE ~ "Very High (>1000)"
        )
    ) |>
    arrange(desc(cases_reported))

# View summary
print(head(df_clean, 10))
summary(df_clean)

# ============================================
# 5. ANALYSIS
# ============================================

# Top 10 states by total cases
top_10_states <- df_clean |>
    slice_max(cases_reported, n = 10) |>
    select(state_ut, cases_reported, total_child_victims, total_women_victims)

print("Top 10 States by Cases Reported:")
print(top_10_states)

# Summary statistics
total_stats <- df_clean |>
    summarise(
        total_cases = sum(cases_reported, na.rm = TRUE),
        total_child_victims = sum(total_child_victims, na.rm = TRUE),
        total_women_victims = sum(total_women_victims, na.rm = TRUE),
        avg_cases_per_state = mean(cases_reported, na.rm = TRUE),
        child_victim_percentage = (sum(total_child_victims, na.rm = TRUE) / sum(total_victims, na.rm = TRUE)) * 100
    )

print("Overall Statistics:")
print(total_stats)

# Age distribution analysis
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

print("Age Distribution of Victims:")
print(age_distribution)

# ============================================
# 6. VISUALIZATIONS
# ============================================

# Saving Plots
pdf("Output/XML//XML_Generated_Plots.pdf", width = 10, height = 8)

# Plot 1: Top 15 states by cases
p1 <- df_clean |>
    slice_max(cases_reported, n = 15) |>
    ggplot(aes(x = reorder(state_ut, cases_reported), y = cases_reported)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 15 States/UTs by Rape Cases Reported (2022)",
        x = "State/UT",
        y = "Number of Cases"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

print(p1)

# Plot 2: Child vs Women victims
p2 <- df_clean |>
    slice_max(total_victims, n = 15) |>
    select(state_ut, total_child_victims, total_women_victims) |>
    tidyr::pivot_longer(
        cols = c(total_child_victims, total_women_victims),
        names_to = "victim_type",
        values_to = "count"
    ) |>
    ggplot(aes(x = reorder(state_ut, count), y = count, fill = victim_type)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    labs(
        title = "Child vs Adult Victims by State/UT (2022)",
        x = "State/UT",
        y = "Number of Victims",
        fill = "Victim Type"
    ) +
    scale_fill_manual(
        values = c("total_child_victims" = "#E74C3C", "total_women_victims" = "#3498DB"),
        labels = c("Child Victims", "Adult Victims")
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

print(p2)

# Plot 3: Age distribution (all India)
age_dist_df <- data.frame(
    age_group = c("<6", "6-12", "12-16", "16-18", "18-30", "30-45", "45-60", ">60"),
    count = as.numeric(age_distribution)
)

p3 <- ggplot(age_dist_df, aes(x = age_group, y = count)) +
    geom_bar(stat = "identity", fill = "#2ECC71") +
    labs(
        title = "Age Distribution of Victims Across India (2022)",
        x = "Age Group (Years)",
        y = "Number of Victims"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

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
# 7. SAVE RESULTS
# ============================================

# Save data to CSV
write.csv(df_clean, "Output/XML//rape_victims_data_2022.csv", row.names = FALSE)

# Save summary statistics
sink("Output/XML//summary_statistics.txt")
cat("=== NCRB Rape Victims Data 2022 - Summary Statistics ===\n\n")
cat("Dataset Information:\n")
print(metadata)
cat("\n\nOverall Statistics:\n")
print(total_stats)
cat("\n\nTop 10 States:\n")
print(top_10_states)
cat("\n\nAge Distribution:\n")
print(age_distribution)
sink()

# Save plots
ggsave("Output/XML//top_states_cases.png", p1, width = 10, height = 8)
ggsave("Output/XML//child_vs_adult_victims.png", p2, width = 10, height = 8)
ggsave("Output/XML//age_distribution.png", p3, width = 10, height = 6)

cat("\n✓ Data extracted and analyzed successfully!\n")
cat("✓ CSV file saved: rape_victims_data_2022.csv\n")
cat("✓ Summary saved: summary_statistics.txt\n")
cat("✓ Plots saved as PNG files\n")
cat("✓ All outputs saved in the 'Output' directory\n")
