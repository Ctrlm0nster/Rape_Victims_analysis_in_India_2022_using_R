# NCRB Rape Victims Data 2022 – Descriptive Statistical Analysis

## 📌 Project Overview

This project performs a **descriptive statistical analysis** of rape victims data in India for the year **2022**, published by the **National Crime Records Bureau (NCRB)**. The objective is to summarize patterns in:

* State/UT-wise reported cases
* Child versus adult victim distribution
* Victim age-group distribution

No predictive modeling or causal inference is conducted — the analysis is strictly descriptive.

---

## 📂 Data Source

* **Dataset:** *State/UT-wise Number of Women and Girls Victims of Rape during 2022*
* **Publisher:** National Crime Records Bureau (NCRB), Government of India
* **Reference URL:**
  [https://ncrb.gov.in/uploads/nationalcrimerecordsbureau/custom/1701607577CrimeinIndia2022Book1.pdf](https://ncrb.gov.in/uploads/nationalcrimerecordsbureau/custom/1701607577CrimeinIndia2022Book1.pdf)
* **Granularity:** Annual
* **Records:** 40 States/UTs

⚠️ **Note:**
States/UTs should not be compared purely on the basis of raw crime counts, as reporting practices and population sizes vary. Clarifications from Nagaland were pending at the time of reporting.

---

## 🔄 Data Processing Approach

Two independent scripts were developed to analyze the **same dataset** using different formats:

* **CSV-based Script:** Reads and processes tabular data from CSV files.
* **XML-based Script:** Parses equivalent data stored in XML format and performs identical transformations and analysis.

Both workflows generate the **same statistical summaries and visualizations**, validating format-independent reproducibility.

---

## 📊 Summary Statistics

| Metric                      | Value |
| --------------------------- | ----- |
| Total reported cases        | 8,062 |
| Total child victims         | 498   |
| Total adult women victims   | 7,947 |
| Average cases per State/UT  | 806   |
| Percentage of child victims | ~5.9% |

---

## 📈 Visualizations Generated

All plots are saved in the `Output/` directory using `ggsave()`.

| Plot                   | File                                | Description                                                               |
| ---------------------- | ----------------------------------- | ------------------------------------------------------------------------- |
| Top States by Cases    | `Output/top_states_cases.png`       | Horizontal bar chart of the top 10 States/UTs by number of reported cases |
| Child vs Adult Victims | `Output/child_vs_adult_victims.png` | Comparison of child and adult women victims by State                      |
| Age Distribution       | `Output/age_distribution.png`       | Victim counts across predefined age groups                                |

---

## 💾 Output Files and Logging

Upon successful execution, the scripts generate the following outputs:

* `rape_victims_data_2022.csv` — Cleaned and structured dataset
* `summary_statistics.txt` — Descriptive summary statistics
* PNG plots in the `Output/` directory

Console messages confirm successful completion:

```
✓ Data extracted and analyzed successfully!
✓ CSV file saved: rape_victims_data_2022.csv
✓ Summary saved: summary_statistics.txt
✓ Plots saved as PNG files
✓ All outputs saved in the 'Output' directory
```

---

## 🧪 Methodology

The analysis includes:

* Frequency counts and totals
* Percentages and averages
* State/UT ranking by reported cases
* Age-group aggregation

This is a **descriptive analysis only** — no inferential statistics or predictive modeling are used.

---

## 🛠 Tools & Technologies

* **Language:** R
* **Libraries:** `ggplot2`, `dplyr`, `tidyr`, `xml2`
* **Visualization Export:** `ggsave()`

---

## 📁 Project Structure

```
├── Data/
│   ├── NCRB_DATA.3.csv
│   └── NCRB_DATA.xml
├── Scripts/
│   ├── analysis_csv.R
│   └── analysis_xml.R
├── Output/
│   ├── top_states_cases.png
│   ├── child_vs_adult_victims.png
│   |── age_distribution.png
│   |── rape_victims_data_2022.csv
│   └── summary_statistics.txt
└── README.md
```

---

## ⚖️ Disclaimer

* The analysis reflects **reported cases**, not actual incidence.
* Crime figures vary with population size, reporting behavior, and administrative practices.
* Results should be interpreted in appropriate social and demographic context.

---

## ✅ Key Takeaways

* A small group of States accounts for a large share of reported cases.
* Adult women constitute the majority of victims, though children account for ~6%.
* The most affected age group is **18–30 years**, followed by **30–45 years**.

---
