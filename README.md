# marketing-mix-modeling-robyn
[mmm_readme.md](https://github.com/user-attachments/files/27529958/mmm_readme.md)
# 📊 Marketing Mix Modeling with Robyn & Linear Regression

> Internship project @ **Technozor** | Feb–May 2025  
> Predictive marketing budget optimization using Meta's Robyn MMM framework

---

## 🧭 Context

This project was developed during my Data Analyst internship at Technozor, within the Predictive Marketing practice. The goal was to build a robust Marketing Mix Model (MMM) to help the marketing team understand the contribution of each media channel to conversions, and to simulate how reallocating budget could improve performance.

---

## 🎯 Objective

- Quantify the impact of paid media channels (SEM, Social, Affiliates, Display, TV-like networks) on daily conversions
- Model adstock and saturation effects using the **Robyn** framework (Meta's open-source MMM library)
- Compare Robyn's Bayesian/ML approach against a baseline **Linear Regression** model
- Generate actionable **budget reallocation scenarios** to maximize response

---

## 🗂️ Repository Structure

```
marketing-mix-modeling-robyn/
│
├── R/
│   └── MMM&LR&DynamicFE.R       # Full modeling pipeline (Robyn + LR)
│
├── data/
│   └── marketing_data_sample.csv       # Anonymized/synthetic dataset (268 daily records)
│
├── results/
│   ├── model_2_172_8_onepager.png      # Best model one-pager (decomposition, response curves)
│   ├── budget_allocation_2_172_8.png   # Budget allocation scenarios
│
├── report/
│   └── MMM_Academic_Report.pdf         # Full methodology and findings report
│
├── presentation/
│   └── MMM_Presentation.pptx           # Slide deck summarizing the project
│
└── README.md
```

---

## ⚙️ Methodology

### 1. Data & Feature Engineering
- Daily marketing data covering **268 days** (March–December 2023)
- Raw channels: SEM, SNS, Affiliates (AFF), Display (DOUGA), TV Ad Networks (ADNW1–3), Brand search, Influencer & Game PR campaigns
- **Data-driven composite features**: rather than using fixed weights, channel combinations (`Digital_Weighted`, `TV_Like_Weighted`, `SEM_TimeWeighted`) are built using regression-estimated weights from normalized data, so the mix reflects actual contribution to conversions:
  - `Digital_Weighted`: SEM (42.4%), AFF (30.8%), SNS (21.3%), DOUGA (5.5%)
  - `TV_Like_Weighted`: ADNW2 (50.1%), ADNW1 (44.7%), ADNW3 (5.3%)
  - `SEM_TimeWeighted`: SEM (43.8%), SEM_lag2 (34.9%), SEM_lag1 (21.3%)
- Created campaign event dummies and grouped them into `INFLU_campaigns`, `GAMEPR_campaigns`, `CP_campaigns`

### 2. Robyn MMM (Meta's Open-Source Framework)
- **Adstock**: Geometric decay — models the lagged, diminishing effect of ad spend
- **Saturation**: Hill function — captures diminishing returns at high spend levels
- **Hyperparameter optimization**: TwoPointsDE Nevergrad algorithm — 5 trials × 2,000 iterations = 10,000 models evaluated
- **Pareto front selection**: 4 fronts, 101 Pareto-optimal models, auto-clustered into 6 groups
- **Best model selected**: `2_172_8` (scored by composite formula: 40% test R² + 30% test NRMSE + 30% DECOMP.RSSD — balancing predictive accuracy with media decomposition quality)

### 3. Linear Regression Baseline
- Same feature set, 70/30 train-test split
- Used as an interpretability and benchmark comparison against Robyn

---

## 📈 Key Results

### Best Robyn Model (`2_172_8`)
| Metric | Train | Validation | Test |
|--------|-------|-----------|------|
| Adj. R² | 0.855 | 0.511 | 0.899 |
| NRMSE | 0.052 | 0.157 | 0.087 |
| DECOMP.RSSD | — | — | 0.006 |

### Linear Regression Baseline
| Metric | Value |
|--------|-------|
| R² | 0.921 |
| RMSE | 48.04 |

### Channel Contribution (Model `2_172_8`)
| Channel | Effect Share | Spend Share | Mean CPA |
|---------|-------------|------------|---------|
| SEM_TimeWeighted | 43.7% | 43.9% | 635 |
| Digital_Weighted | 40.9% | 41.2% | 633 |
| TV_Like_Weighted | 15.4% | 14.9% | 658 |

### Budget Reallocation Scenarios
With the **same total budget** of 18.7M, Robyn's allocator identified:
- **+23% more conversions** under bounded reallocation (30.4K vs 24.7K baseline)
- **+73.7% more conversions** under Bounded ×3 scenario (42.9K conversions, CPA reduced from 757 → 436)

> TV_Like_Weighted showed the lowest CPA (658) despite its relatively lower spend share, suggesting it was **underinvested** relative to its marginal return potential.
<img width="4200" height="4200" alt="image" src="https://github.com/user-attachments/assets/4830e69c-4fd1-4df1-b825-5dcf3da2e12e" />



## 🛠️ Tech Stack

| Tool | Purpose |
|------|---------|
| R (Robyn v4.4.2) | MMM framework, Pareto optimization, budget allocation |
| Meta Robyn | Bayesian hyperparameter search, adstock/saturation modeling |
| Prophet | Trend & weekday decomposition |
| caret | Linear Regression, train/test split |
| ggplot2 / Cairo | Visualization and export |
| Python (see related repo) | Upstream data collection & automation |
| Excel / Power BI | Reporting and stakeholder presentation |

---

## 🔍 How to Run

1. Clone this repo
2. Install R dependencies:
```r
install.packages(c("Robyn", "reticulate", "readxl", "dplyr", 
                   "tidyverse", "caret", "Cairo"))
```
3. Open `R/MMM&LR&DynamicFE.R` and run `run_mmm_analysis()`
4. Point to your own dataset (must include a `DATE` column and numeric channel columns)
5. Outputs will be saved to `~/MMM/`

> ⚠️ The dataset in `/data/` is anonymized. Channel names and spend values have been normalized for confidentiality.

---

## 📄 Related Documents

- 📘 Full academic report: `/report/MMM_Academic_Report.pdf`
- 📊 Project presentation: `/presentation/MMM_Presentation.pptx`

---

## 👩‍💻 Author

**Azza El Mahbouli**  
Computer Science Graduate | Data Analysis & Marketing Intelligence  
[LinkedIn](https://linkedin.com/in/azza-elmahbouli) · elmahbouliazza123@gmail.com
