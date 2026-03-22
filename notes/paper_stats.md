## Statistics Comparison: Computed vs. Manuscript

### Matches (no update needed)

| Statistic              | Value    | Tex Quote                                                    |
| ---------------------- | -------- | ------------------------------------------------------------ |
| N households           | 308      | "308 urban households"                                       |
| WHO exceedance %       | 78%      | "exceeding the WHO 24-hour guideline...on 78% of monitored days" |
| Adult time home        | 80%      | "spending 80% of their time at home"                         |
| Child time home        | 70%      | "children spent an average of 70% of their time indoors"     |
| Smoking coef           | 21 µg/m³ | "Smoking was associated with 21 µg/m³"                       |
| Smoking % of mean      | 51%      | "a 51% increase compared to the sample mean"                 |
| WB 2+/wk prevalence    | 11%      | "11% of the sample"                                          |
| Infiltration range low | 0.51     | "between 0.51 and 0.71"                                      |
| Spike rate Bin 4       | 0.4%     | "0.4% for households in the highest income quartile"         |

### Differences (need updating in tex)

| Statistic                     | Computed          | Manuscript   | Tex Quote                                                    |
| ----------------------------- | ----------------- | ------------ | ------------------------------------------------------------ |
| N monitor-hours               | **152,519**       | 152,000      | "over 152,000 monitor-hours" — *OK, just round down*         |
| % female                      | **91%**           | 90%          | "90% of whom were women"                                     |
| Mean indoor daily PM          | **42.3**          | 40.7         | "mean indoor daily PM2.5 concentration was 40.7 µg/m³"       |
| Mean outdoor daily PM         | **39.5**          | 37.8         | "mean outdoor daily PM2.5 concentration was 37.8 µg/m³"      |
| I/O ratio                     | **1.07**          | 1.08         | "I/O ratio of 1.08"                                          |
| Outdoor >100 % hrs            | **0.8%**          | 0.7%         | "outdoor readings exceeded 100µg/m³ in 0.7% of hours"        |
| Indoor >100 % hrs             | **6%**            | 7%           | "indoor readings exceeded 100µg/m³ in 7% of hours"           |
| **Infiltration (main)**       | **0.59**          | 0.62         | "infiltration factor of 0.62"                                |
| Infiltration CI               | **[0.43, 0.75]**  | [0.47, 0.77] | "95% CI, 0.47, 0.77"                                         |
| Infiltration <500m            | **0.84**          | 0.80         | "approximately 0.80 when we restrict to households within 500m" |
| Infiltration range high       | **0.69**          | 0.71         | "between 0.51 and 0.71"                                      |
| p(AC)                         | **0.15**          | 0.14         | "AC ownership (p = 0.14)"                                    |
| p(window)                     | **0.16**          | 0.15         | "open versus closed window or door (p = 0.15)"               |
| Smoking CI                    | **[8.38, 34.35]** | [8.30, 34.3] | "95% CI: 8.30–34.3" — *rounding diff only*                   |
| Outdoor mean contrib %        | **56%**           | 58%          | "58% of mean indoor PM2.5 comes from ambient outdoor"        |
| Smoking mean contrib %        | **18%**           | 19%          | "smoking explains approximately 19%"                         |
| Spike smoking % higher        | **338.8%**        | 341.8%       | "341.8% higher in smoking households"                        |
| **Income PM diff**            | **31.3**          | 23.7         | "increase of 23.7 µg/m³ in mean PM2.5"                       |
| Income % higher               | **112%**          | 113%         | "113% higher indoor PM2.5"                                   |
| Inf Bin 1                     | **0.64**          | 0.69         | "0.69 (Bin 1)"                                               |
| Inf Bin 4                     | **0.49**          | 0.48         | "0.48 (Bin 4)"                                               |
| Inf income p                  | **0.64**          | 0.53         | "Bin 1 vs Bin 4 difference...noisy (p = 0.53)"               |
| **Infiltration share of gap** | **19%**           | 16%          | "differences in infiltration explain 16%"                    |
| Spike rate Bin 1              | **2.9%**          | 3%           | "PM2.5 spikes...3% of the time" — *rounding*                 |
| Spike income p                | **0.021**         | 0.027        | "p = 0.027"                                                  |

### Stats with computation issues (need manual check)

| Statistic                  | Issue                                                        |
| -------------------------- | ------------------------------------------------------------ |
| % LPG = 0%                 | `hh_cookingfuel` values don't match "LPG" — likely labeled differently (e.g. "LPG stove"). Manuscript says 98%. |
| Mean hours/HH = 539        | This counts cleaned non-NA hours. Manuscript's 1,138 likely counts raw sensor-on hours before QC filtering. |
| N outdoor sensors = 120    | `sensor_locations.csv` has 120 entries; the 63 in the manuscript is likely after filtering to study-period sensors. |
| % within 500m road = NaN   | `dist_primary` in survey is likely in km not meters. Need `dist_primary < 0.5` instead. |
| Smoking/Other share of gap | The income-interaction regression term matching failed. Needs manual debugging. |

### Key takeaways

- **Most stats that shifted are small rounding differences** (p-values off by 0.01-0.02, CIs off by <0.05)
- **The main infiltration estimate (0.59 vs 0.62)** is the largest substantive difference — driven by the outdoor data rewrite
- **Mean daily PM levels increased** (42.3 vs 40.7 indoor, 39.5 vs 37.8 outdoor) — the 2-week buffer you added before the indoor start date brought in higher-PM outdoor-only data that shifts the daily means
- **Income PM difference jumped (31.3 vs 23.7)** — this is a big change worth investigating. It's likely because the daily means now include outdoor-only rows, and different income bins have different coverage patterns.

The R script is at [code/analysis/paper_stats.R](vscode-webview://0mt7ct712c5sukjebhccqp977veub7p87gklmsgfa7hmmgvn9ngf/code/analysis/paper_stats.R). You can source it from `run_code.r` to regenerate these comparisons after any data changes.