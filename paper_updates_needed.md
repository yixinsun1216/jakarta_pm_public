# Paper Updates Needed: Jakarta_PM_New.pdf vs Code

This document identifies places in the manuscript where the text does not align with the current code, plus the new decomposition analysis that needs to be written up.



Make sure lag regression matches main regression specification

---

## 1. Methods Section 4.2: Infiltration Rate Equation (Page 13)

**Current text (Equation 1):**
> PM_it^In = α + β PM_it^Out + π T_it + γ H_it + μ_h + μ_w + ε_it

**What the code does (`fig2_infiltration.R`, line 65):**
The main specification now includes source control variables on the right-hand side:

```
pm25_indoor ~ as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) +
  as.factor(room_pmsource_kitchen) + pm25_outdoor3 + cooking + dist_primary +
  temp_outdoor3 + humidity_outdoor3 | hour + week
```

**Action:** Rewrite Equation (1) to include the source control variables (waste burning indicators, smoking indicator, kitchen proximity indicator, cooking indicator, distance to primary road). Explain why you now control for indoor sources in the infiltration regression — it isolates the outdoor infiltration coefficient from confounding by hyperlocal sources.

---

## 2. Methods Section 4.2: Inverse-Frequency Weighting (Not mentioned anywhere)

**What the code does (`fig2_infiltration.R`, lines 51-57; also in every other code file):**
All regressions now use inverse-frequency weights that upweight hours with more missing indoor PM2.5 data:

```r
hour_weights <- pm %>%
  group_by(hour) %>%
  summarise(prop_observed = mean(!is.na(pm25_indoor))) %>%
  mutate(weight_hour = 1 / prop_observed)
```

**Action:** Add a paragraph in Methods (Section 4.2 or a new subsection) explaining:
- Indoor PM2.5 data has differential missingness across hours of the day
- To ensure hourly representativeness, all regressions are weighted by the inverse of the proportion of non-missing observations in each hour
- This prevents overrepresentation of hours with higher data availability

---

## 3. Methods Section 4.2: Clustering (Page 13-14, and all figure notes)

**Current text (Figure notes, e.g., page 6):**
> "Errors are clustered at the household level."

**What the code does (throughout all files):**
All regressions cluster on `~respondent_id + date_hour` — this is **two-way clustering** at the household and hour-of-sample level.

**Action:** Update all figure notes and the Methods section to state errors are two-way clustered at the household and date-hour level (not just household level).

---

## 4. Section 2.2 / Figure 2a: Infiltration Robustness Specifications (Pages 4-5)

**Current text (page 5, and Figure 2a description on page 6):**
Describes 4 specifications: (1) hour+week FE, (2) +HH FE, (3) +HH^hour FE, (4) <1km restriction.

**What the code does (`fig2_infiltration.R`, lines 62-166):**
Figure 2a now has **6 specifications**:
1. Main spec with source controls + hour + week FE
2. HH + Hour + Week FE
3. HH-Hour + Week FE
4. <1km to Outdoor Sensor
5. 8-Hour Aggregate
6. 24-Hour Aggregate

The 8-hour and 24-hour aggregates address reviewer concerns about temporal aggregation. The 24-hour aggregate uses `week` FE only (no hour FE since data is daily). The 8-hour aggregate uses `period + week` FE.

**Action:**
- Update the text describing Figure 2a to include all 6 specifications
- In Methods Section 4.2, add the 8-hour and 24-hour aggregate specifications as additional robustness checks (they are already described in the numbered list, but need updating to reflect 6 not 4 specs)
- Update the numbered list on page 13-14 accordingly

---

## 5. Section 2.2 / Figure 2c: Heterogeneity Dimensions (Page 5-6)

**Current text (page 5):**
> "We find statistically significant differences based on air conditioning (AC) ownership (p = 0.08) and whether the monitor room has an open versus closed window or door (p = 0.03)."

Figure 2c description (page 6) lists: "income quartile, number of rooms in the house, air conditioning (AC) presence, whether a window/door was open in the room, and time of day (day/night)."

**What the code does (`fig2_infiltration.R`, lines 184-300):**
- Figure 2c now shows: House Size, AC, Open Window/Door, Day/Night
- **Income infiltration heterogeneity has been moved to Figure 4** (saved separately at line 235 and plotted in `fig4_income.R`)
- All heterogeneity regressions now include the full set of source controls (`rhs_controls_str`)

**Action:**
- Remove "income quartile" from the Figure 2c description
- Note that income heterogeneity in infiltration is now presented in Figure 4
- Update p-values if they changed due to the addition of source controls
- Update the figure caption accordingly

---

## 6. Section 2.3: Hyperlocal Sources — NOW Figure 3 is Completely Restructured (Pages 7-8)

**Current text:**
Section 2.3 discusses only two hyperlocal sources: indoor smoking and neighborhood waste burning. Figure 3 has two panels: (a) probability of PM2.5 spikes by source, and (b) PM2.5 levels by source.

**What the code does (`fig3_source_decomposition.r`):**
The new Figure 3 is a fundamentally different figure with two panels:
- **Left pane (a):** Coefficient plot showing effects of hyperlocal sources on **PM2.5 levels** (not spikes). Includes **6 sources**: Smoked (24 Hours), Waste Burning (2+/week), Waste Burning (1-2/week), Kitchen source, Cooking, Distance to Main Road.
- **Right pane (b):** Mean contribution decomposition — horizontal bar chart showing each source's estimated percentage contribution to mean indoor PM2.5 (including outdoor ambient).

The spike analysis has been **moved to the appendix** (`appendix.R`, lines 540-740).

**Action:**
- Rewrite Section 2.3 to discuss all 6 hyperlocal sources (adding kitchen proximity, cooking, and distance to main road)
- Describe the new Figure 3 structure: coefficient plot + contribution decomposition
- Move the spike-specific discussion to an appendix section or mention it briefly with a reference to the appendix figure
- Update the Figure 3 caption entirely

---

## 7. NEW WRITE-UP NEEDED: Source Decomposition Analysis (fig3_source_decomposition.R)

This is entirely new analysis that needs to be written into the paper. The code does two things:

### 7a. Mean Contribution Decomposition (lines 104-186)

A single regression of indoor PM2.5 on all source variables (with hour + week FE) is estimated. Then for each source variable, the "mean contribution" is computed as:

> contribution_j = β_j × mean(X_j)
> fraction_j = contribution_j / mean(PM2.5_indoor)

This decomposes mean indoor PM2.5 into the fraction attributable to each source. A LaTeX table is produced (`mean_contribution_table.tex`) showing: regression estimate, p-value, mean value of source, mean contribution (%), and R² contribution (LMG).

### 7b. Variance Decomposition via LMG (lines 134-147)

Uses the `relaimpo` package's LMG method (Lindeman-Merenda-Gold) to decompose the R² of the regression into the relative importance of each predictor. This shows which sources explain the most **variance** in indoor PM2.5.

**Action:** Write a new subsection (could be part of Section 2.2 or 2.3) that:
- Explains the mean contribution decomposition methodology
- Reports the key finding: outdoor ambient contributes ~63% of mean indoor PM2.5 (verify exact number from table output)
- Reports contributions from other sources (smoking, waste burning, cooking, kitchen, distance to road)
- Mentions the variance decomposition (LMG) as a complementary analysis
- References the output table
- Note: the current text on page 5 already says "63% of mean indoor PM2.5 comes from ambient outdoor pollution" — but the methodology behind this claim is now different (it uses the full regression with all source controls, not just infiltration rate × mean outdoor)

---

## 8. Section 2.4 / Figure 4: Income Heterogeneity — Completely Restructured (Pages 7-9)

**Current text:**
Figure 4 has three panels: (a) mean PM2.5 by income for indoor and outdoor, (b) household characteristics by income (AC, window/door open, number of rooms), (c) hyperlocal source prevalence by income (spikes, smoking, waste burning).

**What the code does (`fig4_income.R` + `fig3_source_decomposition.r`):**
Figure 4 now has **four panels**:
- **(a) p_char_income:** Household characteristics by income (Pr(Has AC), Pr(Window/Door Open), HH Number of Rooms) — similar to old 4b
- **(b) p_hyperlocal_income:** Hyperlocal source prevalence by income (Pr(Waste Burning), Pr(Smoking HH)) — subset of old 4c, **spikes panel removed**
- **(c) p_income_inf:** Income heterogeneity in infiltration rate — **moved from Figure 2c**
- **(d) p_decomp_income:** Stacked bar chart decomposing indoor PM2.5 by income quartile into Outdoor Ambient, Smoking Household, and Other (Hyperlocal)

**The old Figure 4a** (mean PM2.5 levels by income quartile) **appears to be removed** from the main figure. The robustness aggregation methods (old Appendix Figure A2) are still in the appendix.

**Action:**
- Rewrite Section 2.4 to match the new 4-panel structure
- The old Figure 4a content (mean PM2.5 by income showing 23.4 µg/m³ / 79% increase) may need to be stated as a text result rather than a figure panel, or you need to add it back
- Describe the new income decomposition approach (panel d): uses interaction terms (`i(income_quart, pm25_outdoor3)` etc.) rather than split-sample estimation
- Describe the infiltration rate by income (panel c) that was moved from Figure 2
- Update the Figure 4 caption entirely

---

## 9. Section 2.4 / Income Decomposition Methodology Change (fig3_source_decomposition.r, lines 276-377)

**Current text (page 6, Figure 2b description):**
> "The ambient outdoor contribution is calculated by multiplying infiltration rates (from the main regression specification in panel a) by the mean outdoor PM2.5."

**What the code does:**
The income-specific decomposition now uses a **single interaction regression** rather than split-sample:

```r
reg_decomp_income <- feols(
  pm25_indoor ~
    income_quart +
    i(income_quart, pm25_outdoor3) +
    i(as.factor(trash_burning_1week_baseline), income_quart, ref = "Never") +
    i(as.factor(smoke24_endline), income_quart, ref = 0) +
    i(as.factor(room_pmsource_kitchen), income_quart, ref = 0) +
    i(cooking, income_quart, ref = 0) +
    i(income_quart, dist_primary) +
    temp_outdoor3 + humidity_outdoor3 |
    hour + week,
  ...
)
```

Then income-quartile-specific contributions are computed as β_outdoor(iq) × mean_outdoor(iq) and β_smoke(iq) × mean_smoke(iq), with the remainder labeled "Other (Hyperlocal)."

**Action:** Update Methods to describe this interaction approach. Explain that temp/humidity are NOT interacted with income (they are controls, not sources of interest).

---

## 10. Figure 2b: Source Decomposition Bars (Page 6)

**Current text:**
Figure 2b shows decomposition into ambient outdoor and hyperlocal by income quartile.

**What the code does:**
Figure 2b's description references the main regression specification — but the underlying regression now includes all source controls. The decomposition approach is the same conceptually but the infiltration coefficient (0.689) may have changed slightly since source controls are now included.

**Action:** Verify the infiltration coefficient is still 0.689 (or update if it changed). Update Figure 2b description if the methodology for computing the decomposition changed.

---

## 11. Abstract and Key Statistics: Verify Numbers Still Hold

Several key statistics are stated in the abstract and text. With the new source controls and weighting, these may have changed:

- Infiltration rate: 0.689 (95% CI: 0.508, 0.871) — **verify with new reg1 output**
- "63% of mean indoor PM2.5 comes from ambient outdoor pollution" — **verify with new decomposition**
- "2.5 times the rate typically estimated in the United States" — **verify**
- Smoking effect: 5.51% greater probability of spikes, 20.8 µg/m³ higher PM2.5 — **these are from the old Figure 3 which is now restructured; verify new coefficients**
- Income gap: 23.4 µg/m³ / 79% increase from highest to lowest quartile — **verify**
- p-values for AC (0.08), window/door (0.03), income spikes (0.027), smoking by income (0.001) — **all may have changed**

**Action:** Re-run the code and update all specific numbers in the text to match the new regression outputs.

---

## 12. Section 2.3: Smoking Effect Statistics Need Updating

**Current text (page 7):**
> "These households had a 5.51% (95% CI: 1.85–9.17, P < 0.001) greater probability of pollution spikes reaching very unhealthy levels."
> "smoking was associated with 20.8 µg/m³ (95% CI: 7.36–34.3, P < 0.001) higher average hourly indoor PM2.5 concentrations"

**What the code does:**
The old Figure 3 regressions (spike outcome and PM2.5 outcome separately by source) have been replaced with a single regression including all sources simultaneously. The coefficients will be different because they now control for the other sources.

**Action:** Update all specific coefficient values, CIs, and p-values for smoking (and all other sources) from the new regression output.

---

## 13. Discussion Section (Page 10): Update to Reflect New Analyses

**Current text discusses:**
- Infiltration factor of 0.689
- GBD framework limitations
- Smoking and waste burning as hyperlocal sources

**Action:** Update Discussion to also mention:
- The new source variables (cooking, kitchen proximity, distance to main road)
- The mean contribution decomposition results
- The variance decomposition (LMG) findings
- The interaction-based income decomposition approach

---

## Summary Checklist

| # | Section | Change Needed |
|---|---------|--------------|
| 1 | Methods 4.2 (Eq. 1) | Add source controls to infiltration equation |
| 2 | Methods (new) | Describe inverse-frequency weighting |
| 3 | Methods + all figure notes | Two-way clustering, not just household |
| 4 | Section 2.2 + Methods 4.2 | 6 infiltration specs (add 8hr, 24hr aggregates) |
| 5 | Section 2.2 + Fig 2c caption | Remove income from Fig 2c, moved to Fig 4 |
| 6 | Section 2.3 + Fig 3 caption | Complete rewrite: new sources + decomposition |
| 7 | Section 2.2 or 2.3 (NEW) | Write up mean contribution & LMG decomposition |
| 8 | Section 2.4 + Fig 4 caption | 4-panel restructure, new decomposition panel |
| 9 | Methods (new) | Describe interaction-based income decomposition |
| 10 | Fig 2b caption | Verify decomposition methodology description |
| 11 | Throughout | Verify all specific numbers (coefficients, CIs, p-values) |
| 12 | Section 2.3 | Update smoking/source effect statistics |
| 13 | Discussion | Mention new sources and decomposition analyses |
