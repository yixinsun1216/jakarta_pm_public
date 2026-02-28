Responding to reviewers:

I submitted the attached paper to PNAS, and received reviewer comments. I am now responding to these reviewer comments (also attached, where comments are in black). I have started to make changes to respond to these comments. I need help with my response to the reviewers. Specifically, I have updated my code to respond to:

* Reviewer 1 (R1) number (4) "The work implements a regression model for infiltration estimates....". I added code to appendix.R that aggregates at the 8 hour and 24 hour time level. It's in the section titled: "\# Infiltration at different levels of aggregation". I want you to move the 24 hour, week FE specification into the main figure 2. So move the code to figure2_infiltration.R for JUST THAT MODEL. Add this to the p_inf graph. Keep the code for appendix.R as is though - don't delete anything there. I want to keep that appendix graph the same. 
* Staying on the topc of figure2_infiltration.R, i want to change the infiltration regression specification to include  indoor source variables. In this way, I want it to match the code from fig3_source_decomposition. So the main specification for the infiltration rate should include these variables:  `rhs_fml <- ~ as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) + as.factor(room_pmsource_kitchen) + pm25_outdoor3 + cooking +  dist_primary`
  * I've added `rhs_fml <- ~ as.factor(trash_burning_1week_baseline) + as.factor(smoke24_endline) + as.factor(room_pmsource_kitchen) + pm25_outdoor3 + cooking +  dist_primary` to the "run_code.R" file so that it is a global variable for all the code. 
  * So fig2_infiltration.r's p_inf plot should have the following models:
    * pm_indoor ~ rhs_fml | hour + week
    * HH + Hour + Week FE (same as before)
    * HH-Hour + Week FE (same as before)
    * <1km to outdoor sensor (same as before)
    * 24 hour aggregate (from the previous bulletpoint) but regression rhs_fml on the right hand side. Fixed effects are hour + week
  * for p_het and p_behaviors, also add the other variables from rhs_fml that aren't already included. p_het and p_behaviors shoudl just be interacting the respective variable of itnerest with pm25_outdoor3
* In p_het from fig2_infiltration.R, move the infiltration rate for income to figure 4. More to come about this in a later bullet
* For figure 3, I changed the code a lot, but now i want to create a graph that's a combination of the old graph from the manuscript I submitted, to the one that I have now, keeping just stuff related to PM2.5 not to spikes. I want a two pane graph, where the left hand pane is the old figure 3 right hand pane, that's the Figure 3: Contributions of Hyperlocal Pollution, where the x axis is the PM2.5. I want to add in the additional hyperlocal activities: kitchen source, cooking, distance to main road. 
  * OK so that's the left hand pane. The right hand pane is what is currently in the fig3_source_decomposition code, "1) Mean Contribution Decomposition", which produces the plot p_contributions
  * Keep the variance decomposition LMG for pm2.5 in this code file
  * I want to make an analogous two pane graph but with spikes as the outcome. I have some code for this at line 174 - move this to the appendix. Also move the variance decomposition LMG for spikes code to the appendix. 
* For figure 4, let's reconfigure the plots. 
  * Pane a should be p_char_income, pane b should be p_hyperlocal_income, pane c let's get the income heterogeneity infiltration rate from fig2_infiltration. Pane d is p_decomp.
    * Some of the code that creates p_decomp is in fig3_source_decomposition; in this code, don't use split sample to estimate heterogeneity by income. Instead, interact income_quart by all the relevant variables. Make sure that the reference group is right, and write the interaction in feols as something like `i(as.factor(smoke24_endline), income_quart, ref = 0)`. Don't include income_quart by itself in the regression. The reference groups should be:
      * as.factor(trash_burning_1week_baseline) = 0
      * as.factor(smoke24_endline) = 0 
      * as.factor(room_pmsource_kitchen) = 0
      * cooking = 0
    * Don't interact income with temp_outdoor3 or humidity_outdoor3. 



A general statement, there's a lot of missing data for pm25_indoor. Some hours have more missing data than others. Can you run all regression, upweighting hours where there's more missing data in those hours? 



When you have finished it all, please do 2 things:

* Put all figures produced in ANY code file into one latex document. Compile as PDF and show me. 

* Create another agent to act as an aggressive reviewer of the code, and flag anything that could look dodgy for me to review. 

  