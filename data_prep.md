Goal: create a reproducible data appendix for a PNAS paper R&R submission. 

Problem:

* I have cleaned and analyzed a bunch of different datasets in order to write a paper. The main contents of the paper is in the main folder here, called "manuscript.pdf". The supplementary index is called "supplementary_info.pdf"
* The code for the analysis that is done to produce the tables and figuers in this paper is produced in `jakarta_pm_public/code`
* However, the data cleaning is done over three different workstreams. I want you to extract the relevant data cleaning steps, and put it into one folder. At the end of this process, there should be a `code/cleaning` folder, and then a separate `code/analysis` folder. 

First rule, DO NOT DELETE ANYTHING. Always copy; not delete.

I **think** the best way to do this is:

* Look through the analysis. Identify the list of variables that I actually use to create the paper. 
* Then go through the two folders where I keep the code used to create the data. These two folders are:
  * "/Users/yixin.sun/Documents/Educational/jakarta_pm"
  * "/Users/yixin.sun/Documents/Educational/pollution_experience"
* The raw data for creating the analysis datasets are in "/Users/yixin.sun/Documents/Educational/pollution_experience_data". The jakarta_pm and pollution_experience code files use the raw data from this folder, although you will need to update the exact file paths. 
* Once you've understood how each variable is created, I want you to start the "/Users/yixin.sun/Documents/Educational/jakarta_pm_public/code/cleaning" folder, where you will create new cleaning files that create the datasets that I eventually use in my analysis. I want a separate dataset for each of these:
  * pollution data, indoor and outdoor, at the household-hour level
  * survey data
  * Whatsapp survey data
  * and then whatever else is necessary to run all the analyses. 
* Also start a folder in `/Users/yixin.sun/Documents/Educational/jakarta_pm_public/data` called "raw_data". Copy over the necessary raw files from `/Users/yixin.sun/Documents/Educational/pollution_experience_data`
  * When you copy over indoor pollution and survey data, make sure to only copy over households that are in the control group. These are identified by `treatment_status %in% c("Fan", "Control")`
  * When you copy over raw data, make sure the longitude/latitude is jiggered so that it's not identifiable, but still within roughly 1km of where the household actually was. 
  * Don't copy over any identifiable information in the raw data; aka no names, emails, etc. 
* At the end, run the analysis, put all the newly created figures and tables into a new latex document in `/Users/yixin.sun/Documents/Educational/jakarta_pm_public/output` and output a pdf from it.
  * Add comments to this latex summary file at the end that compares the newly outputted figures and tables to the ones in `manuscript.pdf` and `supplementary_info.pdf`. 
* Write code in the style that I write code please. 