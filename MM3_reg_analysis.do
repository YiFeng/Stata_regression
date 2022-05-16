*********MM3 data analysis**********
*********coded by Yi Feng***********
*********05/13/2020*****************


******************************** data preprocessing ****************************
******************************** data preprocessing ****************************
******************************** data preprocessing ****************************
*input final dataset
clear
insheet using "/Users/yifeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/MM3_finaldataset.csv", comma case names 

*drop empty cases (.)
drop if id==.
*notice nl2 not a numerical type var, so destring it
destring nl2, replace
*generate a num var for condition
generate train_con=.
replace train_con=0 if condition=="Eagle Eye"
replace train_con=1 if condition=="Control + Math"
replace train_con=2 if condition=="Control + WM"
replace train_con=3 if condition=="WM + Math"

*standardized for all assessments
foreach test in add_overall d2c_ cs_recall_acc fi_corr tb_acc nl{
foreach i in 1 2 3{
egen z`test'`i' = std(`test'`i')
}
}

*calculate composite score for WM and Math assessments
foreach i in 1 2 3{
egen wm_composite`i' = rowmean(zcs_recall_acc`i' zfi_corr`i' ztb_acc`i')
egen math_composite`i' = rowmean(zadd_overall`i' zd2c_`i' znl`i')
}

*make a composite score for math & wm in pretest as general daseline ability
gen general_baseline = (wm_composite1 + math_composite1)/2
pwcorr wm_composite1 math_composite1, print(.05) star(.05)

*generate dummy var for control+wm, control+math, wm+math
generate control_wm=0
replace control_wm=1 if condition=="Control + WM"
generate control_math=0
replace control_math=1 if condition=="Control + Math"
generate wm_math=0
replace wm_math=1 if condition=="WM + Math"
generate eagle=0
replace eagle=1 if condition=="Eagle Eye"
generate trained=0
replace trained=1 if condition!="Eagle Eye"

*generate a score represent improvement
gen wm_short = wm_composite2 - wm_composite1
gen wm_long = wm_composite3 - wm_composite1
gen math_short = math_composite2 - math_composite1
gen math_long = math_composite3 - math_composite1

*standardized composite score
egen z_general_baseline = std(general_baseline)
foreach outcome in wm_composite2 wm_composite3 math_composite2 math_composite3{
egen z`outcome' = std(`outcome')
}
*global vars and make interaction terms
global group "control_wm control_math wm_math trained"
foreach var in $group{
gen wm_composite1X`var' = wm_composite1 * `var'
}
foreach var in $group{
gen math_composite1X`var' = math_composite1 * `var'
}
foreach var in $group{
gen baselineX`var' = general_baseline * `var'
}
foreach var in $group{
gen zbaselineX`var' = z_general_baseline * `var'
}
global zbaselineXgroup "zbaselineXcontrol_wm - zbaselineXwm_math"
global baselineXgroup "baselineXcontrol_wm - baselineXwm_math"
global wm_groupXpre "wm_composite1Xcontrol_wm - wm_composite1Xwm_math"
global math_groupXpre "math_composite1Xcontrol_wm - math_composite1Xwm_math"
global interaction "wm_composite1Xtrained math_composite1Xtrained"



******************************** data preprocessing ****************************
******************************** data preprocessing ****************************
******************************** data preprocessing ****************************
* descriptive stats
global tests "nl3 add_overall3 d2c_3 tb_acc3 cs_recall_acc3 fi_corr3 wm_composite3 math_composite3"
est clear // clear estimate 'STATA cloud-storage'
estpost summarize $tests if control_wm==1, detail
est store m1
estpost summarize $tests if control_math==1, detail
est store m2
estpost summarize $tests if wm_math==1, detail
est store m3
estpost summarize $tests if eagle==1, detail
est store m4

*esttab to transfer table from 'STATA cloud-storage' to excel 
cd "/Users/yifeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m1 m2 m3 m4 using followup_bygroup.csv, ///
	cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") /// 
	title("Descriptive statistics of all test scores") ///
	mtitle("control_wm" "control_math" "wm_math" "control") /// subtitle
	replace label nogaps plain

* descriptive stats for matched group
global tests "nl1 add_overall1 d2c_1 tb_acc1 cs_recall_acc1 fi_corr1 wm_composite1 math_composite1"
est clear // clear estimate 'STATA cloud-storage'
estpost summarize $tests if control_wm==1 & match_sample5==1, detail
est store m1
estpost summarize $tests if control_math==1 & match_sample5==1, detail
est store m2
estpost summarize $tests if wm_math==1 & match_sample5==1, detail
est store m3
estpost summarize $tests if eagle==1 & match_sample5==1, detail
est store m4

*esttab to transfer table from 'STATA cloud-storage' to excel 
cd "/Users/yifeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m1 m2 m3 m4 using match_pre_bygroup.csv, ///
	cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") /// 
	title("Descriptive statistics of all test scores") ///
	mtitle("control_wm" "control_math" "wm_math" "control") /// subtitle
	replace label nogaps plain

* paired ttest for all assessments 

foreach var in nl add_overall d2c_ tb_acc cs_recall_acc fi_corr wm_composite math_composite{
ttest `var'3 == `var'2 if eagle==1 & match_sample5==1
gen delta_`var' = `var'3 - `var'2 if eagle==1 & match_sample5==1
summ delta_`var'
display `r(mean)'/`r(sd)'
}


***************************FULL SAMPLE Regression ******************************
***************************FULL SAMPLE Regression ******************************
***************************FULL SAMPLE Regression ******************************

************************** 0. Not include pretest *********************
*regeression
est clear
reg zwm_composite2 control_wm control_math wm_math
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zwm_composite3 control_wm control_math wm_math
est store m2 //wm_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zmath_composite2 control_wm control_math wm_math
est store m3 //math_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zmath_composite3 control_wm control_math wm_math 
est store m4 //math_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

cd "/Users/yifeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_result_model0.csv, ///
	title("Regression: training group predict post and followup") ///
	mtitle("WM_post" "WM_followup" "Math_post" "Math_followup") ///
	replace label nogaps beta(2) se(2) r2 // b means coefficient, se means standard error, r means r-square

************************** 1. Using imrpvement score as DV *********************

*using improvement as dependent var to run regression
est clear
reg wm_short control_wm control_math wm_math
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg wm_long control_wm control_math wm_math
est store m2 //wm_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg math_short control_wm control_math wm_math
est store m3 //math_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg math_long control_wm control_math wm_math
est store m4 //math_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_result_improvement.csv, ///
	title("Regression: training group differences in short and long term improvement") ///
	mtitle("WM short-term" "WM long-term" "Math short-term" "Math long-term") ///
	replace label nogaps b(2) se(2) r2 // b means coefficient, se means standard error, r means r-square
	
*adding pretest as baseline ability
est clear
reg wm_short wm_composite1 control_wm control_math wm_math $wm_groupXpre, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test wm_composite1Xcontrol_wm = wm_composite1Xcontrol_math = wm_composite1Xwm_math = 0

reg wm_long wm_composite1 control_wm control_math wm_math $wm_groupXpre, beta
est store m2 //wm_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test wm_composite1Xcontrol_wm = wm_composite1Xcontrol_math = wm_composite1Xwm_math = 0

reg math_short math_composite1 control_wm control_math wm_math $math_groupXpre, beta
est store m3 //math_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test math_composite1Xcontrol_wm = math_composite1Xcontrol_math = math_composite1Xwm_math = 0

reg math_long math_composite1 control_wm control_math wm_math $math_groupXpre, beta
est store m4 //math_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test math_composite1Xcontrol_wm = math_composite1Xcontrol_math = math_composite1Xwm_math = 0
	
************************** 2. Using post & follow up as DV *********************

************divided into 2 groups (training or non-training)
*plot
profileplot wm_composite1 wm_composite2 wm_composite3, by(trained)
profileplot math_composite1 math_composite2 math_composite3, by(trained)

est clear
reg wm_composite2 wm_composite1 math_composite1 trained $interaction
est store m1 
reg math_composite2 wm_composite1 math_composite1 trained $interaction
est store m2
reg wm_composite3 wm_composite1 math_composite1 trained $interaction
est store m3
reg math_composite3 wm_composite1 math_composite1 trained $interaction
est store m4

cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_result_by2group.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("WM_post" "Math_post" "WM_followup" "Math_followup") ///
	replace label nogaps b(2) se(2) r2 // b means coefficient, se means standard error, r means r-square

***********divided into 4 groups (WM math both control)
*plot
profileplot wm_composite1 wm_composite2 wm_composite3, by(train_con)
profileplot math_composite1 math_composite2 math_composite3, by(train_con)

*regeression
est clear
reg wm_composite2 wm_composite1 control_wm control_math wm_math $wm_groupXpre, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test wm_composite1Xcontrol_wm = wm_composite1Xcontrol_math = wm_composite1Xwm_math = 0

reg wm_composite3 wm_composite1 control_wm control_math wm_math $wm_groupXpre, beta
est store m2 //wm_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test wm_composite1Xcontrol_wm = wm_composite1Xcontrol_math = wm_composite1Xwm_math = 0

reg math_composite2 math_composite1 control_wm control_math wm_math $math_groupXpre, beta
est store m3 //math_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test math_composite1Xcontrol_wm = math_composite1Xcontrol_math = math_composite1Xwm_math = 0


reg math_composite3 math_composite1 control_wm control_math wm_math $math_groupXpre, beta
est store m4 //math_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test math_composite1Xcontrol_wm = math_composite1Xcontrol_math = math_composite1Xwm_math = 0

*interaction graph
twoway (scatter math_composite3 math_composite1 if wm_math==1, mcolor(sanb) msymbol(circle_hollow)) ///
	(scatter math_composite3 math_composite1 if eagle==1, mcolor(dkorange)) ///
	(lfit math_composite3 math_composite1 if wm_math==1, lcolor(sanb) lpattern(dash)) ///
	(lfit math_composite3 math_composite1 if eagle==1, lcolor(dkorange) lpattern(solid)), ///
	legend(label(1 "WM+Math") label(2 "Eagle eye") order (1 2))


cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_model1_pretest_beta.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("WM_post" "WM_followup" "Math_post" "Math_followup") ///
	replace label nogaps beta(2) se(2) r2 // b means coefficient, se means standard error, r means r-square
	
************************** 3. Using general ability as control *****************
reg zwm_composite2 control_wm control_math wm_math
reg zmath_composite2 control_wm control_math wm_math
reg zwm_composite3 control_wm control_math wm_math
reg zmath_composite3 control_wm control_math wm_math
*regeression
est clear
reg zwm_composite2 z_general_baseline control_wm control_math wm_math $zbaselineXgroup
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test zbaselineXcontrol_wm = zbaselineXcontrol_math = zbaselineXwm_math = 0

reg zwm_composite3 z_general_baseline control_wm control_math wm_math $zbaselineXgroup
est store m2 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test zbaselineXcontrol_wm = zbaselineXcontrol_math = zbaselineXwm_math = 0

reg zmath_composite2 z_general_baseline control_wm control_math wm_math $zbaselineXgroup
est store m3 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math
test zbaselineXcontrol_wm = zbaselineXcontrol_math = zbaselineXwm_math = 0

reg zmath_composite3 z_general_baseline control_wm control_math wm_math $zbaselineXgroup
est store m4 //wm_post



cd "/Users/yifeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_model2_generalbaseline.csv, ///
	title("Regression: training group and general ability predict post and followup") ///
	mtitle("WM_post" "WM_followup" "Math_post" "Math_followup") ///
	replace label nogaps beta(2) se(2) r2 // b means coefficient, se means standard error, r means r-square
	
	
************************ matched group sample***********************************
************************ matched group sample***********************************
************************ matched group sample***********************************

* Matching in stata 
ssc install ccmatch
ccmatch wm_composite1, id(id) cc(trained) //only can match between 2 gourps

* Using other matching methods and selected sample was marked as 1

* Match sample1: within +-0.5sd for all groups
* Match sample2: select 30 subjects that closed to 0 in each group
* Match sample3: wm and math seperately; match chain(1 control+wm with control - 2 control+math with control - 3 wm+math with control), use SPSS, tolarence:0.05
* Match sample4: using general ability; match chain(1 control+math with control - 2 control+math with control - 3 wm+math with control), use SPSS, tolarence:0.1
* Match sample5: using general ability; match chain(1 control+math with control - 2 control+wm with control - 3 wm+math with control), use SPSS, tolarence:0.05
* Match sample6: wm and math seperately; match chain(1 control+math with control - 2 control+wm with control - 3 wm+math with control), use SPSS, tolarence:0.1

est clear
reg zwm_composite2 control_wm control_math wm_math if match_sample5==1
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zwm_composite3 control_wm control_math wm_math if match_sample5==1
est store m2 //wm_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zmath_composite2 control_wm control_math wm_math if match_sample5==1
est store m3 //math_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


reg zmath_composite3 control_wm control_math wm_math if match_sample5==1
est store m4 //math_followup
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_result_model3_match5_beta.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("WM_post" "WM_followup" "Math_post" "Math_followup") ///
	replace label nogaps beta(2) se(2) r2 // b means coefficient, se means standard error, r means r-square
	
************************ matched group sample***********************************
************************ matched group sample***********************************
************************ matched group sample***********************************	
	
************************ subgroup based on baseline***********************************
************************ subgroup based on baseline***********************************
************************ subgroup based on baseline***********************************
*compute median
sum math_composite1, d
sum wm_composite1, d

*generate group variable based on baseline
generate high_wm_base=0
replace high_wm_base=1 if wm_composite1 >0.0079745
generate high_math_base=0
replace high_math_base=1 if math_composite1 > -0.1569482


* seperately run regression for 4 different group
est clear
reg wm_composite2 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite2 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite2 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite2 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

*wm follow up
reg wm_composite3 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite3 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite3 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg wm_composite3 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math


*math performance
reg math_composite2 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite2 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite2 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite2 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

*math followup
reg math_composite3 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite3 general_baseline control_wm control_math wm_math if high_wm_base==1 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite3 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==1, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

reg math_composite3 general_baseline control_wm control_math wm_math if high_wm_base==0 & high_math_base==0, beta
est store m1 //wm_post
test control_wm = control_math
test control_math = wm_math
test control_wm = wm_math

cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_result_model3_match5_beta.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("WM_post" "WM_followup" "Math_post" "Math_followup") ///
	replace label nogaps beta(2) se(2) r2 // b means coefficient, se means standard error, r means r-square
	
************************ subgroup based on baseline***********************************
************************ subgroup based on baseline***********************************
************************ subgroup based on baseline***********************************
	
	
***************** try all the assessments tests ********************************
***************** try all the assessments tests ********************************
***************** try all the assessments tests ********************************

global group "control_wm control_math wm_math trained"
foreach test in zadd_overall1 zd2c_1 zcs_recall_acc1 zfi_corr1 ztb_acc1 znl1{
foreach var in $group{
gen `test'X`var' = `test' * `var'
}
}

*math
est clear
reg znl2 znl1 control_wm control_math wm_math znl1Xcontrol_wm znl1Xcontrol_math znl1Xwm_math
est store m1 //Numberline post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg znl3 znl1 control_wm control_math wm_math znl1Xcontrol_wm znl1Xcontrol_math znl1Xwm_math
est store m2 //Numberline followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zd2c_2 zd2c_1 control_wm control_math wm_math zd2c_1Xcontrol_wm zd2c_1Xcontrol_math zd2c_1Xwm_math
est store m3 //D2C post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zd2c_3 zd2c_1 control_wm control_math wm_math zd2c_1Xcontrol_wm zd2c_1Xcontrol_math zd2c_1Xwm_math
est store m4 //D2C followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math	

reg zadd_overall2 zadd_overall1 control_wm control_math wm_math zadd_overall1Xcontrol_wm zadd_overall1Xcontrol_math zadd_overall1Xwm_math
est store m5 //Addition post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zadd_overall3 zadd_overall1 control_wm control_math wm_math zadd_overall1Xcontrol_wm zadd_overall1Xcontrol_math zadd_overall1Xwm_math
est store m6 //Addition followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math


cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_math_result.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("Numberline post" "Numberline followup" "D2C post" "D2C followup" "Addition post" "Addition followup") ///
	replace label nogaps b(2) se(2) r2 // b means coefficient, se means standard error, r means r-square

*interaction graph
twoway (scatter zadd_overall3 zadd_overall1 if wm_math==1, mcolor(sanb) msymbol(circle_hollow)) ///
	(scatter zadd_overall3 zadd_overall1 if eagle==1, mcolor(dkorange)) ///
	(lfit zadd_overall3 zadd_overall1 if wm_math==1, lcolor(sanb) lpattern(dash)) ///
	(lfit zadd_overall3 zadd_overall1 if eagle==1, lcolor(dkorange) lpattern(solid)), ///
	legend(label(1 "WM+Math") label(2 "Eagle eye") order (1 2))

*wm
est clear
reg ztb_acc2 ztb_acc1 control_wm control_math wm_math ztb_acc1Xcontrol_wm ztb_acc1Xcontrol_math ztb_acc1Xwm_math
est store m1 //Touchbase post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg ztb_acc3 ztb_acc1 control_wm control_math wm_math ztb_acc1Xcontrol_wm ztb_acc1Xcontrol_math ztb_acc1Xwm_math
est store m2 //Touchbase followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zfi_corr2 zfi_corr1 control_wm control_math wm_math zfi_corr1Xcontrol_wm zfi_corr1Xcontrol_math zfi_corr1Xwm_math
est store m3 //Following instruction post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zfi_corr3 zfi_corr1 control_wm control_math wm_math zfi_corr1Xcontrol_wm zfi_corr1Xcontrol_math zfi_corr1Xwm_math
est store m4 // Following instruction followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math	

reg zcs_recall_acc2 zcs_recall_acc1 control_wm control_math wm_math zcs_recall_acc1Xcontrol_wm zcs_recall_acc1Xcontrol_math zcs_recall_acc1Xwm_math
est store m5 //Coutingsheep post
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math

reg zcs_recall_acc3 zcs_recall_acc1 control_wm control_math wm_math zcs_recall_acc1Xcontrol_wm zcs_recall_acc1Xcontrol_math zcs_recall_acc1Xwm_math
est store m6 // Coutingsheep followup
lincom control_wm-control_math
lincom control_math-wm_math
lincom control_wm-wm_math


cd "/Users/yfeng/GoogleDrive/Yi_UCI_research/GSR other works/MM3/MM3_outcome_statistics/Analysis/Result"
esttab m* using MM3_reg_wm_result.csv, ///
	title("Regression: training group and pretest predict post and followup") ///
	mtitle("Touchbase post" "Touchbase followup" "Following instruction post" "Following instruction followup" "Coutingsheep post" "Coutingsheep followup") ///
	replace label nogaps b(2) se(2) r2 // b means coefficient, se means standard error, r means r-square

twoway (scatter ztb_acc2 ztb_acc1 if wm_math==1, mcolor(sanb) msymbol(circle_hollow)) ///
	(scatter ztb_acc2 ztb_acc1 if eagle==1, mcolor(dkorange)) ///
	(lfit ztb_acc2 ztb_acc1 if wm_math==1, lcolor(sanb) lpattern(dash)) ///
	(lfit ztb_acc2 ztb_acc1 if eagle==1, lcolor(dkorange) lpattern(solid)), ///
	legend(label(1 "WM+Math") label(2 "Eagle eye") order (1 2))

***************** try all the assessments tests ********************************
***************** try all the assessments tests ********************************
***************** try all the assessments tests ********************************

save MM3_reg_data.dta, replace




