ods rtf file='C:\desktop\Group_Project.rtf' ;

data NCHS;
	infile 'C:\desktop\NCHS_-_Age-adjusted_Death_Rates_for_the_Top_10_Leading_Causes_of_Death__United_States__2013.csv' dsd missover firstobs = 2;
	input Year Cause: $60. Cause_name: $30. State: $20. Deaths AADR: 5.;
	if cmiss(of _all_) then delete;
	if Year < 2005 then delete;
	label AADR = 'Age Adjusted Death Rate';
run;
data Risk_Factors; 
	infile 'C:\desktop\Behavioral_Risk_Factors_-_Vision___Eye_Health.csv' dsd missover firstobs = 2; 
	input Year LocationAbbr $ State: $20. Topic: $40. Question: $100. DataSource $ Response $ Data_Value_Unit $
    Data_Value_Type: $30. Data_Value Data_Value_Footnote_Symbol $ Data_Value_Footnote $
	Low_Confidence_limit High_Confidence_Limit Sample_Size Break_Out$ Break_Out_Category$ GeoLocation
	TopicId $ QuestionId$ LocationId BreakOutId $  BreakOutCategoryId $ ResponseId $;
	drop LocationAbbr DataSource Response Data_Value_Unit Data_Value_Footnote_Symbol Data_Value_Footnote Break_Out 
	Break_Out_Category GeoLocation TopicId QuestionId BreakOutId BreakOutCategoryId ResponseId Data_Value;
	if Year > 2013 then delete;
	if cmiss(Sample_Size) then delete; 
	if Sample_Size < 500 then delete;
	Rate = Data_Value/100;
	format Rate percent6.1;
	label Data_Value_Type='Type of Data Value' Data_Value='Data Value' Low_Confidence_limit="Low CI Limit"
	High_Confidence_Limit="High CI Limit" Sample_Size="Sample Size" LocationId='Location ID'; 
run;

proc contents data = NCHS;
proc contents data = Risk_Factors;
run;

proc sort data = NCHS out = p1;
	by year State;
run;
proc sort data = Risk_Factors out = p2;
	by year State;
run;

data Combined;
	merge p1 p2;
	by year state;
	if cmiss(of _all_) then delete;
run;

proc contents data = Combined;
run;

proc univariate data = Combined;
	var AADR Rate Sample_Size Deaths;
	ods select Extremeobs;
run;

data Combined_Clean;
	set Combined;
	if AADR > 100 then delete;
run;

proc means data = Combined_Clean;
 var AADR Rate Sample_Size Deaths;
run;

data sample_number;
	set Combined_Clean;
	Acutal = int(Sample_size * Rate); 
	label Acutal ='Number of Patients';
run;

proc sort data = combined_clean out = sort_question;
	by question;
run;

data average_rate (keep= question rate);
	set sort_question;
	by question;
	if first.question then do
	total_rate = 0;
	number = 0;
	end;
	number + 1;
	total_rate + rate;
	if last.question;
	avg_rate = total_rate/number;
run;

proc print data = average_rate;
run;
	 
ods rtf close;
