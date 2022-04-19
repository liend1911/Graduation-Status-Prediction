libname Sta '/home/u49804996/Sta';

proc format;
	value $College
	"Col Nurse & Health Innovation" = 1
	"College of Business" = 2
    "College of Engineering" = 3
    "College of Liberal Arts" = 4
    "College of Science" = 5
    "Col Arch, Plan & Pub Affairs" = 6
    "College of Education" = 6
    "Division of Student Success" = 6
    "Honors College" = 6
    "School of Social Work" = 6;
run;

/*subset of undergrad only*/
data Sta.undergrad (keep= AcademicProgramDescription CapFlag TransferStudentFlag 
                             Gender CollegeDescriptionTerm1 TermGpaTerm1 
                             TotalCompletedSchTerm1 DepartmentalGpaTerm1 CollegeT1 status);
set Sta.grd;
if AcademicProgramDescription='Masters' then delete;
CollegeT1 = put(CollegeDescriptionTerm1, $College.);
if CollegeT1 = 5 then delete;
if CollegeT1 = 6 then delete;
if CapFlag = "Y" AND TransferStudentFlag = "Y" then delete;
if CapFlag = "N" AND TransferStudentFlag = "N" then status = 1;
if CapFlag = "Y" AND TransferStudentFlag = "N" then status = 2;
if CapFlag = "N" AND TransferStudentFlag = "Y" then status = 3;
run;

/*Histogram*/
proc sort data=Sta.undergrad;
	by status;
run;

/*basically the same across different status*/
proc univariate data=Sta.undergrad noprint; 
	class status; 
 	histogram TermGPATerm1 / midpoints=(0.000 to 4.000 by 0.500) odstitle=title 
 							odstitle2="Histogram of TermGPATerm1 by status";
 	histogram TotalCompletedSchTerm1 / midpoints=(0.000 to 112.000 by 10.000) odstitle=title 
 							odstitle2="Histogram of TotalCompletedSchTerm1 by status";
 	histogram DepartmentalGpaTerm1 / midpoints=(0.000 to 4.000 by 0.500) odstitle=title 
 							odstitle2="Histogram of DepartmentalGpaTerm1 by status";
run;

/*Descriptive statistics*/
proc sort data = Sta.undergrad; 
 	by CollegeT1 status; 
run;

proc freq data=Sta.undergrad;
 tables CollegeT1*status / nocol nopct
 plots=freqplot(twoway=stacked orient=horizontal);
 title ‘Summary of colleges by status (undergraduate)’;
run;

/*Summary Statistics*/
proc tabulate data=Sta.undergrad;
class status CollegeT1;
var TermGPATerm1 TotalCompletedSchTerm1 DepartmentalGpaTerm1;
tables (status*TermGPATerm1)*(MEAN STD N), CollegeT1;
tables (status*TotalCompletedSchTerm1)*(MEAN STD N), CollegeT1;
tables (status*DepartmentalGpaTerm1)*(MEAN STD N), CollegeT1;
title 'Summary statistics by colleges';
run;

/*block = a matched set of observations across the treatments*/
/*block = status*/
/*treatment = colleges*/

/*TermGPATerm1*/
ods graphics on;
proc glm data=Sta.undergrad PLOTS(MAXPOINTS=NONE) PLOTS(ONLY)=INTPLOT; 
 	class CollegeT1 status; 
 	model TermGPATerm1 = CollegeT1 status CollegeT1*status / ss3; 
 	lsmeans CollegeT1 / pdiff stderr; 
 	lsmeans CollegeT1*status / pdiff stderr;
 	label  TermGPATerm1 = 'Term GPA (1)' 
 		   CollegeT1    = 'Colleges' 
           status       = 'Status'; 
    title1 'Two-Way ANOVA'; 
 	title2 'TermGPATerm1';
 	/*t option: pairwise t-tests for multiple comparisons*/
run; 
quit;
ods graphics off;

/*DepartmentalGpaTerm1*/
ods graphics on;
proc glm data=Sta.undergrad PLOTS(MAXPOINTS=NONE) PLOTS(ONLY)=INTPLOT; 
 	class CollegeT1 status; 
 	model DepartmentalGpaTerm1 = CollegeT1 status CollegeT1*status / ss3; 
 	lsmeans CollegeT1 / pdiff stderr; 
 	lsmeans CollegeT1*status / pdiff stderr;
 	label  DepartmentalGpaTerm1 = 'Departmental Gpa (1)' 
 		   CollegeT1    = 'Colleges' 
           status       = 'Status'; 
    title1 'Two-Way ANOVA'; 
 	title2 'DepartmentalGpaTerm1';
 	/*t option: pairwise t-tests for multiple comparisons*/
run; 
quit;
ods graphics off;

/*TotalCompletedSchTerm1*/
ods graphics on;
proc glm data=Sta.undergrad PLOTS(MAXPOINTS=NONE) PLOTS(ONLY)=INTPLOT; 
 	class CollegeT1 status; 
 	model TotalCompletedSchTerm1 = CollegeT1 Gender CollegeT1*Gender / ss3; 
 	lsmeans CollegeT1 / pdiff stderr; 
 	lsmeans CollegeT1*status / pdiff stderr;
 	label  TotalCompletedSchTerm1 = 'Total credit hours (1)' 
 		   CollegeT1    = 'Colleges' 
           status       = 'Status'; 
    title1 'Two-Way ANOVA'; 
 	title2 'TotalCompletedSchTerm1';
 	/*t option: pairwise t-tests for multiple comparisons*/
run; 
quit;
ods graphics off;