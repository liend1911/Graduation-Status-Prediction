libname Diff '/home/u49804996/Diff';

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
data Diff.undergrad (keep= AcademicProgramDescription Gender CollegeDescriptionTerm1 
						   CollegeDescriptionTerm2 TermGpaTerm1 TermGPATerm2 CollegeT12 gpadiff);
set T1.grd;
if AcademicProgramDescription='Masters' then delete;
CollegeDescriptionTerm1 = put(CollegeDescriptionTerm1, $College.);
CollegeDescriptionTerm2 = put(CollegeDescriptionTerm2, $College.);
if CollegeDescriptionTerm1 = CollegeDescriptionTerm2 then CollegeT12 = CollegeDescriptionTerm2 ;
else delete;
if CollegeT12 = 4 then delete;
if CollegeT12 = 5 then delete;
if CollegeT12 = 6 then delete;
gpadiff = TermGPATerm2 - TermGPATerm1;
run;

/*Descriptive statistics*/
proc sort data = Diff.undergrad; 
 	by CollegeT12; 
run;

proc freq data=Diff.undergrad;
tables CollegeT12 / nocol nopct
plots=freqplot(twoway=stacked orient=horizontal);
title ‘Summary of colleges (undergraduate)’;
run;

/*Summary Statistics*/
proc tabulate data=Diff.undergrad;
class CollegeT12;
var gpadiff;
tables gpadiff*(MEAN STD N), CollegeT12 all;
title 'Summary statistics by colleges';
run;

/*one way ANOVA*/
proc means mean std n data = Diff.undergrad; 
by CollegeT12; 
var gpadiff; 
title1 'One-Way ANOVA'; 
run; 

ods graphics on; 
proc glm data = Diff.undergrad plots = boxplot PLOTS(MAXPOINTS=NONE); 
class CollegeT12; 
model gpadiff = CollegeT12; 
means CollegeT12 / hovtest t welch dunnett('1'); 
run; 
ods graphics off;