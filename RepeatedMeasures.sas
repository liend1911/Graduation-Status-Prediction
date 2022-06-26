libname repeat '/home/u49804996/repeat';
libname T1_grd '/home/u49804996/T1_grd';

/*import data set into a library*/
proc import out=T1_grd
datafile = '/home/u49804996/myGRD.xlsx'
dbms=xlsx
replace;
getnames=YES;
run;

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
data repeat.undergrad (keep= AcademicProgramDescription Gender CollegeDescriptionTerm1 
						   CollegeDescriptionTerm2 TermGpaTerm1 TermGPATerm2 CollegeR);
set T1_grd;
if AcademicProgramDescription='Masters' then delete;
CollegeDescriptionTerm1 = put(CollegeDescriptionTerm1, $College.);
CollegeDescriptionTerm2 = put(CollegeDescriptionTerm2, $College.);
if CollegeDescriptionTerm1 = CollegeDescriptionTerm2 then CollegeR = CollegeDescriptionTerm2 ;
else delete;
if CollegeR = 4 then delete;
if CollegeR = 5 then delete;
if CollegeR = 6 then delete;
run;

/*Summary Statistics*/
proc tabulate data=repeat.undergrad;
   class Gender CollegeR;
   var TermGPATerm1 TermGPATerm2;
   table CollegeR*Gender all, (TermGPATerm1 TermGPATerm2)*(MEAN);
   title 'Marginal Summary statistics';
run;

/*group effect: colleges*/
/*time effect: term 1 + term 2*/