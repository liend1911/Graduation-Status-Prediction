
####### Pre-lim data prep #######

# Import Data
library(readxl)
dbs <- read_excel("DBS.xlsx")
# View(dbs)


#Subset excluding "Employee ID" , "TERMDESCRIPTION" , "ACADEMICCAREER", 
#"TRANSFERSTUDENTFLAG", "doctortimetogradbyprogram"
#  "terms to graduate" & "currentlyEnrolledFlag"

dbs_sub <- subset(dbs, select = c("collegedescription", "GENDERDESCRIPTION", "gre_verb_new", "gre_quan_new",
                                  "masterDegreeFlag","doctorDegreeFlag",
                                  "TermGPA_1stFallTerm",	"TermCompletedSCH_1stFallTerm",	"CumulativeGPA_1stFallTerm",	
                                  "TermGPA_1stSpringTerm",	"TermCompletedSCH_1stSpringTerm", "CumulativeGPA_1stSpringTerm",
                                  "TermGPA_1stSummerTerm",	"TermCompletedSCH_1stSummerTerm",	"CumulativeGPA_1stSummerTerm",
                                  "Cumulative GPA First Year"))


#Response variable y, 0 no degree 1 at least one degree
dbs_sub$y <- as.numeric(dbs_sub$masterDegreeFlag + dbs_sub$doctorDegreeFlag != 0) 

#New variables created 
dbs_sub$GPADiff <- dbs_sub$TermGPA_1stSpringTerm - dbs_sub$TermGPA_1stFallTerm
dbs_sub$TermCompletedSCH_1stSpringTerm <- replace(dbs_sub$TermCompletedSCH_1stSpringTerm,
                                                  is.na(dbs_sub$TermCompletedSCH_1stSpringTerm),0)
dbs_sub$FallSpringCH <- dbs_sub$TermCompletedSCH_1stFallTerm + dbs_sub$TermCompletedSCH_1stSpringTerm
dbs_sub$TermCompletedSCH_1stSummerTerm <- replace(dbs_sub$TermCompletedSCH_1stSummerTerm,
                                                  is.na(dbs_sub$TermCompletedSCH_1stSummerTerm),0)
dbs_sub$SummerTaken <- ifelse(dbs_sub$TermCompletedSCH_1stSummerTerm == 0 ,0,1)
dbs_sub$GRETaken <- dbs_sub$gre_verb_new + dbs_sub$gre_quan_new
dbs_sub$GRETaken <- replace(dbs_sub$GRETaken, is.na(dbs_sub$GRETaken),0)
dbs_sub$GRETaken <- ifelse(dbs_sub$GRETaken == 0 , 0, 1)

# standardized GPA
dbs_sub$norm.GPA <- ((dbs_sub$`Cumulative GPA First Year`
                    - mean(dbs_sub$`Cumulative GPA First Year`))) / (sd(dbs_sub$`Cumulative GPA First Year`))


#ifelse(function,x,y) = if function then x, else y changing to numerical binary
dbs_sub$GENDERDESCRIPTION = ifelse(dbs_sub$GENDERDESCRIPTION == "Male",0,1)
dbs_sub$collegedescription =  ifelse(dbs_sub$collegedescription == "College AA",0,1)


#replace spring GPAs with last observed 
dbs_sub$TermGPA_1stSpringTerm <- ifelse(is.na(dbs_sub$TermGPA_1stSpringTerm) & !is.na(dbs_sub$TermGPA_1stFallTerm),dbs_sub$TermGPA_1stFallTerm,dbs_sub$TermGPA_1stSpringTerm)
dbs_sub$TermCompletedSCH_1stSpringTerm <- ifelse(is.na(dbs_sub$TermCompletedSCH_1stSpringTerm) & !is.na(dbs_sub$TermCompletedSCH_1stFallTerm),dbs_sub$TermCompletedSCH_1stFallTerm,dbs_sub$CumulativeGPA_1stSpringTerm)
dbs_sub$CumulativeGPA_1stSpringTerm <- ifelse(is.na(dbs_sub$CumulativeGPA_1stSpringTerm) & !is.na(dbs_sub$CumulativeGPA_1stFallTerm),dbs_sub$CumulativeGPA_1stFallTerm,dbs_sub$CumulativeGPA_1stSpringTerm)

dbs_sub_new  <- subset((dbs_sub), 
                       select = c("collegedescription", "GENDERDESCRIPTION", "gre_verb_new",
                                  "gre_quan_new", "TermGPA_1stFallTerm", "TermGPA_1stSpringTerm",
                                  "Cumulative GPA First Year", 
                                  "FallSpringCH", "SummerTaken","norm.GPA", "y"))
#Removing 43 students who did not take GRE () 355-43 = 312
dbs_sub_new  <- subset(na.omit(dbs_sub_new), 
                       select = c("collegedescription", "GENDERDESCRIPTION", "gre_verb_new",
                                  "gre_quan_new", "TermGPA_1stFallTerm", "TermGPA_1stSpringTerm",
                                  "Cumulative GPA First Year", 
                                  "FallSpringCH", "SummerTaken","norm.GPA", "y"))

#write.csv(dbs_sub_new, 'dbs_sub_new.csv')


####### Distribution of Accuracies (100 runs) 

library(MASS)    # lda() and qda()
library(caret)   # confusionMatrix()
library(class)   # knn()

##### set number of runs and thresholds (0<x<1) ######
num_of_run = 100 
LDA_threshold = 0.5 # default: predictions > 0.5 are classified as 1, otherwise 0
QDA_threshold = 0.5
LR_threshold = 0.538 # 0.538 best for now; 0.545


# initialize variables to store results (accuracy and confusion matrix)
accuracy_matrix = matrix(nrow = num_of_run, ncol = 4) # 100x4 matrix to store accuracies of 4 methods
LDA_cm_list = list() # 100x1 column vector to store 100 confusion matrices for LDA
QDA_cm_list = list() # 100x1 column vector to store 100 confusion matrices for QDA
LR_cm_list = list()  # 100x1 column vector to store 100 confusion matrices for LR
KNN_cm_list = list()  # 100x1 column vector to store 100 confusion matrices for KNN

for (i in 1:num_of_run)
{
  
  set.seed(i) # to make the results reproducible
  
  n=nrow(dbs_sub_new)
  size.train=floor(n*0.70)
  size.test=floor(n*0.30)
  id.train=sample(1:n,size.train,replace=FALSE)
  id.test=sample(setdiff(1:n,id.train),size.test,replace=FALSE)
  mydata.train=dbs_sub_new[id.train,]
  mydata.test=dbs_sub_new[id.test,]
  
  mydata.train$y = as.factor(mydata.train$y)
  mydata.test$y = as.factor(mydata.test$y)
  
  ##### Column 1: Linear Discriminant Analysis #####
  LDA_model = lda(y~ norm.GPA, data = mydata.train) 
  
  LDA_predict = predict(LDA_model, mydata.test)
  LDA_predict = LDA_predict$posterior[,(2)]  # returns a list of numbers between 0 and 1
  LDA_predict = ifelse(LDA_predict>LDA_threshold, 1, 0) 
  
  LDA_accuracy = mean(LDA_predict==mydata.test$y)
  accuracy_matrix[i,1] = LDA_accuracy
  LDA_cm_list[[i]] = table(as.factor(LDA_predict), mydata.test$y) 
  
  ##### Column 2: Quadratic Discriminant Analysis #####
  QDA_model = qda(y~ norm.GPA, data = mydata.train)
  
  QDA_predict = predict(QDA_model, mydata.test)
  QDA_predict = QDA_predict$posterior[,(2)]  # returns a list of numbers between 0 and 1
  QDA_predict = ifelse(QDA_predict>QDA_threshold, 1, 0)  
 
  QDA_accuracy = mean(QDA_predict==mydata.test$y)
  accuracy_matrix[i,2] = QDA_accuracy
  QDA_cm_list[[i]] = table(as.factor(QDA_predict), mydata.test$y)
  
  ##### Column 3: Logistic Regression #####
  LR_model = glm(formula = y ~ norm.GPA, data = mydata.train, family = binomial)
  
  LR_predict = predict(LR_model, type = "response", newdata=mydata.test) # returns a list of numbers between 0 and 1
  LR_predict = ifelse(LR_predict>LR_threshold, 1, 0) 
  
  LR_accuracy = mean(LR_predict==mydata.test$y)
  accuracy_matrix[i,3] = LR_accuracy
  LR_cm_list[[i]] = table(as.factor(LR_predict), mydata.test$y)
  
  ##### Column 4: k Nearest Neighbor #####
  KNN_predict = knn(mydata.train[,"norm.GPA"],mydata.test[,"norm.GPA"],mydata.train$y,k=7) # k=7 yields best accuracy from Lien's code
  
  KNN_accuracy = mean(KNN_predict==mydata.test$y)
  accuracy_matrix[i,4] = KNN_accuracy
  KNN_cm_list[[i]] = table(as.factor(KNN_predict), mydata.test$y)
}

# check results after loop
#accuracy_matrix
#head(LDA_cm_list) # LDA_cm_list contains 100 confusion matrices for LDA; head() shows the first 6 of them 
#head(QDA_cm_list)
#head(LR_cm_list)
#head(KNN_cm_list)

##### mean accuracy ######
LDA_accuracy_mean = mean(accuracy_matrix[,1])
QDA_accuracy_mean = mean(accuracy_matrix[,2])
LR_accuracy_mean = mean(accuracy_matrix[,3])
KNN_accuracy_mean = mean(accuracy_matrix[,4])

LDA_accuracy_mean
QDA_accuracy_mean 
LR_accuracy_mean
KNN_accuracy_mean

##### standard error #####
LDA_accuracy_se = sd(accuracy_matrix[,1]) / sqrt(num_of_run)
QDA_accuracy_se = sd(accuracy_matrix[,2]) / sqrt(num_of_run)
LR_accuracy_se = sd(accuracy_matrix[,3]) / sqrt(num_of_run)
KNN_accuracy_se = sd(accuracy_matrix[,4]) / sqrt(num_of_run)

LDA_accuracy_se
QDA_accuracy_se
LR_accuracy_se
KNN_accuracy_se

##### taking average of TN,FN,FP,TP counts #####
LDA_cm = confusionMatrix(Reduce("+", LDA_cm_list) / length(LDA_cm_list), positive="1")
QDA_cm = confusionMatrix(Reduce("+", QDA_cm_list) / length(QDA_cm_list), positive="1")
LR_cm = confusionMatrix(Reduce("+", LR_cm_list) / length(LR_cm_list), positive="1")
KNN_cm = confusionMatrix(Reduce("+", KNN_cm_list) / length(KNN_cm_list), positive="1")

LDA_cm$table
LDA_cm$byClass
QDA_cm$table
QDA_cm$byClass
LR_cm$table
LR_cm$byClass
KNN_cm$table
KNN_cm$byClass


# sample confusion matrix of one run (we take average from 100 matrices like this)
#             Reference
# Prediction  0      1
#          0  5(TN)  0(FN)
#          1 14(FP) 74(TP)


# plot accuracy distributions with vertical lines for means 
plot(density(accuracy_matrix[,1]), xlim=c(0.60, 1), ylim=c(0, 15), col='blue', lwd=2, main='Accuracy distribution of all models',xlab='Accuracy', ylab='Frequency')
lines(density(accuracy_matrix[,2]), col='red', lwd=2)
lines(density(accuracy_matrix[,3]), col='green', lwd=2)
lines(density(accuracy_matrix[,4]), col='brown', lwd=2)
legend("topright",legend = c("LDA", "QDA", "Logistic Regression", "KNN"), col = c("blue","red", "green", "brown"), lty = c(1,1), cex=.4)
abline(v = LDA_accuracy_mean, col="blue", lwd=1.5, lty=2)  #vertical line for LDA_accuracy_mean
abline(v = QDA_accuracy_mean, col="red", lwd=1.5, lty=2)
abline(v = LR_accuracy_mean, col="green", lwd=1.5, lty=2)
abline(v = KNN_accuracy_mean, col="brown", lwd=1.5, lty=2)



