#read data
studentpor <- read.csv("~/Documents/19-20/CSC 424/student/student-por.csv", sep=";")
#mean test score
studentpor$meanscore = (studentpor$G1 + studentpor$G2 + studentpor$G3 )/3
studentpor2 =studentpor[c(-31, -32,-33)]
scoremmatrix = model.matrix(~., data=studentpor2)
scoremmatrix = scoremmatrix[,c(-1)]

set.seed(123)

library(glmnet)
library(psych)

#converting data into a matrix, dividing the data into training and testing data
scoremmatrix = model.matrix(~., data=studentpor2)
scoremmatrix = scoremmatrix[,c(-1)]

n = nrow(scoremmatrix)
s = sample(n, n/2)
scoretrainx = scoremmatrix[s, ]
scoretrainy = scoretrainx[,c(40)]
scoretrainx = scoretrainx[,c(-40)]

scoretestx = scoremmatrix[-s, ]
scoretesty = scoretestx[,c(40)]
scoretestx = scoretestx[,c(-40)]


#elastic net regression


alphaBest = 0
bestError = 9999999    # Start out with a huge error
alph = seq(0, 1, .1)
mylist = c()
for (alpha in seq(0, 1, .1))
{
  meanError = 0
  for (i in 1:100)
  {
    # Grab test and training sets
    n = nrow(scoremmatrix)
    s = sample(n, n/2)
    scoreTrain = scoremmatrix[s, ]
    scoreTest = scoremmatrix[-s, ]
    
    xTrain = as.matrix(scoreTrain[, -40])   
    yTrain = as.matrix(scoreTrain[, 40])   
    xTest = as.matrix(scoreTest[, -40])  
    yTest = as.matrix(scoreTest[, 40])    
    
    fitElastic = cv.glmnet(xTrain, yTrain, alpha=alpha, nfolds=7)
    elasticPred = predict(fitElastic, xTest, s="lambda.min")
    meanError = meanError + sqrt(mean((elasticPred - yTest)^2))
  }
  meanError = meanError / 100
  
  print(meanError)
  
  mylist = c(mylist, meanError)
  if (meanError < bestError)
  {
    alphaBest = alpha
    bestError = meanError
  }
  
}

glmnet(scoretrainx, scoretrainy, alpha=0.1,  lambda=fitElastic$lambda.min)

fitElastic = cv.glmnet(xTrain, yTrain, alpha=0.1, nfolds=7)


#factor analysis with the elastic net variables
studenttraine = scoretrainx[,c("schoolMS", "addressU", "famsizeLE3", "Fedu", "Mjobhealth"
                              ,"Mjobteacher","Fjobteacher", "reasonreputation", "studytime",
                              "failures", "schoolsupyes", "nurseryyes", "higheryes",
                              "romanticyes", "freetime", "Walc", "absences")]



studentteste = scoretestx[,c("schoolMS", "addressU", "famsizeLE3", "Fedu", "Mjobhealth"
                           ,"Mjobteacher","Fjobteacher", "reasonreputation", "studytime",
                           "failures", "schoolsupyes", "nurseryyes", "higheryes",
                           "romanticyes", "freetime", "Walc", "absences")]




sfact <- factanal(studenttraine, factors=4, rotation="none", scores="regression")
sfact
summary(sfact)

#4 factors fail to reject h0

print(sfact$loadings, cutoff = 0.4, sort = TRUE)
scores = as.data.frame(sfact$scores)
scores$grade = scoretrainy


#linear regression with components
fitpca = lm(grade ~ ., data=scores)
summary(fitpca)


pfs5 = principal(studenttraine, rotate="varimax", nfactors=5)
scores5 = as.data.frame(pfs5$scores)
scores5$grade = scoretrainy
fitpca5 = lm(grade ~ ., data=scores5)
summary(fitpca5)
#slightly higher r squared with 4 components compared to 5

#validation on testing data
pfse = principal(studentteste, rotate="varimax", nfactors=5)
print(pfse$loadings, cutoff = 0.4, sort = TRUE)
scoreste = as.data.frame(pfse$scores)
scoreste$grade = scoretesty

scoretestframe = as.data.frame(scoreste)
pcapred = predict(fitpca, scoretestframe)
rmsepca = sqrt(mean((pcapred - scoretesty)^2))
rmsepca





