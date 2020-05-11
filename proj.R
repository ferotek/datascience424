studentpor <- read.csv("~/Documents/19-20/CSC 424/student/student-por.csv", sep=";")


studentpor2 =studentpor[c(-31,-32)]

n = nrow(studentpor2)
s = sample(n, n/2)
scoretrain = studentpor2[s, ]
scoretest = studentpor2[-s, ]

scoretrainy = scoretrain$G3

n = length(y)
scoretrainx = model.matrix(~., data=scoretrain
                 [,1:30])
scoretrainx = scoretrainx[,-1]

scoretestx = model.matrix(~., data=scoretest
                           [,1:30])
scoretestx = scoretestx[,-1]

scoretesty = scoretest$G3

set.seed(1)
scoreridge = cv.glmnet(scoretrainx, scoretrainy,
                    alpha=0)
plot(scoreridge$cvm)
plot(scoreridge)
scoreridge$lambda.min
glmnet(studentvars, studentscore, alpha=0,  lambda=scoreridge$lambda.min)

ridgepredscoretest = predict(scoreridge, scoretestx, s="lambda.min")

rmsesscoretest = sqrt(mean((ridgepredscoretest - scoretesty)^2))
rmsesscoretest

scorelasso = cv.glmnet(scoretrainx, scoretrainy, alpha = 1)
plot(scorelasso)
print(scorelasso$lambda.min)
coef(scorelasso, s="lambda.min")  

glmnet(studentvars, studentscore, alpha=1,  lambda=cvlasso$lambda.min)

lassopredscoretest = predict(scorelasso, scoretestx, s="lambda.min")

rmsesscoretestlasso = sqrt(mean((lassopredscoretest - scoretesty)^2))
rmsesscoretestlasso






