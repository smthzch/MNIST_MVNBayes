#load data
library(e1071)

source("bayes.R")
source("loadImgs.R")
load_mnist()

#show examples
show_digit(train.b$`0u`)
show_digit(diag(train.b$`0s`))
show_digit(diag(train.b$`1s`))
show_digit(train.b$`1s`[243,])# cool structure, covariance of point
show_digit(diag(train.b$`2s`))
show_digit(diag(train.b$`3s`))
show_digit(diag(train.b$`4s`))
show_digit(diag(train.b$`5s`))
show_digit(diag(train.b$`6s`))
show_digit(diag(train.b$`7s`))
show_digit(diag(train.b$`8s`))
show_digit(diag(train.b$`9s`))

# Naive bayes as baseline
train.nb = naiveBayes(x=train$x,y=as.factor(train$y))

test$yhat = predict(train.nb, test$x, type="class")
sum(test$yhat==test$y) / test$n # ~50% accuracy

#test stuff
x = train$x_
y = train$y

#MVN bayes classifier
train.b = bayesClass.train(x=train$x,y=train$y)

st = Sys.time()
test.b.pred = bayesClass.pred(train.b, test$x)
ed = Sys.time()
ed - st
sum(test$y==test.b.pred) / test$n # 93.9% accuracy