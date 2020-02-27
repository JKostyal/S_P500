# S_P500


# Observation


library(ISLR)
?
fix(Weekly)
cor(Weekly[-9])
plot(Weekly)
attach(Weekly)

#Logistic Regression


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Weekly, family=binomial)
summary(glm.fit)
glm.prob=predict(glm.fit,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.prob>.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)

# split data into training and test


attach(Weekly)
train=Year<2009
Weekly.2008=Weekly[!train,]
Direction.2008=Direction[!train]


# Logistic Regression, one significant predictor


glm.fit1=glm(Direction~Lag2,Weekly,family=binomial,subset=train)
glm.prob1=predict(glm.fit1, Weekly.2008,type="response")
glm.pred1=rep("Down",length(glm.prob1))
glm.pred1[glm.prob1>0.5]="Up"
table(glm.pred1,Direction.2008)
mean(glm.pred1==Direction.2008)
mean(glm.pred1!=Direction.2008)


# Linear Discriminant Analysis


library(MASS)
lda.fit=lda(Direction~Lag2,Weekly,subset=train)
lda.fit
lda.pred=predict(lda.fit,Weekly.2008)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2008)
mean(lda.class==Direction.2008)


# Quadratic Discriminant Analysis


qda.fit=qda(Direction~Lag2+I(Lag2^2),Weekly,subset=train)
qda.pred=predict(qda.fit,Weekly.2008)
names(qda.pred)
qda.class=qda.pred$class
table(qda.class,Direction.2008)
mean(qda.class==Direction.2008)

# K-Nearest Neighbors 

library(class)
attach(Weekly)
train.x=cbind(Lag1,Lag2)[train,]
test.x=cbind(Lag1,Lag2)[!train,]
direction.x=Direction[train]
set.seed(1)
knn.fit=knn(train.x,test.x,direction.x,k=300)
table(knn.fit,Direction.2008)
mean(knn.fit==Direction.2008)
