
#1a) 
data = read.table("D:/Gerrie/All modules taken/ST3243/Project/icu.data.s11.txt", header=FALSE) 
colnames(data) <- c("ID", "STA", "AGE", "SEX", "RACE", "SER", "CAN", "CRN", "INF", "CPR", "SYS", "HRA", "PRE", 'TYP', "FRA", "PO2", "PH", "PCO", 'BIC', 
                    'CRE', 'LOC')
attach(data)

#b) 
plot(AGE, STA, main='Scatterplot of STA against AGE', xlab='AGE', ylab='STA', pch=19) 
#c) 
h1 = sum(STA[AGE> 14 & AGE <= 24])
h2 = sum(STA[AGE> 24 & AGE <= 34])
h3 = sum(STA[AGE> 34 & AGE <= 44])
h4 = sum(STA[AGE> 44 & AGE <= 54])
h5 = sum(STA[AGE> 54 & AGE <= 64])
h6 = sum(STA[AGE> 64 & AGE <= 74])
h7 = sum(STA[AGE> 74 & AGE <= 84])
h8 = sum(STA[AGE> 84 & AGE <= 94])

x1 = sum(AGE> 14 & AGE <= 24)
x2 = sum(AGE> 24 & AGE <= 34)
x3 = sum(AGE> 34 & AGE <= 44)
x4 = sum(AGE> 44 & AGE <= 54)
x5 = sum(AGE> 54 & AGE <= 64)
x6 = sum(AGE> 64 & AGE <= 74)
x7 = sum(AGE> 74 & AGE <= 84)
x8 = sum(AGE> 84 & AGE <= 94)

mean_STA <- c(h1/x1, h2/x2, h3/x3, h4/x4, h5/x5, h6/x6, h7/x7, h8/x8) 
midpt_AGE <- c((15+24)/2, (25+34)/2, (35+44)/2, (45+54)/2, (55+64)/2, (65+74)/2, (75+84)/2, (85+94)/2) 

plot(midpt_AGE, mean_STA, main="Scatterplot of mean_STA against midpt_AGE", xlab="AGE ", ylab="STA ", pch=13)
#d) 
data_glm <- glm(STA~AGE, family=binomial(link='logit'), data=data) 
summary(data_glm)


vector_age_fittedvals <- character()
values <- AGE
for (i in 1:length(AGE)){
  values[i] <- 1/(1+exp(3.25069-0.033394*values[i]))
  vector_age_fittedvals[i] <- values[i]
}


plot(AGE,vector_age_fittedvals, xlab="AGE ", ylab="STA ", pch=16)

vector_midptage_fittedvals <- character()

for (i in 1:length(midpt_AGE)){
  midpt_AGE[i] <- 1/(1+exp(3.25069-0.033394*midpt_AGE[i]))
  vector_midptage_fittedvals[i] <- midpt_AGE[i]
}


plot(midpt_AGE,vector_midptage_fittedvals, xlab="AGE ", ylab="STA ", pch=16)

#g) 
confint(data_glm, level=0.95) 

#i) 
agedata = data.frame(AGE) 
preds = predict(data_glm, type='link', agedata, se=TRUE) 
upper = preds$fit + 1.96*preds$se.fit 

# the values from $se.fit are the standard errors 
lower = preds$fit - 1.96*preds$se.fit 
plot(AGE, preds$fit, main='Pointwise 95% Confidence limits vs AGE for each subject') 
for (k in 1:nrow(data)) {
  points(c(AGE[k], AGE[k]), c(lower[k], upper[k]))
}


#2a) 
table(data$STA, data$CPR) 
odds_ratio = (296*16) / (11*77)
ln_odds_ratio = log(odds_ratio, base = exp(1))
ln_odds_ratio

data_glm <- glm(STA~CPR, family=binomial(link='logit'), data=data) 
summary(data_glm)

squareroot_of_lnOR = sqrt(1/296 + 1/11 + 1/77 + 1/16) 
squareroot_of_lnOR

#calculation for 95% CI for OR below 
upper_limit = exp(ln_odds_ratio + 1.96*squareroot_of_lnOR)
upper_limit

lower_limit = exp(ln_odds_ratio - 1.96*squareroot_of_lnOR)
lower_limit


#b) 
CPR_recoded = ifelse(CPR==0, 4, 2)
data_glm <- glm(STA ~ CPR_recoded, family=binomial(link='logit'), data=data) 
summary(data_glm)


#c) 
table(STA,RACE)
x = factor(RACE) 
is.factor(x) #check if x is factor 
data_glm = glm(STA~x, family=binomial(link='logit'), data=data) 

#d) 

dummy1 = ifelse((AGE >=16 & AGE <=48), 1, 0) 
dummy2 = ifelse((AGE >=49 & AGE <=63), 1, 0) 
dummy3 = ifelse((AGE >=64 & AGE <=73), 1, 0) 
dummy4 = ifelse((AGE >=74 & AGE <=92), 1, 0) 
AGE <- as.factor(AGE) 
x = glm(STA~dummy2+dummy3+dummy4, family=binomial(link='logit'), data=data) 
summary(x) 

col1=c(0,1.3934,1.4447, 2.1919)
col2=c(32,55.5,68, 82.5)
tables=cbind(col1,col2)
plot(col2,col1, xlab='Midpt of age quartiles',ylab='Estimated slope coeffs')

