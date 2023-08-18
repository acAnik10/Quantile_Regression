#Comparison between OLS regression and quantile regression by simulation
#========================================================================

#When error is heteroscedastic
#===============================
m = 100
n = 100
alpha = 2
beta = 5

set.seed(seed=900)
x = runif(n,1,5)

mu = alpha + beta * x

sigma = exp(x)

#generating 100 observations for y from N(mui, sigmai^2) for each xi)
y = matrix(0, m, n)

for(i in 1:n){
  y[,i] = rnorm(m, mu[i], sigma[i])
}

#fitting OLS regression
#==========================
yibar = colMeans(y)

model = (lm(yibar~x))
summary(model)

library(ggplot2)

x1 = rep(x,each = m)
y1 = as.vector(y)
data = data.frame(x1,y1)

ggplot(data, aes(x1,y1))+
  geom_point()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              col = 'red')+
  labs(x = 'x', y = 'y',
       title = 'Fitting OLS Regression on the Data')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


#residual plot
ggplot()+
  geom_point(aes(x, model$residuals),col = 'red')+
  geom_hline(yintercept = 0, size = 0.5)+
  labs(y = 'Residuals',
       title = 'Residual Plot for the OLS Regression')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  


#Fitting Quantile Regression
#=============================

library(quantreg)

#function to fit quantile regression
#Output : Model and the fitted values
qfit = function(t){
  q1 = rq(y1~x1,t)
  Output = list(coefficients = q1$coefficients,
                predicted_values = q1$fitted.values)
  return(Output)
}

#fitting the regression line for quantiles 0.05, 0.5 and 0.95
ggplot()+
  geom_point(aes(x1,y1))+
  geom_line(aes(x1, qfit(0.5)$predicted_values, col = '0.5'), lwd = 1.2)+
  geom_line(aes(x1, qfit(0.05)$predicted_values, col =  '0.05'), lwd = 1.2)+
  geom_line(aes(x1, qfit(0.95)$predicted_values, col = '0.95'), lwd = 1.2)+
  labs(x = 'x', y = 'y',
       title = 'Fitting Quantile Regression on the Data',
       subtitle = expression(tau * ' = 0.05, 0.5, 0.95'),
       color = expression(tau))+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5))

#model coefficients
qfit(0.05)$coefficients
qfit(0.5)$coefficients
qfit(0.95)$coefficients


#finding the goodness of fit for each regression line

library(WRTDStidal)

Rp = function(t){
  res1 = rq(y1~x1, tau = t)$residuals
  res2 = rq(y1~1, tau = t)$residuals
  R = goodfit(res1 , res2, tau = t)
  return(R)
}

#tau = 0.05
#-----------
R1 = Rp(0.05)

#tau = 0.5
#-----------
R2 = Rp(0.5)

#tau = 0.95
#-----------
R3 = Rp(0.95)

