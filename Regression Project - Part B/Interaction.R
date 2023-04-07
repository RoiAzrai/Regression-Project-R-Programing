library(sjPlot)
library(sjmisc)
library(ggplot2)

data(efc)
theme_set(theme_sjplot())

# make categorical
efc$c161sex <- to_factor(efc$c161sex)

# fit model with interaction
fit <- lm(neg_c_7 ~ c12hour + barthtot * c161sex, data = efc)

plot_model(fit, type = "pred", terms = c("barthtot", "c161sex"))