## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(fig.path='figures/',
                      collapse = TRUE,
                      comment = "#>",
                      fig.width = 8, fig.height = 7
)

## ----warning=FALSE,results='hide',message=FALSE-------------------------------
## CRAN
#install.packages('jtdm', repos = "http://cran.us.r-project.org")
## Github
#library(devtools)
#install_github("giopogg/jtdm")
library(jtdm)
library(ggplot2)

## ----Y------------------------------------------------------------------------
data(Y)
summary(Y)

## ----X------------------------------------------------------------------------
data(X)
summary(X)

## ----results='hide',warning=FALSE, message=FALSE------------------------------
# Short MCMC to obtain a fast example: results are unreliable !
m = jtdm_fit(Y = Y, X = X,
             formula = as.formula("~poly(GDD,2)+poly(FDD,2)+poly(GDD,2):forest+poly(FDD,2):forest"),
             sample = 100)

summary(m)

## -----------------------------------------------------------------------------
# Inferred parameters
B = getB(m)

## -----------------------------------------------------------------------------
Sigma = get_sigma(m)$Smean

## -----------------------------------------------------------------------------
plot(m)

## -----------------------------------------------------------------------------
predictions = jtdm_predict(m, Xnew = X, Ynew = Y,validation = T)
predictions$R2
predictions$RMSE

## ----results = 'hide',message=FALSE,warning=FALSE-----------------------------
predictionsCV = jtdmCV(m, K = 5, sample = 100)

predictionsCV$R2
predictionsCV$RMSE

## -----------------------------------------------------------------------------
partial_response(m, indexGradient = "GDD",indexTrait = "SLA")$p

## -----------------------------------------------------------------------------
partial_response(m, indexGradient = "GDD", indexTrait = "SLA",
                 FixX = list(GDD = NULL, FDD = NULL, forest = 1))$p

## -----------------------------------------------------------------------------
# plot the pairwise SLA-LNC partial response curve along the GDD gradient
ellipse_plot(m, indexTrait = c("SLA","LNC"), indexGradient = "GDD")

## -----------------------------------------------------------------------------
# plot the pairwise SLA-LNC partial response curve along the GDD gradient
ellipse_plot(m, indexTrait = c("SLA","LNC"), indexGradient="GDD",
             FixX = list(GDD = NULL, FDD = NULL, forest = 1))

## -----------------------------------------------------------------------------
joint_trait_prob(m, indexTrait = c("SLA","LNC"), Xnew = X["VCHA_2940",],
                 bounds = list(c(20,Inf),c(20,Inf)),
                 FullPost = TRUE)$PROBmean


## -----------------------------------------------------------------------------
joint = joint_trait_prob_gradient(m, indexTrait = c("SLA","LNC"), 
                                  indexGradient = "GDD",
                                  bounds = list(c(mean(Y[,"SLA"]),Inf),c(mean(Y[,"SLA"]),Inf)),
                                  FullPost = TRUE)

## ----echo=FALSE---------------------------------------------------------------
table = data.frame(x=joint$gradient, mean= joint$GradProbsmean,
                   q02 = joint$GradProbsq025,
                   q97 = joint$GradProbsq975)
ggplot(data=table) + geom_ribbon(mapping=aes(x=x, ymin=q02, ymax=q97),position = position_dodge(0.3), size=1,alpha=0.2) + geom_line(mapping=aes(x=x, y=mean), size=1, position=position_dodge(width=0.3),col="#F8766D") + xlab("GDD")  + theme_classic() +
  ggtitle("Joint probability of SLA and LNC to be both greater than 10 as a function of GDD")

