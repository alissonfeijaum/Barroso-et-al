#The distribution of marine gastropods is more influenced by 
#larval development than by adult characteristics
#Barroso et al. 2022. Marine Biology

#Import data

#fit full model
library(nlme)
fm0 <- gls(Range ~ 
             Larva + 
             Depth + 
             Zonation +
             Habitats + 
             Length + 
             Larva:Depth + 
             Larva:Habitats + 
             Larva:Length + 
             Depth:Habitats + 
             Depth:Length +
             Habitats:Length, 
           data = dados,
           method = "ML")

fm1 <- lme(Range ~ 
             Larva + 
             Depth + 
             Zonation +
             Habitats + 
             Length + 
             Larva:Depth + 
             Larva:Habitats + 
             Larva:Length + 
             Depth:Habitats + 
             Depth:Length +
             Habitats:Length, 
           random = ~1 | Genus/Family, 
           data = dados,
           method = "ML")

#Compare model with or without random factors
AIC(fm0, fm1)
anova(fm0, fm1)

#select random factors
fm2 <- update(fm1, random = ~1 | Genus)
fm3 <- update(fm1, random = ~1 | Family)

#compare models for random factors
AIC(fm1, fm2, fm3)
anova.lme(fm1, fm3, type="marginal") #best model = fm3

#select fixed factors
summary(fm3)

fm4 <- update(fm3, . ~ . -
                Larva:Depth - 
                Larva:Habitats - 
                Larva:Length - 
                Depth:Habitats - 
                Depth:Length -
                Habitats:Length) #exclude non-significant interaction terms

fm5 <- update(fm4, .~. - Zonation - Length) #further exclude non-significant terms

#compare models for fixed factors
AIC(fm3, fm4, fm5)
anova.lme(fm3, fm5, type="marginal")

#fit final model with REML
final.model <- update(fm5, method="REML") 
summary(final.model)

#fitted vs residual plot
plot(final.model)  


#Analysis Hierarchical Partitioning
library(hier.part) 

env <- data.frame(Larval = dados$Larval, 
                  Depth = dados$Depth, 
                  Habitats = dados$Habitats,
                  Zonation = dados$Zonation,
                  Length = dados$Length) #creating the data set of the independent variables
gofs <- all.regs(y = dados$Range, xcan = env, 
                 fam = "gaussian",
                 gof = "Rsqu", 
                 print.vars = TRUE)
partition(gofs, pcan = 5) #partitioning results

#Test model significant variables results
mod1 <- lme(Range ~ Depth, random = ~1 | Genus/Family, data = dados)
mod2 <- lme(Range ~ Larval, random = ~1 | Genus/Family, data = dados)
mod3 <- lme(Range ~ Habitats, random = ~1 | Genus/Family, data = dados)
mod4 <- lme(Range ~ Length, random = ~1 | Genus/Family, data = dados)
mod5 <- lme(Range ~ Zonation, random = ~1 | Genus/Family, data = dados)

#Significance value from ANOVA
anova(mod1)
anova(mod2)
anova(mod3)
anova(mod4)
anova(mod5)


#Bootstraping of larval types
boot <- matrix(nrow=1000, ncol=3)
colnames(boot) <- c("IM","LEC","PLA")

set.seed(2022)
for(i in 1:1000) {
boot[i,] <- table(sample(dados$Larva, 122, replace=T))/122*100 }

res.boot <- cbind(
  mean=colMeans(boot),
  SD = apply(boot, 2, sd),
  lower = colMeans(boot) - 1.96*apply(boot, 2, sd),
  upper = colMeans(boot) + 1.96*apply(boot, 2, sd)
)

res.boot

