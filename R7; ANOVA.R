# Recitation for Slides07

# One-way ANOVA

## count insects recorded after 6 sprays are applied.
data(InsectSprays) # load data.
str(InsectSprays) # check the data structure.
attach(InsectSprays) # Set it as the default data.

tapply(count, spray, length) # Calculate sample sizes.
tapply(count, spray, mean) # Calculate sample means.
tapply(count, spray, var) # Calculate sample variances.

boxplot(count ~ spray) # Visualize all groups.

cs = aov(count ~ spray) # Do ANOVA
summary(cs) # Produce the ANOVA table
detach(InsectSprays)



# Inference on contrast

## count insects recorded after 6 sprays are applied.
data(InsectSprays); attach(InsectSprays) # Get data.

tapply(count, spray, mean) # do group sample means.

# define the contrasts
pattern = cbind(c(1,1,-1,-1,-1,1), c(0,0,-1,2,-1,0))

# define contrasts and do ANOVA
contrasts(InsectSprays$spray) <- pattern
go <- aov(count ~ spray, data = InsectSprays)

# Test the significance of the contrasts
summary.aov(go, split=list(spray=list("A,B,F vs. C,D,E"=1, "2D vs. C+E"=2)))

detach(InsectSprays)



# Fisher's LSD

data(InsectSprays); attach(InsectSprays)
modi = aov(count ~ spray) # Do ANOVA.

# Use package 'agricolae'. Install it if not listed.
# install.packages("agricolae")
library(agricolae)

# Do LSD
comparison = LSD.test(modi, "spray", p.adj="none")
comparison # print the report
plot(comparison, variation="SD") # Visualize comparison
detach(InsectSprays)