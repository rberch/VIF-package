# VIF-package
VIFpkg is an R package that computes the Variance Inflation Factor (VIF) of predictor variables when we have multicollinearity issues.
The main funtion is  VIFsM() which finds the VIF values for the variables. The other functions are Plot, which is used to plot the VIF values, and summarY which prints out the VIF values.


## Using the VIFsM() function to find VIF on a predictor dataset in R


### When Interested in the VIF of the current data (multi = FALSE, as in default)

```{r setup}

# load the VIFpkg pacakge

library(VIFpkg)

# Using Boston data from MASS package for our example

df <- data.frame(MASS::Boston[,-1])

# Find VIF values

xs <- VIFsM(df, multi=FALSE,maxim = 4)

# View the results

summarY(object=xs)


# Plot the VIF values

Plot(x = xs,maxx = 4)





```



### When Interested in checking the VIF of the data and dropping the variable with VIF > maxim


```{r, fig.height=12, fig.width=10}
# Find VIF values

xs <- VIFsM(df, multi=TRUE,maxim = 4)

# View the results

summarY(object=xs, multi = TRUE)


# Plot the VIF values

Plot(x = xs,multi = TRUE, maxx = 4)

```


