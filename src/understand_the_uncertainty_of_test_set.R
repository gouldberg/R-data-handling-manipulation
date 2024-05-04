
# ---------------------------------------------------------------------------
# One method for understanding the uncertainty of a test set is to use a 
# confidence interval.
# To obtain a confdence interval for the overall accuracy, the based R function binom.test can be used.
# It requires the user to input the number of samples and the number correctly classified to calculate the interval.
# ---------------------------------------------------------------------------

# for example, suppose a test set sample size is 20 and a model that is 80% accurate (16 out of 20 correct),
# the confidence interval would be computed using

binom.test(16, 20)


# -->
# In this case, the width of the 95% confidence interval is 37.9%.
# Try different samples sizes and accuracy rates to understand the trade-off between the uncertainty in the results,
# the model performance, and the test set size.


binom.test(800, 1000)

