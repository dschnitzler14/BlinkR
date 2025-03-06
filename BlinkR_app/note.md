# Calculating the Effect Size  

Now we can calculate the effect size.  

The effect size is a measure of how strong the difference is between two groups. Unlike p-values, which tell us whether a difference exists, effect size tells us how meaningful that difference is.  

A larger effect size means a bigger difference between groups, while a smaller effect size suggests a small or negligible difference.  

## Using Cohen’s D  

Here, we will use Cohen’s D, which is commonly used to measure effect size for comparisons between two groups.  

A general rule for interpreting Cohen’s D:  
- 0.2 → Small effect  
- 0.5 → Medium effect  
- 0.8 or higher → Large effect  

We can calculate Cohen’s D in R using the following code:  

```r
effect_size_t <- average_trs %>%
  cohens_d(Average_Blinks_Per_Minute ~ Stress_Status)
