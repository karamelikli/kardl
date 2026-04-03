# Symmetry Test for non-linear KARDL Models

The symmetry test is a statistical procedure used to assess the presence
of symmetry in the relationship between variables in a model. It is
particularly useful in econometric analysis, where it helps to identify
whether the effects of changes in one variable on another are symmetric
or asymmetric. The test involves estimating a model that includes both
positive and negative components of the variables and then performing a
Wald test to determine if the coefficients of these components are
significantly different from each other. If the test indicates
significant differences, it suggests that the relationship is
asymmetric, meaning that the impact of increases and decreases in the
variables differs. This test returns results for both long-run and
short-run variables in a KARDL model. Where applicable, it provides the
Wald test statistics, p-values, degrees of freedom, sum of squares, and
mean squares for each variable tested. If the null hypothesis of
symmetry is rejected, it indicates that the effects of positive and
negative changes in the variable are significantly different, suggesting
an asymmetric relationship.

The non-linear model with one asymmetric variables is specified as
follows: \$\$ \Delta{y\_{t}} = \psi + \eta\_{0}y\_{t - 1} +
\eta^{+}\_{1} x^{+}\_{t - 1}+ \eta^{-}\_{1} x^{-}\_{t - 1} + \sum\_{j =
1}^{p}{\gamma\_{j}\Delta y\_{t - j}} + \sum\_{j =
0}^{q}{\beta^{+}\_{j}\Delta x^{+}\_{t - j}} + \sum\_{j =
0}^{m}{\beta^{-}\_{j}\Delta x^{-}\_{t - j}} + e\_{t} \$\$

This function performs the symmetry test both for long-run and short-run
variables in a kardl model. It uses the
[`nlWaldtest`](https://rdrr.io/pkg/nlWaldTest/man/nlWaldtest.html)
function from the nlWaldTest package for long-run variables and the
[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
function from the car package for short-run variables. The hypotheses
for the long-run variables are: \$\$ H\_{0}:
-\frac{\eta^{+}\_{1}}{\eta\_{0}} = -\frac{\eta^{-}\_{1}}{\eta\_{0}} \$\$
\$\$ H\_{1}: -\frac{\eta^{+}\_{1}}{\eta\_{0}} \neq
-\frac{\eta^{-}\_{1}}{\eta\_{0}} \$\$

The hypotheses for the short-run variables are: \$\$ H\_{0}: \sum\_{j =
0}^{q}{\beta^{+}\_{j}} = \sum\_{j = 0}^{m}{\beta^{-}\_{j}} \$\$
\$\$H\_{1}: \sum\_{j = 0}^{q}{\beta^{+}\_{j}} \neq \sum\_{j =
0}^{m}{\beta^{-}\_{j}} \$\$

## Usage

``` r
symmetrytest(kmodel)
```

## Arguments

- kmodel:

  The kardl obejct

## Value

A list with class "kardl" containing the following components:

- `Lwald:` A data frame containing the Wald test results for the
  long-run variables, including F-statistic, p-value, degrees of
  freedom, and residual degrees of freedom.

- `Lhypotheses:` A list containing the null and alternative hypotheses
  for the long-run variables.

- `Swald:` A data frame containing the Wald test results for the
  short-run variables, including F-statistic, p-value, degrees of
  freedom, residual degrees of freedom, and sum of squares.

- `Shypotheses:` A list containing the null and alternative hypotheses
  for the short-run variables.

## Details

This function performs symmetry tests on non-linear KARDL models to
assess whether the effects of positive and negative changes in
independent variables are statistically different.

This function evaluates whether the inclusion of a particular variable
in the model follows a linear relationship or exhibits a non-linear
pattern. By analyzing the behavior of the variable, the function helps
to identify if the relationship between the variable and the outcome of
interest adheres to a straight-line assumption or if it deviates,
indicating a non-linear interaction. This distinction is important in
model specification, as it ensures that the variable is appropriately
represented, which can enhance the model's accuracy and predictive
performance.

## References

Shin, Y., Yu, B., & Greenwood-Nimmo, M. (2014). Modelling asymmetric
cointegration and dynamic multipliers in a nonlinear ARDL framework.
Festschrift in honor of Peter Schmidt: Econometric methods and
applications, 281-314.

## See also

[`kardl`](kardl.md), [`pssf`](pssf.md), [`psst`](psst.md),
[`ecm`](ecm.md), [`narayan`](narayan.md)

## Examples

``` r
kardl_model<-kardl(imf_example_data,
                   CPI~Lasym(PPI+ER)+Sas(ER)+deterministic(covid)+trend)
ast<- symmetrytest(kardl_model)
ast
#> 
#> Symmetry Test Results - Long-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value  Pr(>F)  
#> PPI  1 0.00002915 0.00002915  0.1321 0.71644  
#> ER   1 0.00093159 0.00093159  4.2217 0.04049 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Symmetry Test Results - Short-run:
#> =======================
#>    Df Sum of Sq   Mean Sq F value   Pr(>F)   
#> ER  1 0.0021074 0.0021074  9.5499 0.002124 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
# Detailed results of the test:
summary(ast)
#> Long-run symmetry tests:
#> 
#> Test for variable:  PPI 
#> F-value:  0.1320953 , p-value:  0.7164405 
#> Test Decision:  Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable PPI. 
#> Hypotheses:
#> H0: - Coef(LK1_PPI_POS)/Coef(LK1_CPI) = - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)
#> H1: - Coef(LK1_PPI_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)
#> 
#> Test for variable:  ER 
#> F-value:  4.221689 , p-value:  0.04048856 
#> Test Decision:  Reject H0 at 5% level. Indicating long-run asymmetry for variable ER. 
#> Hypotheses:
#> H0: - Coef(LK1_ER_POS)/Coef(LK1_CPI) = - Coef(LK1_ER_NEG)/Coef(LK1_CPI)
#> H1: - Coef(LK1_ER_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_ER_NEG)/Coef(LK1_CPI)
#> 
#> 
#> _____________________________
#> Short-run symmetry tests:
#> 
#> Test for variable:  ER 
#> F-value:  9.549852 , p-value:  0.002123947 
#> Test Decision:  Reject H0 at 5% level. Indicating short-run asymmetry for variable ER. 
#> Hypotheses:
#> H0: Coef(D0.d.ER_POS) + Coef(D1.d.ER_POS) = Coef(D0.d.ER_NEG)
#> H1: Coef(D0.d.ER_POS) + Coef(D1.d.ER_POS) ≠ Coef(D0.d.ER_NEG)
#> 
# The null hypothesis of the test is that the model is symmetric, while the alternative
# hypothesis is that the model is asymmetric. The test statistic and p-value are provided
# in the output. If the p-value is less than a chosen significance level (e.g., 0.05),
# we reject the null hypothesis and conclude that there is evidence of asymmetry in the model.

# To get symmetry test results in long-run, you can use the following code:
ast$Lwald
#> Symmetry Test Results - Long-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value  Pr(>F)  
#> PPI  1 0.00002915 0.00002915  0.1321 0.71644  
#> ER   1 0.00093159 0.00093159  4.2217 0.04049 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# To get symmetry test results in short-run, you can use the following code:
ast$Swald
#> Symmetry Test Results - Short-run:
#> =======================
#>    Df Sum of Sq   Mean Sq F value   Pr(>F)   
#> ER  1 0.0021074 0.0021074  9.5499 0.002124 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# To get the null and alternative hypotheses of the test in long-run,
# you can use the following code:

ast$Lhypotheses
#> $H0
#> $H0$PPI
#> [1] "- Coef(LK1_PPI_POS)/Coef(LK1_CPI) = - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)"
#> 
#> $H0$ER
#> [1] "- Coef(LK1_ER_POS)/Coef(LK1_CPI) = - Coef(LK1_ER_NEG)/Coef(LK1_CPI)"
#> 
#> 
#> $H1
#> $H1$PPI
#> [1] "- Coef(LK1_PPI_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)"
#> 
#> $H1$ER
#> [1] "- Coef(LK1_ER_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_ER_NEG)/Coef(LK1_CPI)"
#> 
#> 

# To get the null and alternative hypotheses of the test in short-run,
# you can use the following code:

ast$Shypotheses
#> $H0
#> $H0$ER
#> [1] "Coef(D0.d.ER_POS) + Coef(D1.d.ER_POS) = Coef(D0.d.ER_NEG)"
#> 
#> 
#> $H1
#> $H1$ER
#> [1] "Coef(D0.d.ER_POS) + Coef(D1.d.ER_POS) ≠ Coef(D0.d.ER_NEG)"
#> 
#> 

# Using magrittr package
library(magrittr)
imf_example_data %>% kardl(CPI~ER+PPI+asym(ER+PPI)+deterministic(covid)+trend,
                           mode=c(1,0,1,1,0)) %>% symmetrytest()
#> 
#> Symmetry Test Results - Long-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value  Pr(>F)  
#> ER   1 0.00108422 0.00108422  4.4841 0.03476 *
#> PPI  1 0.00003435 0.00003435  0.1421 0.70640  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Symmetry Test Results - Short-run:
#> =======================
#>     Df  Sum of Sq    Mean Sq F value  Pr(>F)  
#> ER   1 0.00017740 0.00017740  0.7337 0.39215  
#> PPI  1 0.00096021 0.00096021  3.9712 0.04688 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

# To get the summary of the symmetry test results in one line, you can use the following code:
imf_example_data %>% kardl(CPI~ER+PPI+asym(ER+PPI)+deterministic(covid)+trend,
                           mode=c(1,0,1,1,0)) %>% symmetrytest() %>% summary()
#> Long-run symmetry tests:
#> 
#> Test for variable:  ER 
#> F-value:  4.484108 , p-value:  0.03475681 
#> Test Decision:  Reject H0 at 5% level. Indicating long-run asymmetry for variable ER. 
#> Hypotheses:
#> H0: - Coef(LK1_ER_POS)/Coef(LK1_CPI) = - Coef(LK1_ER_NEG)/Coef(LK1_CPI)
#> H1: - Coef(LK1_ER_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_ER_NEG)/Coef(LK1_CPI)
#> 
#> Test for variable:  PPI 
#> F-value:  0.1420743 , p-value:  0.7064044 
#> Test Decision:  Fail to Reject H0 at 5% level. Indicating long-run symmetry for variable PPI. 
#> Hypotheses:
#> H0: - Coef(LK1_PPI_POS)/Coef(LK1_CPI) = - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)
#> H1: - Coef(LK1_PPI_POS)/Coef(LK1_CPI) ≠ - Coef(LK1_PPI_NEG)/Coef(LK1_CPI)
#> 
#> 
#> _____________________________
#> Short-run symmetry tests:
#> 
#> Test for variable:  ER 
#> F-value:  0.7336693 , p-value:  0.3921503 
#> Test Decision:  Fail to Reject H0 at 5% level. Indicating short-run symmetry for variable ER. 
#> Hypotheses:
#> H0: Coef(D0.d.ER_POS) = Coef(D0.d.ER_NEG) + Coef(D1.d.ER_NEG)
#> H1: Coef(D0.d.ER_POS) ≠ Coef(D0.d.ER_NEG) + Coef(D1.d.ER_NEG)
#> 
#> Test for variable:  PPI 
#> F-value:  3.971236 , p-value:  0.04688462 
#> Test Decision:  Reject H0 at 5% level. Indicating short-run asymmetry for variable PPI. 
#> Hypotheses:
#> H0: Coef(D0.d.PPI_POS) + Coef(D1.d.PPI_POS) = Coef(D0.d.PPI_NEG)
#> H1: Coef(D0.d.PPI_POS) + Coef(D1.d.PPI_POS) ≠ Coef(D0.d.PPI_NEG)
#> 
```
