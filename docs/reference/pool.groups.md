# Pool the results of two treatment arms

This function allows to pool the mean, standard deviation and sample
size of two experimental groups of a study. Results of two treatment
arms may be pooled to mitigate the risk of a unit-of-analysis error and
to avoid "double-counting" in meta-analyses in which studies with more
than two experimental groups are included.

## Usage

``` r
pool.groups(n1, n2, m1, m2, sd1, sd2)
```

## Arguments

- n1:

  Numeric vector or single number. The number of participants in arm 1.

- n2:

  Numeric vector or single number. The number of participants in arm 2.

- m1:

  Numeric vector or single number. The mean in arm 1.

- m2:

  Numeric vector or single number. The mean in arm 2.

- sd1:

  Numeric vector or single number. The standard deviation (\\(SD\\)) in
  arm 1.

- sd2:

  Numeric vector or single number. The standard deviation (\\(SD\\)) in
  arm 2.

## Value

Returns a `data.frame` containing the following columns:

- `Mpooled`: The pooled mean of both groups

- `SDpooled`: The pooled standard deviation of both groups

- `Npooled`: The pooled number of participants of both groups

## Details

Many randomized-controlled trials do not only include a single
intervention and control group, but compare the effect of two or more
interventions to a control group. It might be tempting in such a
scenario to simply include all the comparisons between the intervention
groups and control within a study into one meta-analysis. Yet,
researchers should abstain from this practice, as this would mean that
the control group is used twice for the meta-analysis, thus
“double-counting” the participants in the control group. This results in
a **unit-of-analysis error**, as the effect sizes are correlated, and
thus not independent, but are treated as if they would stem from
independent samples.

One way to deal with this is to synthesize the results of the
intervention arms to obtain one single comparison to the control group.
Despite its practical limitations (sometimes, this would mean
synthesizing the results from extremely different types of
interventions), this procedure does avoid the unit-of-analysis error
problem.

To synthesize the pooled effect size data (pooled mean, standard
deviation and \\(N\\)), the following formulae are used:

\$\$N\_{pooled}=N_1+N_2\$\$
\$\$M\_{pooled}=\frac{N_1M_1+N_2M_2}{N_1+N_2}\$\$ \$\$SD\_{pooled} =
\sqrt{\frac{(N_1-1)SD^{2}\_{1}+
(N_2-1)SD^{2}\_{2}+\frac{N_1N_2}{N_1+N_2}(M^{2}\_1+M^{2}\_2-2M_1M_2)}
{N_1+N_2-1}}\$\$

**What should I do when a study has more than two intervention groups?**

If a study has more than two intervention groups you want to synthesize
(e.g. four arms, with three distinct intervention arms), you can pool
the effect size data for the first two interventions, and then
synthesize the pooled data you calculated with the data from the third
group.

## References

Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019). *Doing
Meta-Analysis in R: A Hands-on Guide*. DOI: 10.5281/zenodo.2551803.
[Chapter 13.9](https://doing-meta.guide/i.html)

## See also

[`se.from.p`](http://dmetar.protectlab.org/reference/se.from.p.md)

## Author

Mathias Harrer & David Daniel Ebert

## Examples

``` r
pool.groups(n1 = 50, n2 = 56,
    m1 = 7.83, m2 = 8.32,
    sd1 = 3.52, sd2 = 2.25)
#>    Mpooled SDpooled Npooled
#> 1 8.088868 2.914511     106
```
