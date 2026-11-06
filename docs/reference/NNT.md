# Calculate the number needed to treat (NNT)

This function calculates the number needed to treat (\\(NTT\\)) using
event data or effect sizes (such as Cohen's \\(d\\) or Hedges' \\(g\\)).

## Usage

``` r
NNT(d, CER, event.e, n.e, event.c, n.c, names, method)
```

## Arguments

- d:

  A single numeric or concatenated vector of numerics representing the
  effect size expressed as Cohen's \\(d\\) or Hedges' \\(g\\). If this
  is the only parameter specified in the function, the method by Kraemer
  and Kupfer is used automatically to calculate \\(NNT\\)s.

- CER:

  The control group event ratio. Furukawa's method (Furukawa &
  Leucht, 2011) to calculate `NNT`s from `d` requires that the assumed
  response ("event") ratio in the control group
  (\\(\frac{n\_{responders}}{N\_{total}}\\)) is specified. The CER can
  assume values from 0 to 1. If a value is specified for `CER`,
  Furukawa's method is used automatically. Argument `method` has to be
  set to `"KraemerKupfer"` to override this.

- event.e:

  Single number or numeric vector. The number of (favourable) events in
  the experimental group.

- n.e:

  Single number or numeric vector. The number participants in the
  experimental group.

- event.c:

  Single number or numeric vector. The number of (favourable) events in
  the control group.

- n.c:

  Single number or numeric vector. The number of participants in the
  control group.

- names:

  Optional. Character vector of equal length as the vector supplied to
  `d` or `event.e` containing study/effect size labels.

- method:

  The method to be used to calculate the NNT from `d`. Either
  `"KraemerKupfer"` for the method proposed by Kraemer and Kupfer (2006)
  or `"Furukawa"` for the Furukawa method (Furukawa & Leucht, 2011).
  Please note that the Furukawa's method can only be used when `CER` is
  specified.

## Details

This function calculates the number needed to treat (\\(NNT\\)) from
effect sizes (Cohen's \\(d\\) and Hedges' \\(g\\)) or, alternatively,
from raw event data.

Two methods to calculate the \\(NTT\\) from `d` are implemented in this
function.

- The method by **Kraemer and Kupfer** (2006), calculates \\(NTT\\) from
  the Area Under the Curve (\\(AUC\\)) defined as the probability that a
  patient in the treatment has an outcome preferable to one in the
  control. This method allows to calculate the NNT directly from `d`
  without any extra variables.

- The method by **Furukawa** calculates the \\(NNT\\) from `d` using a
  reasonable estimate of \\(CER\\), in this context the response rate in
  the control group.

Furukawa's method has been shown to be superior in predicting the
\\(NNT\\) compared to the Kraemer & Kupfer method (Furukawa & Leucht,
2011). If reasonable assumptions can be made concerning the \\(CER\\),
Furukawa's method should therefore be preferred.

When event data is used for the function, the \\(CER\\) and \\(EER\\)
(experimental group event rate) is calculated internally, and the
standard definition of the \\(NTT\\), \\(\frac{1}{EER-CER}\\), is used.

Please note that negative NNT values returned by the function refer to
the number needed to harm (\\(NNH\\)), as the intervention is assumed to
be inferior to the control group treatment based on the effect size data
supplied to the function.

## References

Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019). *Doing
Meta-Analysis in R: A Hands-on Guide*. DOI: 10.5281/zenodo.2551803.
[Chapter 9.1](https://doing-meta.guide/smallstudyeffects.html).

Furukawa, T. A., & Leucht, S. (2011). How to obtain NNT from Cohen's d:
comparison of two methods. *PloS one, 6*(4), e19070.

Kraemer H.C., Kupfer D.J. (2006) Size of treatment effects and their
importance to clinical research and practice. *Biol. Psychiatry 59*:
990–996.

## See also

[`se.from.p`](https://dmetar.protectlab.org/reference/se.from.p.md)

## Author

Mathias Harrer & David Daniel Ebert

## Examples

``` r
# Example 1: Convert Cohen's d using the Kraemer & Kupfer method
d = c(-0.123, 0.234, 0.123, 1.234, 0.12)
NNT(d)
#> Kraemer & Kupfer method used. 
#> Negative NNT values refer to the number needed to harm (NNH) 
#> [1] -14.428364   7.609168  14.428364   1.620477  14.788175

# Example 2: Convert Cohen's d using the Furukawa method
d = c(-0.123, 0.234, 0.123, 1.234, 0.12)
CER = c(0.42, 0.35, 0.26, 0.21, 0.23)
NNT(d, CER)
#> Furukawa & Leucht method used. 
#> Negative NNT values refer to the number needed to harm (NNH) 
#> [1] -21.110785  11.128241  24.147924   2.195288  26.309816

# Example 3: Convert event data
NNT(event.e = 10, event.c = 20, n.e = 200, n.c = 200)
#> [1] 20
```
