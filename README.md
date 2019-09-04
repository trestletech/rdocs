# rdocs (Abandoned)

Exploring the evolution of functions/parameters in base R over time.

This was a crude experiment to figure out if it would be possible to use package sources to infer the functions and their signatures exposed in an R package. 

We compare to [rcheology](https://github.com/hughjonesd/rcheology/) as the "gold standard." Overall, this approach was able to get fairly close to rcheology's performance without the cost of having to install the package and inspect it in a live environment. This is appealing because it would be more computationally viable for a CI approach and you don't have to deal with the headaches of trying to get every package to install (obscure dependencies, platform-dependant, etc.).

This work has been put on hold for now. The reasoning is that the number of functions that people are likely to actually use that have changed in recent versions of R is pretty small. (See [backports](https://github.com/r-lib/backports) for a package that tries to cover all of the motivating changes.) 

The current recomendation is to exercise unit tests in CI on the oldest version of R that you intend to support. Unfortunately, this won't catch errors in your code that aren't covered in unit tests, but it would at least cover some of the code.

## See also

https://github.com/hughjonesd/apicheck for a nascent package trying to do something similar.

## Usage

This was intended to grow into an R package (hence the structure) but it never did. The interesting bits are all RMD files in `inst/`. Start with [https://github.com/trestletech/rdocs/blob/master/inst/parse-rs.Rmd](parse-rs.rmd) (or see the pre-rendered HTML adjacent) and branch into the other RMDs in that directory to do more analysis. 

Alternatively, if you just want the output of this work, see [https://github.com/trestletech/rdocs/blob/master/inst/parsed_rd.rds](parsed_rd.rds) which represents the best I could do at identifying functions and their signature using package sources in base R since version 3.0.0.

```
> head(parsed_rd, n=10)
     ver  pkg          file        fun class    paramName                  placeholder evaluated defunct exported
1  3.0.0 base abbreviate.Rd abbreviate  <NA>    names.arg                         <NA>      TRUE   FALSE    FALSE
2  3.0.0 base abbreviate.Rd abbreviate  <NA>    minlength                            4      TRUE   FALSE    FALSE
3  3.0.0 base abbreviate.Rd abbreviate  <NA>  use.classes                         TRUE      TRUE   FALSE    FALSE
4  3.0.0 base abbreviate.Rd abbreviate  <NA>          dot                        FALSE      TRUE   FALSE    FALSE
5  3.0.0 base abbreviate.Rd abbreviate  <NA>       strict                        FALSE      TRUE   FALSE    FALSE
6  3.0.0 base abbreviate.Rd abbreviate  <NA>       method c("left.kept", "both.sides")      TRUE   FALSE    FALSE
7  3.0.0 base      agrep.Rd      agrep  <NA>      pattern                         <NA>      TRUE   FALSE    FALSE
8  3.0.0 base      agrep.Rd      agrep  <NA>            x                         <NA>      TRUE   FALSE    FALSE
9  3.0.0 base      agrep.Rd      agrep  <NA> max.distance                          0.1      TRUE   FALSE    FALSE
10 3.0.0 base      agrep.Rd      agrep  <NA>        costs                         NULL      TRUE   FALSE    FALSE
```
