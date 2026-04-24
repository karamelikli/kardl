# Merge Lists with Priority to the First Argument

The first list of values takes precedence. When both lists have items
with the same names, the values from the first list will be applied. In
merging the two lists, priority is given to the left list, so if there
are overlapping items, the corresponding value from the left list will
be used in the merged result.

## Usage

``` r
lmerge(first, second, ...)
```

## Arguments

- first:

  The first list

- second:

  The second list

- ...:

  Additional lists to merge

## Value

A merged list with unique names, prioritizing values from the first list
in case of name conflicts.

## Details

The `lmerge()` function is designed to merge multiple lists while giving
precedence to the values in the first list. This function is
particularly useful when you want to combine settings or parameters from
multiple sources while ensuring that the primary source (the first list)
takes priority over others.

For right merge, a user can simply swap the order of the lists to give
priority to the second list. For example, `lmerge(b, a)` will prioritize
values from list `b` over those in list `a`.

## See also

[`append`](https://rdrr.io/r/base/append.html)

## Examples

``` r

a<-list("a"="first a","b"="second a","c"=list("w"=12,"k"=c(1,3,6)))
b<-list("a"="first b","b"="second b","d"=14,"e"=45)
myMerged<- lmerge(a,b)
print(unlist(myMerged))
#>          a          b        c.w       c.k1       c.k2       c.k3          d 
#>  "first a" "second a"       "12"        "1"        "3"        "6"       "14" 
#>          e 
#>       "45" 

# for right merge
myMerged<- lmerge(b,a)
print(unlist(myMerged))
#>          a          b          d          e        c.w       c.k1       c.k2 
#>  "first b" "second b"       "14"       "45"       "12"        "1"        "3" 
#>       c.k3 
#>        "6" 

# for more than two lists
myMerged<- lmerge(a,b,c("v1"=11,22,3,"v5"=5))
print(unlist(myMerged))
#>          a          b        c.w       c.k1       c.k2       c.k3          d 
#>  "first a" "second a"       "12"        "1"        "3"        "6"       "14" 
#>          e         v1                               v5 
#>       "45"       "11"       "22"        "3"        "5" 

# for more than two lists with nested lists
m2<-list("m1"="kk2","m1.2.3"=list("m1.1.1"=333,"m.1.4"=918,"m.1.5"=982,"m.1.6"=981,"m.1.7"=928))
m3<-list("m1"="kk23","m2.3"=2233,"m1.2.4"=list("m1.1.1"=333444,"m.1.5"=982,"m.1.6"=91,"m.1.7"=928))
a<-c(32,34,542,"k"=35)
b<-c(65,"k"=34)

h1<-lmerge(a, m2)
print(unlist(h1))
#>                                                       k            m1 
#>          "32"          "34"         "542"          "35"         "kk2" 
#> m1.2.3.m1.1.1  m1.2.3.m.1.4  m1.2.3.m.1.5  m1.2.3.m.1.6  m1.2.3.m.1.7 
#>         "333"         "918"         "982"         "981"         "928" 

h2<-lmerge(a,b,m2,m3,list("m1.1"=4))
print(unlist(h2))
#>                                                       k               
#>          "32"          "34"         "542"          "35"          "65" 
#>            m1 m1.2.3.m1.1.1  m1.2.3.m.1.4  m1.2.3.m.1.5  m1.2.3.m.1.6 
#>         "kk2"         "333"         "918"         "982"         "981" 
#>  m1.2.3.m.1.7          m2.3 m1.2.4.m1.1.1  m1.2.4.m.1.5  m1.2.4.m.1.6 
#>         "928"        "2233"      "333444"         "982"          "91" 
#>  m1.2.4.m.1.7          m1.1 
#>         "928"           "4" 
```
