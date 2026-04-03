# IMF Example Data

This is an example data set used for testing purposes. It contains
monthly data on exchange rates, consumer price index, producer price
index, and a dummy variable for the COVID-19 pandemic for Turkey from
January 1985 to February 2024.

## Usage

``` r
imf_example_data
```

## Format

A data frame with 470 rows and 4 variables:

- ER:

  Numeric. Exchange rate of Turkey.

- CPI:

  Numeric. CPI of Turkey.

- PPI:

  Numeric. PPI of Turkey.

- covid:

  Integer.Covid19 dummy variable.

## Details

These data obtained from imf.data package. The sample data is not
updated and obtained by following codes:

`install.packages("imf.data")`  
[`library("imf.data")`](https://pedrobtz.github.io/imf.data/)  
`IFS <- load_datasets("IFS")`

- **PCPI_IX** Prices, Consumer Price Index, All items, Index  

- **AIP_IX** Economic Activity, Industrial Production, Index  

- **ENDE_XDC_USD_RATE** Exchange Rates, Domestic Currency per U.S.
  Dollar, End of Period, Rate

` trdata<-IFS$get_series(freq = "M", ref_area = "TR", indicator = c("PCPI_IX","AIP_IX","ENDE_XDC_USD_RATE"),start_period = "1985-01",end_period = "2024-02")`  
` PeriodRow<-trdata[,1]`  
` trdata[,1]<-NULL`  
` colnames(trdata)<-c("ER","CPI","PPI")`  
` trdata<-log(as.data.frame(lapply(trdata, function(x) as.numeric(x))))`  
` rownames(trdata)<-PeriodRow`

**Inserting covid dummy variable**

` trdata<-cbind(trdata,covid=0)`  
` trdata[420:470,4]<-1`

## See also

[`load_datasets`](https://pedrobtz.github.io/imf.data/reference/load_datasets.html)

## Examples

``` r
data(imf_example_data)
head(imf_example_data)
#>                ER       CPI      PPI covid
#> 1985-01 -7.698074 -4.895304 3.514587     0
#> 1985-02 -7.634863 -4.862836 3.393818     0
#> 1985-03 -7.618904 -4.816122 3.492322     0
#> 1985-04 -7.576649 -4.807842 3.452126     0
#> 1985-05 -7.545791 -4.784120 3.548855     0
#> 1985-06 -7.531908 -4.791754 3.275258     0
```
