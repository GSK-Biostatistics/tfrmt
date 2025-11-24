# Print to ggplot

Print to ggplot

## Usage

``` r
print_to_ggplot(tfrmt, .data, ...)
```

## Arguments

- tfrmt:

  tfrmt object that will dictate the structure of the ggplot object

- .data:

  Data to style in order to make the ggplot object

- ...:

  Inputs to geom_text to modify the style of the table body

## Value

a stylized ggplot object

## Examples

    # Create data
    risk<-tibble(time=c(rep(c(0,1000,2000,3000),3)),
                label=c(rep("Obs",4),rep("Lev",4),rep("Lev+5FU",4)),
                value=c(630,372,256,11,620,360,266,8,608,425,328,14),
                param=rep("n",12))

    table<-tfrmt(
     label = label ,
     column = time,
     param = param,
     value = value) %>%
      print_to_ggplot(risk)

    table

![Simple table to stack with a
KM-plot](https://raw.githubusercontent.com/GSK-Biostatistics/tfrmt/master/images/example_print_to_ggplot.png)
