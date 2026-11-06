# json to tfrmt

Reader to read JSON files/objects into tfrmt objects

## Usage

``` r
json_to_tfrmt(path = NULL, json = NULL)
```

## Arguments

- path:

  location of the json file to read in

- json:

  json object to read in. By default this is null. This function will
  read in json object preferentially. So if both a path and a json
  object are supplied the json object will be read in.
