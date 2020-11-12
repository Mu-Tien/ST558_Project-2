automation code
================
Mu-Tien, Lee

``` r
library(tibble)
library(knitr)
library(rmarkdown)
```

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Tuesday.md",
       params =list(w=2))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Tuesday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Tuesday.md --to html4 --from gfm --output Tuesday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Wednesday.md",
       params =list(w=3))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Wednesday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Wednesday.md --to html4 --from gfm --output Wednesday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Thursday.md",
       params =list(w=4))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Thursday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Thursday.md --to html4 --from gfm --output Thursday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Friday.md",
       params =list(w=5))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Friday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Friday.md --to html4 --from gfm --output Friday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Saturday.md",
       params =list(w=6))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Saturday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Saturday.md --to html4 --from gfm --output Saturday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Sunday.md",
       params =list(w=0))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |...                                                                   |   5%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.......                                                               |  10%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..........                                                            |  14%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.............                                                         |  19%
    ## label: require package
    ##   |                                                                              |.................                                                     |  24%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |....................                                                  |  29%
    ## label: data
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...........................                                           |  38%
    ## label: summarizing data

    ##   |                                                                              |..............................                                        |  43%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................                                     |  48%
    ## label: plotting data

    ##   |                                                                              |.....................................                                 |  52%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................                              |  57%
    ## label: tree based model

    ##   |                                                                              |...........................................                           |  62%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: boosting tree model (with options) 
    ## List of 1
    ##  $ results: chr "hide"
    ## 
    ##   |                                                                              |..................................................                    |  71%
    ## label: boosted result

    ##   |                                                                              |.....................................................                 |  76%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........................................................             |  81%
    ## label: predicting tree model

    ##   |                                                                              |............................................................          |  86%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................................       |  90%
    ## label: predicting boosted model

    ##   |                                                                              |...................................................................   |  95%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |......................................................................| 100%
    ## label: unnamed-chunk-1
    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Sunday.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS Sunday.md --to html4 --from gfm --output Sunday.html --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none --metadata pagetitle=PREVIEW
