ST558 Project 2 (Tuesday)
================
Mu-Tien, Lee

``` r
library(tibble)
library(knitr)
library(rmarkdown)

# set up table for render 
w <- c(2,3,4,5,6,0)
output_file <- paste(w,".md")
params = lapply(w, FUN=function(x){list(w=x)})
reports <- tibble(output_file, params)

# apply render function to build other 6 files
apply(reports, MARGIN = 1,
      FUN=function(x){
       render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = x[[1]],
       params = x[[2]])
      }
)
```

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d004a1e2dbe.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "2 .md" --to html4 --from markdown_github --output "2 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none 
    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d002b95194b.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "3 .md" --to html4 --from markdown_github --output "3 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none 
    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d006340335f.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "4 .md" --to html4 --from markdown_github --output "4 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none 
    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d00450f2e7d.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "5 .md" --to html4 --from markdown_github --output "5 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none 
    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d002928166e.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "6 .md" --to html4 --from markdown_github --output "6 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none 
    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |....                                                                  |   5%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.......                                                               |  11%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ## 
      |                                                                            
      |...........                                                           |  16%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............                                                       |  21%
    ## label: require package
    ## 
      |                                                                            
      |..................                                                    |  26%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................                                                |  32%
    ## label: data
    ## 
      |                                                                            
      |..........................                                            |  37%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.............................                                         |  42%
    ## label: summarizing data

    ## 
      |                                                                            
      |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |.....................................                                 |  53%
    ## label: plotting data

    ## 
      |                                                                            
      |.........................................                             |  58%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |............................................                          |  63%
    ## label: tree based model

    ## 
      |                                                                            
      |................................................                      |  68%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |....................................................                  |  74%
    ## label: boosting tree model (with options) 
    ## List of 2
    ##  $ cache  : logi TRUE
    ##  $ results: chr "hide"
    ## 
    ## 
      |                                                                            
      |.......................................................               |  79%
    ## label: boosted result

    ## 
      |                                                                            
      |...........................................................           |  84%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |...............................................................       |  89%
    ## label: predicting tree model

    ## 
      |                                                                            
      |..................................................................    |  95%
    ##   ordinary text without R code
    ## 
    ## 
      |                                                                            
      |......................................................................| 100%
    ## label: predicting boosted model

    ## 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS ST558_project2_Monday.utf8.md --to markdown_github --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc2d0079644bc0.md --standalone --table-of-contents --toc-depth 3 --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\default.md" 
    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS "0 .md" --to html4 --from markdown_github --output "0 .html" --standalone --self-contained --highlight-style pygments --template "C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\preview.html" --variable "github-markdown-css:C:\Users\LeeSF\Documents\R\win-library\4.0\rmarkdown\rmarkdown\templates\github_document\resources\github.css" --email-obfuscation none

    ## [1] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/2 .md"
    ## [2] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/3 .md"
    ## [3] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/4 .md"
    ## [4] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/5 .md"
    ## [5] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/6 .md"
    ## [6] "C:/Users/LeeSF/Desktop/2020 fall/ST558/repo/ST558_Project-2/0 .md"
