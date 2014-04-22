Notes on [Advanced R](http://adv-r.had.co.nz/)
----------------------
by Bing He

### [Introudction](http://adv-r.had.co.nz/Introduction.html)
1. Two meta-techniques to learn R:
    * read source code
    * develop a scientific mindset: try & error

### [Data sturcture](http://adv-r.had.co.nz/Data-structures.html)
1. When you attempt to combined different types of atomic vectors, they will be __coerced__ to the most flexible type. Types from least to most flexible are: logic, integer, double, and character.
2. All objects can have arbitrary additional attributes. Attributes can be thought as named list.
3. `structure()` function returns a new object with modified attributes, e.g., `structure(1:10, my_attribute = "This is a vector")`.
4. Remove names of vectors by `unname(x)` or `names(x) = NULL`.
5. In data loading functions in R, you can use `stringsAsFactor=FALSE` to preventing automatic conversion of character to factor.
6. use `dimnames()` to change names of array
7. `abind()` is the generalized version for `cbind()`/`rbind()` for array. `aperm()` is the generalization of `t()` for array.
8. Under the hood, data frame is a list of equal-length vectors. 
9. __You can add a column that is a list/array/matrix to a dataframe, using `I()`, e.g. `dfl <- data.frame(x = 1:3, y = I(list(1:2,1:3,1:4)))`__.

### [Subsetting](http://adv-r.had.co.nz/Subsetting.html)
1. R's subsetting operators are powerful and fast, and mastering them allows you to express complex operations in a succinct fashion few other languages can match.
2. __Matrices and arrays are implemented as vectors with special attributes (like `dim`), you can also subset them with a single vector. In that case they will behave like a vector. Arrarys in R are stored in column-major order.__
3. Data frames possess the characteristics of __both lists and matrices__: if you subset with __a single vector__, they behave like lists; if you subset with two vectors, they behave like matrices. For example, `df[c("col1","col2")]` subsets a data frame as it is a list; and `df[,c("col1","col2")]` subsets a data frame as it is a matrix. __There is an important difference if you select a single column: matrix subsetting simplifies by default, list subsetting does not__. In matrix subsetting, you can use the argument `drop=F` to prevent automatic simplification, i.e., `df[,"col1",drop=F]`.
4. S3 objects are made of atomic vectors, arrays and lists, so you can always use above subsetting techiques to extract a field. For S4 objects, two additional subsetting operators are used `@` (like `$`) and `slot()` (equivalent to `[[`).
5. `upper.tri(matrix)` gives TRUE in upper triangle of matrix.

#### Simplifying vs preserving subsetting
1. Simplifying subsets returns the simplest possible data structure, which is very useful for interactively; preserving subsets returns the output in the same format as the input, which is very useful for programming. 
2. omitting `drop=FALSE` in matrix or data frame subsetting will cause unexpected error when someone pass in a single-column data.
3. There is a very useful table for simplifying v.s. preserving.
    * vector and list: simplifying `[['; perserving `[`
    * __factor__: simplifying `drop=T`, which can __drop any unused levels__
    * array: simplifying `drop=FALSE`
    * data frame: simplifying `x[,1]` or `x[[1]]`; preserving `drop=F` or `x[1]`(i.e., treat data frame as a list.)
4. `x$y` translates to `x[["y",exact=FALSE]]`. 
    * __When you store a column name in a var `var <- "ryl"`, `mtcars$var` won't work. Instead, use `mtcars[[var]]`__
    * `$` does partial matching, that's why there is a `exact=FALSE` when we tranlate `$` into `[[`.
5. Indexing with a blank can be useful in conjunction with assignment because it will preserve the original object class and structure. `mtcars[] <- lapply(mtcars, as.integer)` will return a data frame, while `mtcars <- lapply(mtcars, as.integer)` will return a list.
6. You can use `x <- list(a=1,b=2); x[["b"]] <- NULL` to remove components from a list. You can do this with data.frame to remove columns.
7. __A very nice example for Lookup table__: `sex <- c("m","f","m","m","m","f"); lookup <- c(m = "Male", f="Female")`.  Then do `lookup[x]`.
8. __A very nice example for matching and merging__: `grade <- c(1,2,2,3,1); info <- data.frame(grade = 3:1, desc=c("Excellent", "Good", "Poor")`. There are two ways of matching: `id <- match(grades, infor$grade); info[id,]`. __here `match()` returns the positions of first argument's matches elements in the second argument__. Or use `rownames(info) <- info$grade; info[as.character(grades),]`
9. use `df[setdiff(names(df),"z")]` to keep the columns except for `z`.
10. `subset()` is a shorthand function for subsetting data frames and saves some typing. For example, `subset(mtcars, cyl==4)`.
11. Set operations in R: `intersect(x,y); union(x,y); setdiff(x,y)` and `setdiff(union(x,y),intersect(x,y))` for XOR.

### [Vocabulary](http://adv-r.had.co.nz/Vocabulary.html)
#### Basics
1. `<<-`: if var name is not found in local environment, search parent environment until global environment.
2. `match(x,table)` returns the positions of (first) matches of `x`'s elements in `table`
3. `subset(x,subset,select)` subset matrices/data frames that meet conditions. `x` the matrices/data frame to be subsetted. `subset` logical expression to select elements (1-dim) or rows (higher dim); `select` logical expression to select columns.
    * For example, `data(CO2); subset(CO2,Plant == "Qn1", conc)`. Note `Plant` and `conc` are column names in data frame `CO2`.
4. `with(data,expr)`: a generic function for evaluating an `expr` with the environment constructed from `data`. Here `data` can be an environment, a list, a data frame, or an integer as in `sys.call`.
5. `assign(x,value,pos,envir,inherits)`: assign `value` to `x`. `pos`: where to do the assignment, defaulting to current environment.
6. `get(x,pos,envir,mode)` and `mget(x,pos,envir,mode)`: get value of one variable (`get`) or 0 or more varibales (`mget`) `x` in environment specified by `pos`/`envir`. If `mode` is specified (e.g., double, integer, or function), only values for variables with specified mode will be returned.
7. `all.equal(x,y)` compares R objects `x` and `y` for `near equality`. Note the difference between the following 3 examples, `d45 <- pi*(1/4 + 1:10)`:
    * `all.equal(tan(d45),rep(1,10))` gives `TRUE`.
    * `all.equal(tan(d45),rep(1,10),tolerance=0)` gives `"Mean relative difference: 1.29526e-15"`. Comparing the above two examples, we know that `all.equal()` allows for small difference in numeric results.
    * `all(tan(d45)==rep(1,10))` gives `FALSE`, because `all` is exact match in R.
8. `identical()` can be used to test whether two R objects are exactly the same. It is a safe and good way compared than `==`, `&&`
9. `is.finite()` test for `Inf` in data.
10. `%%` modulas calculator. `%/%` integer division `5%/%2` is `2`.
11. `sign,acos,asin,atan,atan2`; for positive arguments `atan2(y, x) == atan(y/x)`. 
12. `ceiling,floor,round,trunc,signif`. `trunc` truncates the value toward 0.
13. `prod(...,na.rm=FALSE)` returns the product of all values present in its arguments.
14. `cummax,cummin,cumprod,cumsum`, and `diff(x,lag=1,differences=1)` lagged and iterated differences.
15. `pmax` and `pmin` are the element-wise max/min of its arguments.
16. `range()`
17. `rle()`: run length encoding and `inverse.rle()` reconstructs vectors from run length encoding.
18. `missing()` can be used to test whether a value was specified as an argument to a function. 
19. `on.exit(expr = NULL, add = FALSE)`: execute code on exit of function. Typical use for graphic parameter settings: `test <- function(x) {oldpar <- par(frow=c(2,2)); on.exit(par(oldpar))}`
20. `invisible()` return an invisible copy (not print if not assigned) from a function.
21. `isTRUE(expr)` is equivalent to `identical(TRUE, expr)`
22. `xor(x,y)`
23. `setequal(x,y)`
24. `sweep(array, margin, stats, fun)` sweep a summary stats as fun defined to the margin of the array.
25. `data.matrix` converts all columns in a data frame into numeric mode and save as a matrix.
26. `rep,rep.int,rep_len`: `rep.int(x,times)` replicate vector `x` `times` times. `rep_len(x,length.out)` replicate vector `x` until its length reaches `length.out`.
27. `seq,seq_along,seq_len,seq.int`: `seq_along(x)` and `seq_len(length.out)` generates integers starting from 1 to the `length(x)` or `length.out`.
28. `rev(x)` returns a reversed version of its argument
29. `sample(x,size,replace,prob)`: `x` vector to be sampled from; `size` numbers of elements to be sampled.
30. `choose(n,k)` returns binomial coefficients.
31. `unlist` returns a vector contains all atomic components in the list
32. `split(x, f, drop = FALSE, ...)`: split `x` according to factor `f`, `drop=T` to drop levels in `f` that does not occur.
33. `expand.grid` create a data frame that have all the combinations of provided vector/factor. For example, `expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50), sex = c("Male","Female"))`.
34. `next/break`
35. `switch(EXPR, alternatives)` if `EXPR` is character/string, alternatives are named and matched by names; if `EXPR` is integer, alternatives can be without names and are matched by order of occurence. `switch` can be used within function.
36. `ISOdate, ISOdatetime, strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "%a, %d %b %Y %H:%M:%S %z")` convert string to date or backwards. `difftime` creates time intervals,`julian,months,quarters,weekdays` extracts julian time, month, quarter, and weekday of a date. __Julian time is number of days since origin__.

#### String
37. `agrep(pattern,x)` approximate string match which finds matches of `x`'s element to `pattern` using edit distance.
38. String match:
    * `grep(x,pattern,value=FALSE)` returns the incides of the elements of `x` to `pattern`; 
    * `grep(x,pattern,value=TRUE)` returns the character vectors containing the matched elements.
    * `grepl` returns a logical vector
39. `sub/gsub(x,pattern,replacement)` substitute matches in `x` to `pattern` with `replacement`. `gsub()` is global substitution (all matches); `sub` is first match. 
    * use `perl=TRUE` to enable perl-style regular expression.
40. `strsplit(x,split,perl)` split each element in `x` by `split`. The return value is `list`, you can use `unlist()` to transform the returned value to a vector.
41. `chartr(old,new,x)` translate each character in `x` from `old` to `new`. For example, `chartr("iXs", "why", x)`.
42. `nchar` num of characters
43. `library(stringr)`

#### Statistics
44. For `factor`, `levels,nlevels,reorder,relevel`; `cut(x,breaks)` cuts `x` according to `breaks` and return a factor.
42. `findInterval(x,vec)`: `vec` is vector of increasing numbers, representing intervals; `i <- findInterval(x,vec)`: `v[i[j]] <= x[j] < v[i[j+1]]`
43. `aperm(a,perm)` transpose an array by permuting its dimensions.
44. `duplicated(x)` returns a logical vector with the same length of `x`: `TRUE`indicates this element is the same as some element with smaller subscript. `x[!duplicated(x)]` extracts the unique values.
45. `ftable()` flat contigency table
46. `merge(x,y,by=)` merge `x` and `y` by common columns or rownames
47. `fitted, predict, resid, rstandard, influence.measures, rstudent(),hat, deviance`
48. `logLik(object, REML=FALSE)` extract log likelihood from a fitted model, for example, you can extract log-likelihood from `glm()`
49. `I()` treat as "as is"
50. `anova,coef,confint,vcov`; `contrasts` sets the constrasts associated with a factor.
52. `crossprod, tcrossprod`: ` t(x) %*% y (crossprod)` or `x %*% t(y) (tcrossprod)`
53. `%o%` outer product for matrices/arrays. 
54. `outer(month.abb, 1999:2003, FUN = "paste")`.
55. `rcond` returns conditional number of a matrix

#### Help
51. `apropos(what)` find objects by partial match of names. For example, `apropos("../test")` returns test functions in R.
56. `exists` look for an R object with given name
57. `require` is the same as `library`, but `require` is designed for use within function. For non-existing packages, `require` throws a warning, while `library` throws an error.
58. `RSiteSearch,citation,demo,example` and `vignette("grid")`
59. `help.search("linear models")`

#### Debugging and error handling
60. `options(error = recover)` when error occurs, automatically list the current active function calls. By selecting a function call, this allows user to browse directly on the function call; can also be called with `recover()`. After choosing an active function call, then it behaves just like `browser()`.
61. `geterrmessage()` gives the last error.
62. `warning("test it")` generates a warning that corresponds to its argument
63. `stop("error here!\n")` stop current computing
64. `message()` generates a message according to its argument
65. __`tryCatch()` evaluates the expression and exception handlers__. [Here](http://mazamascience.com/WorkingWithData/?p=912) is a nice post about error-handlering in R, where the following codes are extracted:
```R
result = tryCatch({
        expr
}, warning = function(w) {
        warning-handler-code
}, error = function(e) {
     error-handler-code
}, finally = {
        cleanup-code
}
```
66. `try(expr)` evaluates an expression and allow user's code to handle exception. Returned value is the value of the expr if no error, but an invisible object containing the error. Can be used in `if` to allow user to handler different exception.

67. `dput` write an R object (e.g., like a functioin) to ASCII file.
68. `format` an R object for pretty printing. Options `trim = FALSE, digits = NULL, nsmall = 0L, justify = c("left", "right", "centre", "none")`: trim spaces, digits, number of decimals, alignment (justify).
69. `sink` `capture.output` evaluate expression and write the output to a file
70. `count.fields` count the number of fields in each row of the file with `sep`
71. `read.fwf()` read fixed width field data
72. `readLines(con,n)` read n lines from the connection con
73. `readRDS(file="")` and `saveRDS(object, file)` read and write a single R object. Useful compared to `load` as the read in data can be renamed `data2 <- readRDS("data.RData"`
74. `list.files() list.dirs()`
75. Given a full path as input, `basename` removes the path up to the final file; `dirname` returns the path (excluding the final file).
76. `file_ext` returns the file extension in the directory
77. `file.path()` concatenate the string arguments separating by `/`
78. `path.expand` expandes a relative path to full path. `path.expand("~/foo")` gives `[1] "C:\\Users\\Bing He\\Documents/foo"`
79. `normalizePaht(x,winslash="//")` express file path in a platform-friendly way. For example, change the backward slash to forward slash to be compatible with windows platform.
80. `file.choose()` will promot a file selection GUI window
81. `file.copy, file.create, file.remove, file.rename, dir.create`
82. `file.exists, file.info`
83. `tempdir, tempfile` returns a path and name appropriate for temporary directory and file.
84. `download.file(url,destfile)` download a file from Internet to the destination file `destfile`

### [Functions](http://adv-r.had.co.nz/Functions.html)
#### Function Basics
1. Function is also an R object just like any other R object. There are three components of a function: `body()`, `formals()` (argument list), and `environment()`. Function can have user-defined attributes. For example, you can set the `class()` and add a custom `print()` method.
2. Primitive functions call C code directly with `.Primitive()` and contain no R code. Primitive functions are only found in the `base` R package.
3. Use `ls("package:base", all = TRUE)` to list all objects in the base package.

#### Lexical scoping
1. Four principles of lexical scoping in R
    * Name masking
    * Functions v.s. variables
    * a fresh start
    * Dynamic lookup
2. Lexical scoping looks up symbol values based on how functions were nested when they were defined, not how they are nested when they are called. 
3. First look inside the current function, then where that function was defined, and so on, all the way up to the global environment, and then on to other loaded packages. 
4. closures: functions created by other functions. It is easy to create a function using a user-defined function, just return the function name. This function will preserve the environment in which it was defined.
5. Looking up functions are the same as looking up for variable values. When R looks up a name where it is obvious you want a function, R will ignore non-function objects.
6. Every time a function is called, a new environment is created to host execution. Each invocation of one function is completely independent. 
7. Lexical scoping determines where to look up variable values. When the function is run, R looks for values, not when it's created.
8. __You want to make a function self-contained, so that its output only depends on its arguments__. You can use `findGlobals()` function from package `codetools`, which lists all the external dependencies of a function.

#### Every operation is a function call
1. "everything that exists in an object; everything that happens is a function call." John Chambers
2. backtick allows you refer to functions or variables that have otherwise reserved or illegal names. For example, `x[3]` is equivalent to ```[`(x,3)``
3. It is very useful to combined `sapply()/lapply()` with '+' and '['. For example,`x <- list(1:10,4:9,10:15); lappy(x,"[",2)`; this is to get the 2nd element of each component of the list.

#### Calling functions
1. When calling a funciton, we can specify the arguments by name, by position, and and by partial names. The actual values of arguments are matched to formal arguments, first by exact matching of name, second by partial match of names, and finally by position.
2. When specifying the arguments, named arguments should be after unamed arugments (matched by position).
3. __call a function with an argument list__: `do.call(mean,list(1:10,na.rm=TRUE))`
4. Note you can determine if an argument was supplied or not with the `missing()` inside a function.
5. When specifying default values but it takes a few lines of code to compute, you can set the default value to `NULL` and then inside the funciton, us `is.null()` to test whether the value is given, then add your lines of code for calcualting the default value.

#### Lazy evaluation
1. by default, R function arguments are lazy. The arguments are only evaluated if they are actually used. Use `force` to force evluation.
2. Default values are evaluated inside function. But arguments given function are evaluated in the environment where the function is defined.
3. Taking advantage of lazy evaluation, we can use `!is.null(a) || stop("a is null\n")`

#### Special calls
1. infix functions: most functions in R are "prefix" operators, the name of the function comes before the arguments. You can create infix functions where the function name comes in between its arguments, like `+` or `-`.
2. __User-defined infix functions should start and end with `%`__. Their names can contain any special characters (special characters should be escaped). 
3. Replacement functions act like they modify their arguments in place (actually they still make a local temporary copy), and have the special name "xxx<-". `"second<-" <- function(x,value) { x[2] <- value; x}`. Use `second(x) <- 5L`.
4. Most R objects have copy-on-modify semantics. So modifying a function argument does not change the original value. __There are two important excepting to copy-on-modify semantics: reference classes and environment. These can be modified in place, so extra care is needed when working with them__.
5. pure function: functions that always map the smae input ot hte same output and have no other impact on the workspace.
6. Most base R function are pure functions. Here are some exceptions:
    * `library` loads a package and modifies the search path
    * `setwd, Sys.setenv, Sys.setlocale` change the working dir, environment variables and the locale respectively.
    * others
7. Invisible return value: `f2 <- function() invisible(1); f2(); f2()==1;` and `(f2())` actually prints 1.
8. `on.exit()` the expression inside is called and evluated when the function terminates. Be cautious if you are using multiple `on.exit()` calls, need to set `add = TRUE`. For example, `{old <- setwd(dir); on.exit(setwd(old))}`

### [OO field guide](http://adv-r.had.co.nz/OO-essentials.html)

