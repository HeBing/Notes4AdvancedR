Notes4AdvancedR
===============

Here are the notes I've made while reading Advanced R Programming by Hadley Wickham

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
26. 
