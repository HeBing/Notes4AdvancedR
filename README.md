Notes on [Advanced R](http://adv-r.had.co.nz/)
----------------------
by Bing He

### [Introudction](http://adv-r.had.co.nz/Introduction.html)
1. Two meta-techniques to learn R:
    * read source code
    * develop a scientific mindset: try & error

### [Data sturcture](http://adv-r.had.co.nz/Data-structures.html)
1. When you attempt to combined different types of atomic vectors, they will be *_coerced_* to the most flexible type. Types from least to most flexible are: __logic, integer, double, and character__.
2. All objects can have arbitrary additional attributes. Attributes can be thought as named list. (BH: I found that for objects of base types, their attributes do not contain class; though their class can be accessed through `class()`. For S3 objects, the attributes contain a named element `class`).
3. `structure()` function returns a new object with modified attributes, e.g., `structure(1:10, my_attribute = "This is a vector")`. (BH: replace `my_attribute` with `class` and this is the way to create an S3 object of the class type).
4. Remove names of vectors by `unname(x)` or `names(x) = NULL`.
5. In data loading functions in R, you can use `stringsAsFactor=FALSE` to preventing automatic conversion of character to factor. (BH: it turns out that setting this arg to FALSE is very helpful/even necessarily when reading tables.)
6. use `dimnames()` to change names of array.
7. `abind()` is the generalized version for `cbind()`/`rbind()` for array. `aperm()` is the generalization of `t()` for array.
8. Under the hood, data frame is a list of equal-length vectors. 
9. __You can add a column that is a list/array/matrix to a dataframe, using `I()`, e.g. `dfl <- data.frame(x = 1:3, y = I(list(1:2,1:3,1:4)))`__.

### [Subsetting](http://adv-r.had.co.nz/Subsetting.html)
1. R's subsetting operators are powerful and fast, and mastering them allows you to express complex operations in a succinct fashion few other languages can match.
2. __Matrices and arrays are implemented as vectors with special attributes (like `dim`), you can also subset them with a single vector. In that case they will behave like a vector. Arrarys in R are stored in column-major order.__
3. Data frames possess the characteristics of __both lists and matrices__: if you subset with __a single vector__, they behave like lists; if you subset with two vectors, they behave like matrices. For example, `df[c("col1","col2")]` subsets a data frame as it is a list; and `df[,c("col1","col2")]` subsets a data frame as it is a matrix. __There is an important difference if you select a single column: matrix subsetting simplifies by default, list subsetting does not__. In matrix subsetting, you can use the argument `drop=F` to prevent automatic simplification, i.e., `df[,"col1",drop=F]` (use `drop=T` is good practice especially matrix computing is involved).
4. S3 objects are made of atomic vectors, arrays and lists, so you can always use above subsetting techiques to extract a field. For S4 objects, two additional subsetting operators are used `@` (like `$`) and `slot()` (equivalent to `[[`). (BH: `slot()` takes in two arguments, name of the S4 object and name of the slot).
5. `upper.tri(matrix)` gives TRUE in upper triangle of matrix.

#### Simplifying vs preserving subsetting
1. Simplifying subsets returns the simplest possible data structure, which is very useful interactively; preserving subsets returns the output in the same format as the input, which is very useful for programming. 
2. omitting `drop=FALSE` in matrix or data frame subsetting will cause unexpected error when someone pass in a single-column data.
3. There is a very useful table for simplifying v.s. preserving.
    * vector and list: simplifying `[['; perserving `[`
    * __factor__: simplifying `drop=T`, which can __drop any unused levels__ (BH: good one to know!)
    * array: simplifying `drop=T`
    * data frame: simplifying `x[,1]` or `x[[1]]`; preserving `drop=F` or `x[1]`(i.e., treat data frame as a list.)
4. `x$y` translates to `x[["y",exact=FALSE]]`. 
    * __When you store a column name in a var `var <- "ryl"`, `mtcars$var` won't work. Instead, use `mtcars[[var]]`__
    * `$` does partial matching, that's why there is a `exact=FALSE` when we tranlate `$` into `[[`.
5. Indexing with a blank can be useful in conjunction with assignment because it will preserve the original object class and structure. `mtcars[] <- lapply(mtcars, as.integer)` will return a data frame, while `mtcars <- lapply(mtcars, as.integer)` will return a list.
6. You can use `x <- list(a=1,b=2); x[["b"]] <- NULL` to remove components from a list. You can do this with data.frame to remove columns.
7. __A very nice example for Lookup table__: `sex <- c("m","f","m","m","m","f"); lookup <- c(m = "Male", f="Female")`.  Then do `lookup[x]`.
8. __A very nice example for matching and merging__: `grade <- c(1,2,2,3,1); info <- data.frame(grade = 3:1, desc=c("Excellent", "Good", "Poor")`. There are two ways of matching: `id <- match(grades, info$grade); info[id,]`. __here `match()` returns the positions of first argument's matches elements in the second argument__. Or use `rownames(info) <- info$grade; info[as.character(grades),]`
9. use `df[setdiff(names(df),"z")]` to keep the columns except for `z`.
10. `subset()` is a shorthand function for subsetting data frames and saves some typing. For example, `subset(mtcars, cyl==4)`.
11. Set operations in R: `intersect(x,y); union(x,y); setdiff(x,y)` and `setdiff(union(x,y),intersect(x,y))` for XOR.

### [Vocabulary](http://adv-r.had.co.nz/Vocabulary.html)
#### Basics
1. `<<-`: if var name is not found in local environment, search parent environment until global environment. (BH: this one is useful when creating RC reference class object.)
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
8. `identical()` can be used to test whether two R objects are exactly the same. It is a safe and good way compared to `==`, `&&`
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
19. `on.exit(expr = NULL, add = FALSE)`: execute code on exit of function. Typical use for graphic parameter settings: `test <- function(x) {oldpar <- par(frow=c(2,2)); on.exit(par(oldpar))}`.
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
39. `sub/gsub(pattern,replacement,x)` substitute matches in `x` to `pattern` with `replacement`. `gsub()` is global substitution (all matches); `sub` is first match. 
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
60. `options(error = recover)` when error occurs, automatically list the current active function calls. By selecting a function call, this allows user to browse directly on the function call; can also be called with `recover()`. After choosing an active function call, then it behaves just like `browser()`. [=(BH: this `recover()` seems very helpful!)
61. `geterrmessage()` gives the last error.
62. `warning("test it")` generates a warning that corresponds to its argument
63. `stop("error here!\n")` stop current computing
64. `message()` generates a message according to its argument
65. __`tryCatch()` evaluates the expression and exception handlers__. [Here](http://mazamascience.com/WorkingWithData/?p=912) is a nice post about error-handlering in R, where the following codes are extracted:
66. `try(expr)` evaluates an expression and allow user's code to handle exception. Returned value is the value of the expr if no error, but an invisible object containing the error. Can be used in `if` to allow user to handler different exception.
```r
# error handling block
test = try(length(intersect(motifSite[current,start:end], peak[i,start:end])))
if(class(test) == "try-error") {
  browser()
}
```

#### files
67. `dput` write an R object (e.g., like a functioin) to ASCII file.
68. `format` an R object for pretty printing. Options `trim = FALSE, digits = NULL, nsmall = 0L, justify = c("left", "right", "centre", "none")`: trim spaces, digits, number of decimals, alignment (justify).
69. `sink` `capture.output` evaluate expression and write the output to a file
70. `count.fields` count the number of fields in each row of the file with `sep`
71. `read.fwf()` read fixed width field data
72. `readLines(con,n)` read n lines from the connection con
73. `readRDS(file="")` and `saveRDS(object, file)` read and write a single R object. Useful compared to `load` as the read in data can be renamed `data2 <- readRDS("data.RData")`
74. `list.files() list.dirs()`
75. Given a full path as input, `basename` removes the path up to the final file; `dirname` returns the path (excluding the final file).
76. `file_ext` returns the file extension in the directory
77. `file.path()` concatenate the string arguments separating by `/`
78. `path.expand` expandes a relative path to full path. `path.expand("~/foo")` gives `[1] "C:\\Users\\Bing He\\Documents/foo"`
79. `normalizePath(x,winslash="//")` express file path in a platform-friendly way. For example, change the backward slash to forward slash to be compatible with windows platform.
80. `file.choose()` will promot a file selection GUI window
81. `file.copy, file.create, file.remove, file.rename, dir.create`
82. `file.exists, file.info`
83. `basename` gives the file name without paths. 
84. `tempdir, tempfile` returns a path and name appropriate for temporary directory and file.
85. `download.file(url,destfile)` download a file from Internet to the destination file `destfile`

### [Functions](http://adv-r.had.co.nz/Functions.html)
#### Function Basics
1. Function is also an R object just like any other R object. There are three components of a function: `body()`, `formals()` (argument list), and `environment()`. Function can have user-defined attributes. For example, you can set the `class()` and add a custom `print()` method (BH: it is actually adding a method associated with a user-created class to the S3 generic function `print`, see OO field guide).
2. Primitive functions call C code directly with `.Primitive()` and contain no R code. Primitive functions are only found in the `base` R package. Use `is.primitive()` to check whether a function is or not.
3. Use `ls("package:base", all = TRUE)` to list all objects in the base package.

#### Lexical scoping
0. Lexical scoping looks up symbol values based on how function were nested when they were created (i.e., their enclosing environments), but not how they were nested when they were called (calling environments). So with lexical scoping, we do not need to know how the function is called but only where the function is defined. 
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
7. Lexical scoping determines where to look up variable values. When the function is run, R looks for values, not when it's created (dynamic lookup).
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
5. pure function: functions that always map the same input to the same output and have no other impact on the workspace.
6. Most base R function are pure functions. Here are some exceptions:
    * `library` loads a package and modifies the search path
    * `setwd, Sys.setenv, Sys.setlocale` change the working dir, environment variables and the locale respectively.
    * others
7. Invisible return value: `f2 <- function() invisible(1); f2(); f2()==1;` and `(f2())` actually prints 1.
8. `on.exit()` the expression inside is called and evluated when the function terminates. Be cautious if you are using multiple `on.exit()` calls, need to set `add = TRUE`. For example, `{old <- setwd(dir); on.exit(setwd(old))}`

### [OO field guide](http://adv-r.had.co.nz/OO-essentials.html)
0. base types
  * base types are internal C struct that describes how that object is stored in memory.
  * common base types include atomic vectors and lists, functions, environments, and other exotic objects like names, calls, and promises.
  * determine an object's base type using `typeof()`
  * method dispatch for base types are written in C (`SWITCH(TYPE)`)
1. There are 3 OO systems in R: S3, S4, and RC.
2. S3
  * S3: generic-function OO. A generic function decides which method to call based on the classes of the input arguments. It is the most commonly used system.
  * Use `is.object(x) $ !isS4(x)` to determine whether an object is S3 or not
  * Check source code for a call to `UseMethod()` to determine whether a function is a S3 generic function.
  * We can use `methods(generic.function,class)` to find out methods belong to a generic function using the 1st args, and list all generic functions that have a method for a given class using the 2nd args.
  * In S3, defining classes and creating objects are accomplished in one simple step, i.e., `foo <- structure(list(),class="foo")` or `foo <- list(); class(foo) <- "foo"`.
  * Creating S3 generic function by `f <- function(x) UseMethod("f"); f.a <- function(x) "Class a"; a<- structure(list(),class = "a"); f(a)`
3. S4
  * S4 is more formal than S3. S4 requires formal class definitions. It supports multiple inheritance and multiple dispatches. All S4 related code i stored in `library(methods)`. Bioconductors contain rich body of code in S4.
  * Use `isS4()` to determine whether an object is an S4 object or not.
  * Use `is(x)` to list all classes that an object inherits from (for both S3 and S4 objects)
  * Find the documentation for a class with `class?className`
  * Use `setClass(name, slots, contains)` to define the representation of a class and use `new()` to create one object of the calss. For example, `setClass("Person",slots = list(name = "character",age="numeric")); alice <- new("Person",name="Alice",age=40)`.
  * access a specific slot of an S4 object by `alice@age` or `slot(alice,"age")`.
  * Use `setGeneric("union")` to create a new generic in S4 or converts an existing function into a generic. Then use `setMethod()` with name of the generic and the method-associated classes, and the function (i.e.method).
4. RC (Reference classes)
  * RC implements message-passing OO. RC objects are mutable (or passed by reference insteat of by value); RC methods belong to objects
  * Use `setRefClass()` to define a class: `Account <- setRefClass("Account",fields=list(balance="numeric"), methods=list(withdraw=function(x){balance <<- balance -x}))`
  * Call method use `myAccount$deposit(100)`
  * use `is(x,"refClass")` to check whether an object is RC or not.
5. (From [Google's R Style Guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)) Comparing S3 and S4 OO fields, S3 is more interactive and flexible, whereas S4 is more formal and rigorous. Use S3 system unless there is a strong reason to use S4. Two primary justification for S3 is
  * to directly use rigorously defined class and objects in C++ code
  * S4 generic method can dispatch on multiple arguments.

### [Style](http://r-pkgs.had.co.nz/style.html)
0. Here I describe the style that I feel comfortable with, combining [Google's R Style Guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml) and Hadley's Style.
1. File names should follow the camelCase and end in ".R".
2. Variable names should be nouns and follow the camelCase, while function names should be verbs and follow the PascalCase.
3. Don't use dot in names. Use underscore only if dates appear in file names.
4. Place spaces around all infix operators.
5. Line maximum is 80 characters.
6. Idention, of course.
7. Curly braces: first one should be at the end of line and 2nd one shoul be at the beginning of a line. An exception is `} else {`
8. Code organization (from Google's R Style Guide):
  * Copyright statement comment
  * Author comment
  * File description comment: purpose, input and output
  * `source()` and `library()` statements
  * Function definitions
  * Executed s tatements, if applicable
  * Use `#-----------------------` to break up your file into chunks
9. Function documentation should be right above the function definition.
  * include at least purpose, args description, input, and output

### [Environments](http://adv-r.had.co.nz/Environments.html)
#### Environments Basics
0. The environment is a data structure that powers scoping. Environments have reference semantics.
1. Create new environments: 
```r
e <- new.env()
e$a <- FALSE
e$b <- "a"
```
2. Every environment has a parent; and the root environment is the enmpty environment. Common environments include: `globalenv()` is the interactive workspace. The parent of this environment is the last library you loaded; `baseenv()` is the environment for base package; `emptyenv()` is the root environment; and `environment()` is the current environment. 
3. `search()` list all parents of the global environment. Access a specific environment with `as.environment("package:stats")`; list the bindings in the environment's frame with `ls(e)`
4. `ls.str(e)` can list the structures of all the bindings in an environment
5. `exists("x",envir=e)` to determine if a binding exists in `e` or its parents, use arg `inherits = FALSE` to look in `e` only.

#### Function Environments

6. Function Environments include 4:
* Enclosing environment, where a function is created/defined. Note enclosing environment is used by lexical scoping.
* Binding environment, where a function is bound to a name. Note a function's enclosing and binding environment may be different. 
* Calling environment, where a function is called. Use `parent.frame()` to get a function's calling environment.
* Execution environment, which stores variables created during execution. 
7. Every package has two environments associated with it: the package environment and the namespace environment. Every exported function in a package is bound into the package environment but enclosed by the namespace environment. 
8. Looking up variables in the calling environment rather than in the enclosing environment is called dynamic scoping.

#### Binding names to values
9. surrounding unusual names with backticks.
10. Two special types of binding: delayed (`delayedAssign()`) and active binding (`activeAssign()`)
  * delayed: creates and stores a promise (i.e., expression) to evaluate the expression when needed
  * active: reevaluated every time they are accessed

#### Explicit environments
11. Take advantage of the reference semantics of environment data structure:
  * avoiding copies of large data (modify-in-place)
  * managing state within a package
  * efficiently look up values from names, like a hashtable access with `O(1)` (see package `hash`)
12. Note: if environment is used for this sake, always set the parent environment ot the empty environment to avoid bindings inherited from other environments: `my_env ,- new.env(parent = emptyenv())`.

###[Debugging, condition handling, and defensive programming](adv-r.had.co.nz/Exceptions-Debugging.html)
0. This chapter is very helpful, especially the discussion on interactive analysis and programming as well as defensive programming. 
1. __conditions__ include erros, warnings, and messages. Errors are raised by `stop()`, warnings are generated by `warning()`, and messages are generated by `message()`.

#### Debugging tools
2. Debugging tools include `traceback()`, `options(error = browser)` and `browser()`.
3. `traceback()` shows the sequence of calls that lead up to an error.
4. `options(error = browser)` will start an interactive console in the environment where the error occurred. Use `options(error = NULL)` to return to default behaviour.
5. You can also use `recover` and `dump.frames` in the `error` option. `recover` allows you to enter the environment of any of the calls in the call stack. `dump.frames` is an equivalent to `recover` for non-interactive code. iT CREATES A `last.dump.rda` and can be loaded, accessed interactively with `debugger()`.
6. `browser()` set up the debug mode and has several special commands:
  * `n`, next line
  * `s`, works like `n`, but if the next step is a function, it allows you to step into that function so you can work through each line.
  * `f`, finish the execution of the current loop or function
  * `c`, continues regular execution of the function
  * `Q`, stop debugging and terminates the function.
  * `where`, the interactive equivalent of traceback. (Note if not in debug mode, use `sys.calls()`; check functions start with `sys` that provide access to environments associated with functions further up the calling stack.)
7. Use `utils::setBreakpoint()` to set up breakpoint; it takes in a file name and line number.
8. A worse scenario is that your code might crash R completely, which indicates a bug in underlying C code. You can use `valgrind` to debug memory errors and use `gdb` to interactively debug the C code.

#### Condition handling
9. Some errors are expected and you want to handle them automatically. In R, there following three tools for handling conditions: 
  * `try()`: the error message will be printed but execution will continue. If no error, returns the evaluated value; if there is error, it return an (invisible) object of class `try-error`. You can use `is.error <- function(x) inherits(x,"try-error")` to test the try-error class.
  * `tryCatch()` let you specify different actions(i.e., functions or handlers) for warnings, errors, messages, and interrupts. 
  * `withCallingHandlers()` is a variant of `tryCatch()`
10. Examples using `try()`:
```r
# example 1: checks the class of returned object of try() includes try-error
is.error <- function(x) inherits(x,"try-error")
succeeded <- !sapply(results, is.error)

# example 2: another useful idiom is using a default value if an experssion fails
default <- NULL
try(default <- read.csv(myfile), silent = TRUE)
```
11. Examples using `tryCatch()`:
```r
tryCatch(mycode, 
  error = function(c) "error",
  warning = function(c) "warning",
  message = function(c) "message",
  interrupt = function(c) "Try again!")
```

#### Custom signal classes
12. `try()` and `tryCatch()` only handle conditions include `error`, `warning`, `message` and `interupt`. Since conditions are S3 classes, so you can define your own classes if you want to distinguish different types of error. But we can write a constructor function for conditions. Conditions must contain `message` and `call` compoents. When creating a new condition, it should always inherit from `condition`.
```r
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...)
} # Note structure returns the given object with further attributes set
```

#### Defensive programming
13. A key principle of defensive programming is to "fail fast":
  * Be strict about what you accept. use `stopifnot()` or `assertthat` package
  * Avoid functions that use non-standard evaluation: `subset`, `with`.
  * Avoid functions that return different types of output depending on their input. For example, use `vapply()` instead of `sapply()`; and always use `drop = FALSE` when subsetting a dataframe.

### [Functional programming](adv-r.had.co.nz/Functional-programming.html)
#### Overview
1. To prevent bugs and to make more flexible code, adopt the "do not repeat yoursefl" or DRY principle. This is the main thing I like about this book; you can learn about not R but also general and practical programming techniques.
2. Functions in R are first-class functions. You can do anything with functions that you can do with vectors. You can assign functions to variables, store them in lists, pass them as arguments to other functions (known as functionals), create them inside functions, and returen them as the result of a function (known as closures).
3. An example: 
```r
# summary function
summary <- function(x) {
  # assign functions to a list
  funs <- c(mean, median, sd, mad, IQR)
  # pass the list of funs to another fun lapply
  lapply(funs, function(f) f(x, na.rm = TRUE))
}
# system.time example
lapply(funList, function(f) system.time(f(x)))
```

#### Anonymous functions and closures
1. An naive example: `(function (x) 3)()`
2. One use of anonymous functions is to create closures (i.e., functions returned by other function)
3. Closures get their name because they enclose the execution environment of the parent function and can access all its variables.
4. In R, almost every function is a closure. All functions remember the environment in which they were created, typically either the global environment or a package environment. (Remember lexical scoping: lexical scoping looks up symbol values based on how functions were nested when they were created not when they were called.)
5. Closures are useful for function factories and mutable state:
  * function factories return functions.
  * use `<<-` to change values in parent function. 
