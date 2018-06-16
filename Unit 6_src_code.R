####################
#### Unit 6 - Advanced Topics in R High Performance Computing
#### R codes
####################

# Measuring execution time for a block of expressions with system.time()
system.time({
  rnorm(1000000, 0,1)
  lm(mpg ~ ., mtcars)
})

library(rbenchmark)
# Replicate & run each statement for 10 times. 
bm10t = benchmark(
  rnorm(1000000, 0,1),
  lm(mpg ~ ., mtcars), 
  replications = rep(1,10))

# Get average elapsed time for each statement
by(bm10t$elapsed, INDICES = list("Elapsed time" = bm10t$test), mean)

# Using microbenchmark
library(microbenchmark)
mb = microbenchmark(
  rnorm(10^5, 0,1),
  rnorm(10^6, 0,1),
  rnorm(10^7, 0,1),
  times = 20, unit = "s"
)
autoplot(mb); mb

# Different implementation of getting sample mean
my_summary = function(d){
  # loop-based
  loop_mean = function(x){
    total = 0
    for(i in 1:length(x)) total = total + x[i]
    return(total / length(x))
  }
  # vectorized 
  vector_mean = function(x) sum(x)/length(x)
  # built-in function
  builtin_mean = function(x) mean(x)
  # print result
  print(loop_mean(d)); print(vector_mean(d)); print(builtin_mean(d))
}

# R profiling functions
Rprof("profile.txt", memory.profiling = T)
my_summary(rnorm(10^8,0,1))
Rprof(NULL); summaryRprof("profile.txt", memory = "both")

install.packages("proftools", type = "source")
source("http://bioconductor.org/biocLite.R"); biocLite(c("graph", "Rgraphviz"))

# Load packages
Map(require, c("proftools", "graph", "Rgraphviz"), character.only=T)

Rprof("profile_no_mem.txt")
my_summary(rnorm(10^8,0,1))
Rprof(NULL); summaryRprof("profile_no_mem.txt")

p = readProfileData(filename = "profile_no_mem.txt")
plotProfileCallGraph(p, style=google.style, score="total")

# Using package "profvis"
library(profvis)
diamonds_df = as.data.frame(diamonds)
profvis({ 
  diamonds_df$logPrice = diamonds$price
  summary(diamonds_df)
  diamonds_lm = lm(logPrice ~ . - price, data = diamonds_df)
})
profvis({
  dat <- data.frame(
    x = rnorm(5e4),
    y = rnorm(5e4)
  )
  plot(x ~ y, data = dat)
  m <- lm(x ~ y, data = dat)
  abline(m, col = "red")
})

# Vectorization
set.seed(1)
rand_1to10 = sample(1:10, 1000, T)
lagged_rand = numeric(999)

library(microbenchmark)
# Loop
microbenchmark(
  for(i in 1:999){
    for(j in 2:1000) lagged_rand[i] = rand_1to10[j] - rand_1to10[i]
  },
  times = 10, unit = "s"
)
# Vectorized 
microbenchmark(
  lagged_rand = rand_1to10[2:1000] -  rand_1to10[1:999],
  times = 10, unit = "s"
)
# Built-in function, diff()
microbenchmark(
  lagged_rand = diff(rand_1to10, 1),
  times = 10, unit = "s"
)
# Function to generate key-value pairs(a letter and its size) using loop
# The object.size() is not a vectorized function
genKV = function(keys, values){
  kv_pairs = list(); kv_len = length(keys)
  for(i in 1:kv_len) kv_pairs[keys[i]] = object.size(values[i])  
  return(kv_pairs)
}
genKV(letters, 1:26)
# Create the function that works on scalar value
genKV_v = function(k, v){
  kv = list(); kv[k] = object.size(v); return(kv)
} 
genKV_v(letters, 1:26) # It doesn't work as we expected
# Say, we'd like to vectorize the function genKV_v 
genKV_v2 = Vectorize( genKV_v, vectorize.args = c("k", "v"), USE.NAMES = T)
genKV_v2(letters, 1:26) 
# Or we can just use Map()
Map(genKV_v, k = letters, v = 1:26)

# Using anonymous functions
(function(x) x^3)(2) # 2^3
(function(x, y, z) x+y+z)(1,2,3) # 1 + 2 + 3

# More complicated example. DO NOT do it. 
(function(oper) if(oper == "square") function(x) x^2 else function(x) x^3)("square")(2)
# Give the function a name
pow = function(oper) if(oper == "square") function(x) x^2 else function(x) x^3
pow("")(2)

# Anonymous function also has formals() & body()
formals(function(x, y) x^2 + y^2)
body(function(x, y) x^2 + y^2)

# Using anonymous function with functionals
lapply(ggplot2movies::movies, function(x) any(is.na(x)) )

# My own lapply() with 1 anonymous function
my_lapply = function(x, FUN) 
  for(i in colnames(x)) {l <- list(); l[[i]] <- FUN(x[i]); print( l )} 
# Equivalent to lapply() but just to print the results
my_lapply(ggplot2movies::movies, function(x) any(is.na(x)) )

# My nested functional with 2 anonymous functions
my_nested_fun = function(x, FUN1, FUN2) FUN1(FUN2(x))
# A sample use of the nested functional- create a matrix with the same size of the input data
my_nested_fun(mtcars, function(x) matrix(nrow=x[1], ncol=x[2]), function(x) dim(x) )


# Closures
# A function to generate functions to get a value near n percentile values  
closeToNPercentile = function(n){
  function(v) v[which.min(abs(v - quantile(v, probs = n/100)))]
}
# Creating functions that get a value close to 10% or 25% percentile 
closeTo10Percentile = closeToNPercentile(10)
closeTo25Percentile = closeToNPercentile(25)
closeTo10Percentile(mtcars$mpg)
closeTo25Percentile(mtcars$mpg)

closeToNPercentile(50)(mtcars$mpg)
# An closeToNpercentle() factory but with a variable to count 
# "how many times such functions are called by users".
closeToNPercentile_ct = function(n){
  counter = 0
  function(v){
    counter <<- counter + 1
    print(paste("This function has been called", counter, "time(s)."))
    v[which.min(abs(v - quantile(v, probs = n/100)))]
  }
}
closeTo10Percentile = closeToNPercentile_ct(10)
closeTo25Percentile = closeToNPercentile_ct(25)
closeTo10Percentile(mtcars$mpg)
closeTo10Percentile(mtcars$mpg)
closeTo25Percentile(mtcars$mpg)

# Let's say we'd like to standardize r1 to r10 in "movies"
library(ggplot2movies); data(movies)
col2replace = paste("r", 1:10, sep="") # r1 to r10

# Using bulit-in function It's a bit slow. 
movies[, col2replace] = scale(movies[, col2replace])

# Using loop with few iterations is faster.
for(i in col2replace) movies[, i] = scale(movies[, i])

# Using anonymous closure with access to parental dataset    
# Using anonymous closure with access to parental dataset   
Map(function(x) {movies[,x] <<- scale(movies[,x]);}, list(col2replace));

# Raw functions can be stored in an R list
sum_fun = list( min, max, mean, sd, median)
# Serialized functions are just raw binary values stored in a vector 
s_sum_fun = serialize(sum_fun, connection = NULL)
unserialize(s_sum_fun)

# Lists of functions in practice
my.summary = function(df){
  sum_fun = list(min, max, mean, sd, median)
  Map(function(f){ sapply(df, function(v) f(v, na.rm = T) )} , sum_fun)
}
my.summary(mtcars)

# Functionals vs. loop

# A trivial example to demonstrate loop pattern using functionals instead of loops
m = matrix(rep(0,10^6), ncol = 10^3)
microbenchmark(
  for(i in 1:10^3) for(j in 1:10^3) m[i,j] = i + j,
  times = 5, unit = "s"
)
microbenchmark(
  Map(function(i){
    Map(function(i, j){
      m[i, j] <<- i + j
    }, i, list(1:10^3)) 
  }, list(1:10^3) ),times = 5, unit = "s"
)

# Map, Reduce, and Filter
# With "accumulate = T" to return result of each step
Reduce( function(v1, v2) v1 + v2, 1:5, accumulate = T)

# Folding from "right to left"
Reduce( "+", 1:5, accumulate = T, right = T)

# Recursive set operations
setOfNum = list(1, 4, 6, c(1,3,5,7,9), c(3, 5))
Reduce(union, setOfNum)

# Reduce() in action: 
L1 = list(1:10); L2 = letters[1:10]; L3 = list(1:5)
Reduce(function(x,y) cbind(data.frame(x), data.frame(y)), list(L1, L2, L3))

library(randomForest)
# Assume that we have 3 different random forests trained by different nodes/machines
rf10 = randomForest(mpg ~ . , data = mtcars, ntree = 10)
rf20 = randomForest(mpg ~ . , data = mtcars, ntree = 20)
rf30 = randomForest(mpg ~ . , data = mtcars, ntree = 30)

# Combine these forests to form a bigger one
rf60 = Reduce(randomForest::combine, list(rf10, rf20, rf30)); rf60

# We can surely use Map() to generate a list of forests used here to simulate the results.
Reduce(function(f1, f2) combine(f1,f2),
       Map(function(nt) randomForest(mpg ~ . , data = mtcars, ntree = nt), list(10, 20, 30)))

# Predicate Functionals
list_w_na = list(x1 = c(0,1,3), x2 = c("a", NA, "0"),  x3 = c(4,5,NA))
# Keep x1 values > 0
Filter(function(x) x > 0, list_w_na$x1)
# Keep NOT (x1 values > 0) with Negate()
Filter(Negate(function(x) x > 0), list_w_na$x1)
# Keep all values > 0
Map(function(v) Filter(function(x) x > 0, v), list_w_na)
# Find the positions of "NA"s
Map(function(v) Position(is.na, v), list_w_na)

# Package "parallel"
library(parallel); library(randomForest)
# Use all CPU cores may freeze your machine!
cl = makeCluster(detectCores() - 1)
# Export randomForest to all R instances
clusterExport(cl, "randomForest")
# Train a forest with different formulas
microbenchmark(
  Map(function(f) randomForest(f, data = ggplot2::diamonds, ntree = 10), 
      list(log(price) ~ carat, log(price) ~ y , log(price) ~ cut, log(price) ~ clarity)),
  times = 5, unit = "s"
)
microbenchmark(
  parLapply(cl, list(log(price) ~ carat, log(price) ~ y , log(price) ~ cut, log(price) ~ clarity), 
            function(f) randomForest(f, data = ggplot2::diamonds, ntree = 100)
  ),
  times = 5, unit = "s"
)
# Stop all worker instances
stopCluster(cl)

### Using dplyr
library(dplyr)
# Say, our data is simple an R data frame. "mtcars" again?
# Convert data frame into "tibble". "mtcars" has become a "tbl"
mtcars_tb = as_tibble(mtcars); class(mtcars_tb)
glimpse(mtcars_tb, width = 80 ) # Get a "glimpse" of a tibble
print(mtcars_tb, n = 30) # 30 rows to show 


# Using contains() and between(). Only works on local tibbles
mtcars_tb[dplyr::between(mtcars_tb$mpg, 20, 25),]
mtcars_tb[dplyr::contains("toyota", ignore.case = T, vars = rownames(mtcars_tb)),]
slice(mtcars_tb, 3:5) # 3-5th rows


# Just like the way we create R data frame
aTibble = tibble(x = letters[1:5], y = lubridate::today(), z = runif(5,0,1))

# More efficient versions of rbind() and cbind()
bind_rows(aTibble, aTibble)
bind_cols(mtcars_tb, tibble(rowNum = 1:nrow(mtcars)))


library(RSQLite) # If our dataset is in an SQLite database
# Create a temporary in-memory SQLite database with a table for "movies"
con = dbConnect(RSQLite::SQLite(), dbname = ":memory:")
# Or, create a SQLite embedded database file in hard drive
# con = dbConnect(RSQLite::SQLite(), dbname = "./myDB.Sqlite")
dbWriteTable(con, "movies", ggplot2movies::movies ) 
dbListTables(con) # list tables in the database
# Get result of a query as a data frame (a SQL pass-through query)
movies_df = dbGetQuery(con, "select * from movies") 

# Say we'd like to directly manipulate tables in the database
sqliteDB = dbplyr::src_dbi(con, auto_disconnect = F)
# movies & movies_8 are simply pointers to the tables in the database!
movies_tb = tbl(sqliteDB, "movies")
movies_tb_8 = movies_tb %>% filter(rating > 8) %>% select(title, year, rating)
# tbl_lazy" denotes the "lazy evaluation" of the data manipulation
class(movies_tb_8) 
# Both tibbles are simply "pointers" to the tables in the database
# The size of them are relatively smaller than original data frame
pryr::object_size(ggplot2movies::movies)
pryr::object_size(movies_tb)
# We can surely check out the query plan
dplyr::explain(movies_tb_8)

# "movies_8" actually does not exist. 
# Let's execute and store result in remote database server 
sqliteDB # or run dbListTables(con)
compute(movies_tb_8, name = "movie_8_table"); sqliteDB
# We can surely download/copy the table from the database
movies_8_df = collect(movies_tb_8)
# How about the other way around? Copy a data frame to remote database.
copy_to(dest = sqliteDB, df = mtcars, name = "mtcars_table", temporary = F)
dbListTables(con)

## Piping your data
# Select by names. Equivalent to select(movies_tb, title, length, rating)
movies_tb %>% select(title, length, rating)

# title with r1-r5
movies_tb_r1to5 = movies_tb %>% select(title, num_range(prefix="r", range = 1:5))
explain(movies_tb_r1to5)

# all variables except r1 to r10
movies_tb %>% select(-(r1:r10))

# title with any variable names that contains numbers
movies_tb %>% select(title, matches("[[:digit:]]"))

# select and rename 
movies_tb %>% select(title, rating_1to10 = rating )

# A local tibble. movies is already a tibble.
movies_tb_local = as_tibble(ggplot2movies::movies)
# The good, the bad, and the ugly(very long movie title?) 
movies_tb_local %>% select(title, length, rating ) %>% 
  filter(rating > 9 | rating < 2 | nchar(title) > 80) %>% arrange(desc(nchar(title))) 

# Give me "Star Trek" series!
movies_tb_local %>% select(title, length, rating) %>% 
  filter(between(length, 60, 150), base::grepl("star trek", title, ignore.case = T))

# Note that "grepl()" doesn't support by SQLite. Below will show an ERROR !
movies_tb %>% select(title, length) %>% filter(grepl("star trek", title, ignore.case = T))

# Split by "longShort" (Grouped by: longShort)
movies_longShort = movies_tb %>% group_by(longShort = ifelse(length > 130, "long", "short")) 
# Apply n() and Combine. Type ?summarise for more aggregation functions
movies_longShort = movies_longShort %>% summarize(ct = n())

# Movies rating counter
movies_rating_ct = movies_tb %>% select(rating) %>% group_by(round_rating = round(rating, 0)) %>% 
  dplyr::summarise( numOfObs = n()) %>% arrange(desc(round_rating)) %>% head(n = 5)
# Equivalent to the SQL explained below
dplyr::explain(movies_rating_ct)

# Add new columns with mutate(). Only keep new columns with transmute()
movies_tb %>% select(title, length, rating) %>% 
  mutate(lenOfTitle = nchar(title), round_rating = round(rating, 0)) 

# Sampling with sample_n() and sample_frac()
# May only work on local R tibbles
movies_tb_local %>% select(title, length, rating) %>% sample_frac(size = 0.7, replace = F)


# Reshaping your tibbles with tidyr
library(tidyr)
reshape_tb = tibble(ID=c(1,1,1,2,2,3), time=c("t1", "t2", "t3", "t1", "t3", "t1"), 
                    var1=c(2,4,2,1,3,2), var2=c(3,1,3,2,4,3))

# from "wide" to "long" format
gathered_tb = reshape_tb %>% gather(., key = "var", value = "val", fill= 3:4) %>% arrange(., ID, time)
# from "long" to "wide" format
gathered_tb %>% spread(., key = "var", value = "val")
# Data diamonds
diamonds_tb_local = ggplot2::diamonds %>% mutate(logPrice = log(price)) %>% select(-price)
# Copy "diamonds" to SQLite database
diamonds_tb_remote = copy_to(sqliteDB, diamonds_tb_local, 
                             "diamonds", overwrite = T, temporary = F)
# Check out tables in the database
sqliteDB # Or dbListTables(con)

# Unite columns into one using unite() and unite_()
diamonds_tb_local %>% tidyr::unite(col = xyz, x, y, z, sep = ",")
# Unfortunately, functions in tidyr don't work on remote databases as below 
diamonds_tb_remote %>% tidyr::unite(col = xyz, x, y, z, sep = ",")
# Separate columns with separate()
hitters_tb_local = as_tibble(ISLR::Hitters) %>% mutate(hitter_name = rownames(.))
hitters_tb_local %>% select(hitter_name, Years) %>% 
  separate(col=hitter_name,into = c("first_name", "last_name"), sep = " ")
# Disconnect
dbDisconnect(con); rm(con)
# Do some housekeeping
rm(list = ls()); gc()

### Using data.table
library(ggplot2movies); library(data.table); library(microbenchmark)
movies_df = data.frame(movies, stringsAsFactors = F)
movies_dt = data.table(movies, stringsAsFactors = F)
# Dataset size are approximately the same in new version of data.table
Map(function(d) pryr::object_size(d), 
    list("DF" = movies_df, "DT" = movies_dt))
# A simple test on filtering data
microbenchmark(
  nrow(movies_df[movies_df$rating > 6,]),
  times = 10, unit = "s"
)
microbenchmark(
  nrow(movies_dt[rating > 6]),
  times = 10, unit = "s"
)

diamonds_dt = data.table(diamonds)
# Frequencies of cut by clarity. Here ".N" is used to count # of rows
diamonds_dt[, .N, .(cut, clarity) ]
# Find thoses big diamonds > 6mm in length, width, and depth
# setorder() is used for super fast row reordering of a data.table 
diamonds_6mm = setorder(diamonds_dt[ (x > 6 & y > 6 & z > 6) , ], carat)
diamonds_6mm
# Equivalent to "select avg(price) as avg_price, 
# count(*) as ct from diamonds_dt where carat > 0.5 group by cut"
diamonds_dt[carat > 0.5, .(avg_price = mean(price), ct = length(price), N=.N ), .(cut) ]
# data.table also supports melt() and dcast()
melted_genre = setorder(data.table::melt(movies_dt, id = 1, measure=list(18:24) ), title)

# All existing data.tables in the R environment
data.table::tables()
# Show me "Star Trek"
movies_dt[title %like% "Star Trek", .(title, rating)]
# Rating between 6 and 7
movies_dt[rating %between% list(8,9), .(title, rating)]

# Fast plain text data file reader and writer
fwrite(movies_dt, "movies_dt.csv", sep=",") 
movies_dt_copy = fread("movies_dt.csv", sep=",", stringsAsFactors = F,fill = T, verbose = T)

# "Real" data set operations
setTbl1 = data.table( "X1" = 1:5, "X2" = 11:15)
setTbl2 = data.table( "X1" = 4:5, "X2" = c(13L,15L)) # 13L & 15L are integers
fsetdiff(setTbl1, setTbl2)
fintersect(setTbl1, setTbl2)
funion(setTbl1, setTbl2)

# Joining data.table
tbl1 = data.table( "ID1" = sample(1:10^5), "X" = runif(10^5,0,1) )
tbl2 = data.table( "ID2" = sample(1:10^5), "Y" = runif(10^5,0,1) )

microbenchmark( merge(tbl1, tbl2, by.x = "ID1", by.y = "ID2"),
                times = 20, unit = "s" )
setkey(tbl1, ID1); setkey(tbl2, ID2)
microbenchmark( merge(tbl1, tbl2, by.x = "ID1", by.y = "ID2"),
                times = 20, unit = "s")

setkey(movies_dt, title)
# Access by a "key"
movies_dt["Star Trek: Nemesis"]

# Update with rounded values
diamonds_dt[, c("x", "y", "z") := list(round(x), round(y), round(z))]

# Update column names 
setnames(diamonds_dt, c("x", "y", "z"), c("length", "width", "depth") )

# Delete columns by using NULL
diamonds_dt[, c("x", "y", "z") := NULL];

# Chainning data.table. Equivalent to
# "select cut, count(*) as N from diamonds_dt group by cut order by N desc"
diamonds_dt[, .N , .(cut)][order(-N)]

# More complicated example
diamonds_dt[, .N , .(cut, color)][order(-color, cut)][color %in% c("G", "H") ]

# Print intermediate tables group by "color"
diamonds_dt[, print(.SD) , .(color)]
# Count the number of rows
diamonds_dt[, nrow(.SD) , .(color)]
diamonds_dt[, .N , .(color)]

# Get max value of each columns grouped by "color"
diamonds_dt[, Map(max,.SD) , .(color)]

# Using "big"-family functions to create memory-mapped file
library(bigmemory)
bigIntegerMatrix = as.big.matrix( matrix(sample(1:5, 10^8, replace = T ), nrow = 10^4), type = "integer", 
                                  backingfile = "bigIntegerMatrix.bm", descriptorfile = "bigIntegerMatrix.bm.desc")

# It's actually just a pointer to files in hard drive.
bigIntegerMatrix; object.size(bigIntegerMatrix)

# How about the size of actual files? 4 Bytes * 10^4 ~= 400,000,000 Bytes
list.files(pattern = "bigIntegerMatrix.bm"); file.info("bigIntegerMatrix.bm")

# Remove all objects in R environment
rm(list = ls())
# Get the matrix back by loading the file descriptor first.
bigIntegerMatrix_desc = dget("bigIntegerMatrix.bm.desc")

# We have it back by attaching it again
bigIntegerMatrix = attach.big.matrix(bigIntegerMatrix_desc)

# Get the info. of the object
describe(bigIntegerMatrix)

# Get summary statistics of the matrix
library(biganalytics)
summary(bigIntegerMatrix[, 1:100])
colna(bigIntegerMatrix) # Numbers of NAs

# Remove the files if no longer needed
file.remove("bigIntegerMatrix.bm", "bigIntegerMatrix.bm.desc")

# Let's say "diamonds" dataset is big
diamonds_bm = as.big.matrix(as.data.frame(ggplot2::diamonds))

# Creating a big.matrix object ready for modeling
library(dummies)
diamonds_df = as.data.frame(diamonds)
# Remove orders
invisible(
  Map(function(var) diamonds_df[,var] <<- factor(diamonds_df[,var], ordered = F), 
      c("cut", "color", "clarity"))
)
diamonds_numeric = dummy.data.frame(diamonds_df, sep = "_")

# In case if some level names are not syntactically valid
colnames(diamonds_numeric) = make.names(colnames(diamonds_numeric))
# The variables in "diamonds" is now all numeric
str(diamonds_numeric)

# Creating big.matrix object
diamonds_numeric_bm = as.big.matrix(diamonds_numeric, type = "double")

# Fitting linear model. "Fair" as the reference level  
diamonds_bigglm = bigglm(
  log(price) ~ carat + depth + table + x + y + z + 
    cut_Good + cut_Very.Good + cut_Premium + cut_Ideal, 
  data = diamonds_numeric_bm)
# We skip "cut_Fair"
summary(diamonds_bigglm)
# It is eqivalent to...
diamonds_df$cut = factor(diamonds_df$cut, ordered = F)
summary(lm(log(price) ~ carat + depth + table + x + y + z + cut, diamonds_df) )
# Notice that we removed the "order" of variable cut. Did we lost some information?

