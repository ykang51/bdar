####################
#### Unit 2 R codes
####################


print("Hello R!")

num <- c(1,2,3,4,5) #use c() to create/combine vectors
sum(num)
mean(num)
prod(num)
num * 10

num <- 1:5; # equivalent to “num <- c(1,2,3,4,5)”
# “apply” each function name to the anonymous function with num as its input
lapply( c(sum, mean, prod), FUN = function(f) f(num) )

# Load all datasets and functions on the RStudio server
library(bigDataR)

## Operators
# From:To
v = 1:5
v = 6:10
# Using seq()
v = seq(from = 1, to = 10, by = 2)
# Matrix Multiplication, dot and outer product
m1 = matrix(1:4, ncol = 2, byrow = T);m1
m1 * m1
m1 %*% m1

v = 1:3
v * v # scalar/cell product
v %*% v # dot product

v %o% v # outer product
outer(v, v, "*")
outer(v, v, FUN = "+")
outer(v, v, FUN = function(v1, v2) paste(v1, v2, sep ="---") )

m1 %*% m1 
m1 %*% m1 %*% m1

library(expm)
m1 %^%  3

# Vector/Scalar AND and OR
T | F
c(F, T, T) | c(T, F, T)
c(F, T, T) || c(T, F, T)

# Short-circuit 
x = 0
(x == 0) & (x <- x + 1) 
(x == 0) && (x <- x + 1)

# %in%
'a' %in% letters
c('a', 'b', '1') %in% letters

# Package::function() or Package::object
a = 1:5
mean = function(x) print("a function called \"mean\" ")
mean(a)
base::mean(a)
## end of "Operators"


typeof('abc')
typeof(9i)
typeof(123)

v <- c('a','b','1')
v %in% letters

if(T | (a <- 10) ) print('Hi')
if(T || (a <- 20) ) print('Hi')

mean(x = 1:10) # "=" is used as a function argument binding
mean(x <- 1:10) # "<-" is used as a variable assignment

v1 = 1:10
v1
v2 = c("a","b","c","1","2","3")
v2
v3 = vector(mode="character", length= 3)
v3
v4 = vector(mode="logical", length= 2)
v4

m = matrix(1:4, ncol=2, nrow=2, byrow = T)
m

colnames(m) = c('c1','c2'); rownames(m) = c('r1','r2')
m
t(m) # transpose the matrix

a = array(c(1,2,3,4,5,6), dim=c(1,2,3)) # 1 by 2 by 3
a
þ
dim(a) # get the dimension of the array

v1 = c(9,8,7)
v2 = c("a","b","c")
#combine 2 vectors
d = data.frame(x1 = v1, x2 = v2, stringsAsFactors = F)

str(d) # show the structure of the data frame “d”

# Using package "data.table"
library(data.table)
d_dt = data.table(d)
str(d_dt) # "data.table" is also a kind of data frame
d_dt[, c("v1") :=  v1 * 10, ]


L = list(k1 = c(9, 8, 7), k2 = c("a","b","c"), k3 = c(1))
L

data.frame(L)
L$k1  # get the data in the list L with the key “k1”
L$k3

group = c("control", "treatment", "treatment", "control")

group
group_f = factor(group, levels=c("treatment", "control"))
group_f  # check the actual data type of “group_f”
typeof(group_f)
unclass(group_f) # remove class attributes to get real data values

library('bigDataR')
smoker = as.data.frame(smoker)
tbl_smoker = table(smoker$Smoke, smoker$SES)
table(smoker$Smoke, smoker$SES)

chisq.test(tbl_smoker) # chi-square test of independence


a = c(1,2,3,4)
a
is.vector(a)
is.matrix(a)
m = as.matrix(a)
is.matrix(m)
t(m) # transpose the matrix

a = NA ; typeof(a)
object.size(a) # get the object size (bytes) in memory
b = NULL; typeof(b)
object.size(b)

square = function(x = NULL){
  return(ifelse(is.null(x), "NULL", x^2 ) );
}
# if x is not NULL, then output x squared.
square()
square(3)

x = 'global'
printXY = function(){
  y = 'local';
  print(x);
  print(y);
}
printXY()
x
y
# assign a new string to x then print 
x = 'global_x' 
printX = function(x){ 
  print(x); x = 'local_x'; print(x); 
} 
printX(x) 

# Set operations, an R ellipsis example
setOper = function(f, ...){
  el = list(...)
  return(Reduce(f, el))
}
setOper(intersect, 1:5, 2:6, 3:5)
setOper(union, 1:5, 2:6, 3:5)

f = function(x){ 
  if(x > 10){ 
    print("x is great than 10"); 
  } else if( x >=0 & x <=10) { 
    print("x is between 0 and 10"); 
  } else { 
    print("x is less than 0"); 
  }
} 
f(-1) 
f(5) 
f(11)

library(ggplot2movies)
movies = as.data.frame(movies); movies$longshort = ""
# Very bad practice with for loop. Don't do this!
system.time({
  for(i in 1:nrow(movies)){
    if(movies[i, "length"] > 120) movies[i, "longshort"] = "long"
    else movies[i, "longshort"] = "short" }
})
# Use ifelse() instead
system.time( 
  movies$longshort <- ifelse(movies$length > 120, "long", "short"))

# Or simply vectorized it!
system.time({
  movies[movies$length > 120, "longshort"] = "long"
  movies[movies$length <= 120, "longshort"] = "short"
})

for(i in c('a','b')) print(i) 

for(k in 1:3) {   
  if(k == 3) break; print(k); 
} 
x = 3; 
while(x > 0){ 
  print(x);
  x = x - 1; 
} 
x = 3 
repeat{ 
  print(x); 
  x = x - 1; 
  if(x < 1) break; 
} 

switch(3, 'a' = {x = x + 5;},
       'b' = {x = 999},'c' = {x = 'ABC'}
)
x # 2nd statement after expr
x = 0;
switch(1, 'a' = {x = x + 5;},
       'b' = {x = 999}, 'c' = {x = 'ABC'})
x # 1st statement after expr
switch('a', 'a' = {x = x + 5;},
       'b' = {x = 999}, 'c' = {x = 'ABC'})
x


bmi_num = c(19,39,20,22,34,24);

bmi_cat = cut(x=bmi_num,breaks=c(0,18.5,24.9, 29.9, Inf),
              labels=c('Underweight','Normal weight','Overweight','Obesity'));

bmi_cut_str = "lo:18.5 = 'Underweight';
18.5:24.9 = 'Normal weight';
24.9:29.9 = 'Overweight';
29.9:hi = 'Obesity'"

bmi_cat = car::recode(bmi_num, bmi_cut_str, as.factor.result = T,
                      levels = c('Underweight','Normal weight','Overweight','Obesity'))

table(bmi_cat)                      
xtabs(~ bmi_cat,  bmi_cat)

library("RGtk2Extras")
df_data = data.frame()
dfedit(df_data, modal=F)


vars_data = read.csv(file='../vars.csv', header=T, quote="'")
vars_data


library("foreign")
write.foreign(df = vars_data, datafile='vars_data',
              codefile="spss_read_code.txt" , package=c("SPSS"))
file.show('vars_data') # Export as SPSS files
file.show('spss_read_code.txt')

save.image() # Save everything in current Environment
save(x=vars_data, file='vars_data.RData'); # Save “vars_data”
rm(list=ls()) #Remove all objects
load("vars_data.RData") #load the image file we saved

library("DBI")
library("RMySQL")
# create connection
mycon = dbConnect(dbDriver('MySQL'), dbname='testdb', host='127.0.0.1', user='root');
# using R SQL pass-through facility:
dbSendQuery(mycon, "create table test (var1 int, var2 char(8))");
dbSendQuery(mycon, "insert into test values(101,'a') ");
test_data = dbGetQuery(mycon, "select * from test");
test_data
dbDisconnect(mycon) # close connection

write.csv(as.data.frame(matrix(runif(10 ^ 6 ,0,1),  nrow=1000)), file='rnum.csv');
# An ~18 MB CSV file.
file.info("rnum.csv")$size; # get file size
system.time({rnum = read.csv(file= "rnum.csv", header=T)});

rm(rnum); library("data.table"); #load package data.table
system.time({ rnum = fread(input="rnum.csv")});

rm(rnum); library("sqldf"); # load package sqldf
system.time({ rnum = read.csv2.sql(file="rnum.csv", header=T,sep = ",");})
# The elapsed times vary on different machines

mtcars = data.frame(mtcars)
mtcars[grep('Toyota', rownames(mtcars)),] # show me “Toyota”

mtcars[,c('mpg','hp','wt')] # select columns
mtcars[mtcars$wt > 5, c('mpg','wt')] #please also check subset()

set.seed(1) # set random seed
rand = runif(n=5,min=0,max=1)
rand # generated random numbers
sort(rand)

# sort by descending “wt”
mtcars[order(mtcars$wt,decreasing=T),c('mpg','wt')]
# sort by ascending “cyl” and descending “wt”
mtcars[order(mtcars$cyl, -mtcars$wt),]
# rank by "mpg“ & create a new dataset
data.frame("car_name" = rownames(mtcars),
           "mpg" = mtcars$mpg,
           "rank" = rank(mtcars$mpg, ties.method = "first"))

dup = data.frame(x1=c('a','a','b','b','c'), x2=c(1,1,2,3,4))
dup

dup_removed = dup[!duplicated(dup), ]
dup_removed

mtcars[,c('vs','am','gear','carb')] = list()
mtcars #some variables/columns have been removed
mtcars = mtcars[, ! colnames(mtcars)  %in% c('cyl','disp','drat','qsec')]
mtcars
# Or, we can use "NULL“
mtcars$mpg = NULL;
mtcars

# Add a new column
# data frame is a sort of “List”
mtcars$newVar = c(1:32)
mtcars # a new column “newVar” has been added

options(stringsAsFactors = F)
A = data.frame(id=c(1,3,5), A_val=c('a','x','c'))
B = data.frame(id=c(3,5,6), B_val=c('x','y','z'))

A_B = merge(x=A, y=B, by.x='id', by.y='id') 
A_B 

left_A_B = merge(x=A, y=B, by.x='id', by.y='id', all.x=T)
left_A_B # A left join B

right_A_B = merge(x=A, y=B, by.x='id', by.y='id', all.y=T) 
right_A_B # A right join B

full_A_B = merge(x=A, y=B, by.x='id', by.y='id', all=T) 
full_A_B  # A full outer join B

colnames(A) = colnames(B) = c('id','val');  
rbind(A,B) # Concatenate vertically

cbind(A,B) # Concatenate horizontally

# A intersect B
subset(A, (A$id %in% B$id & A$val %in% B$val))

# A except B
subset(A, ! (A$id %in% B$id & A$val %in% B$val)) 

# B except A
subset(B, ! (B$id %in% A$id & B$val %in% A$val))

# More efficient row/column binding
dplyr::bind_rows(A, B)
dplyr::bind_cols(A, B)

# A intersect/union B
dplyr::intersect(A, B); dplyr::union(A, B);

# A except B; B except A
dplyr::setdiff(A, B); dplyr::setdiff(B, A)

# Observation-level set comparison
dplyr::setequal(A, B);

library(sqldf)
# Inner join
sqldf('select A.id, A.val as A_val, B.val as B_val from A inner join B on A.id = B.id')

# Left outer join
sqldf('select A.id, A.val as A_val, B.val as B_val from A left join B on A.id = B.id')

# Union
sqldf('select * from A union select * from B')

sqldf('select * from A except select * from B') 
sqldf('select * from A intersect select * from B') 
sqldf('select * from (select * from A union all select * from B) where id > 5') 

mtcars = data.frame(mtcars)
sqldf('select row_names, mpg, cyl, wt from mtcars where row_names like "%Toyota%" ', row.names=T) 

# Reshape
library(bigDataR); library(reshape2); data("reshape_data");
md = melt(data = reshape_data, id.vars = c("ID", "time"));
md

# "ID" by "time + variable". Default aggregation function
# length() is used to count the number of rows
dcast(data = md, formula = ID + time ~ variable,
      value.var = "value")

dcast(data = md, formula = ID ~ time + variable, 
      value.var = "value")

dcast(data = md, formula = ID ~ time + variable, 
      value.var = "value", fun.aggregate = length)

# "ID" by "variable" with aggregation function mean()
dcast(data = md, formula = ID ~ variable,
      value.var = "value", fun.aggregate = mean)

# Using tidyr to "gather" into long or spread into wide datasets
# Note that those column names/indices defined after "key" and "value" will be melted
md = gather(reshape_data, key = "keyCol", value = "valCol", 3, 4)
# Spread our "meleted" data 
# Again column names/indices defined after "key" and "value" will be spread
spread(md, key = "keyCol", value = "valCol", 3, 4)

# Try it
CO2 = datasets::CO2
data1 = dcast(data=CO2, formula =  Plant ~ Treatment,
              fun.aggregate = length)
data1
data2 = dcast(data=CO2, formula = Type ~ Treatment,
              value.var = "uptake", fun.aggregate = mean )
data2

data(mtcars)
aggregate(x=mtcars$mpg, by=list("cyl"= mtcars$cyl), FUN=mean) 
by(data=mtcars$mpg, INDICES=list("cyl"= mtcars$cyl), FUN=mean) 

m = matrix(1:16,ncol=4)
apply(m, MARGIN=1,FUN=sum) # get sums by row
apply(m, MARGIN=2,FUN=sum) # get sums by column

set.seed(1); # set seed
L=list( num = 1:10 , randNum = runif(100,0,1) )
lapply(X=L, FUN=mean) # get means for “keys” in the list

# “Split” mtcars by different cutpoints of horsepower(hp)
kv1 = split(mtcars, cut(mtcars$hp, breaks = c(0,100,200,Inf)));
kv1
# “Apply” mean() for each key-value pair
kv2 = lapply(kv1, FUN=function(x) return( mean(x$mpg)) );
kv2
# “Combine” the result
rt = data.frame(cbind(kv2)); colnames(rt) = 'average_mpg';
rt

# Equivalent to above statements
aggregate(mtcars$mpg,by =
            list(hp = cut(mtcars$hp,  breaks = c(0,100,200,Inf))), FUN=mean)

library(plyr)
two_lm = dlply(.data = mtcars[,c('mpg','wt','am')],
               .variable = 'am',
               .fun=function(x) lm(x$mpg ~ x$wt))

ggplot(data=mtcars,mapping = aes(x = wt, y=mpg)) +
  geom_point() + stat_smooth(method=lm) +
  facet_grid( ~ am, labeller = label_both)


# Hadoop environment variables
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")

# Load RHadoop libraries
library(rhdfs)
library(rmr2)

# Initialize rhdfs
hdfs.init()

library("rmr2"); # Load package rmr2 of RHadoop
mtcars_dfs = to.dfs(mtcars); # Put mtcars on HDFS
mtcars_fit = from.dfs(
  mapreduce( input = mtcars_dfs,
             map = function(k, v){
               return(keyval(key =  v$am, val = v )); # Split by "am"
             },
             reduce = function(k, v){
               return(keyval(key=k, val=list(lm(mpg ~ wt, data=v))));
             } )
)
summary(mtcars_fit$val[[1]]) # model for am=0, automatic
summary(mtcars_fit$val[[2]]) # model for am=1, manual

mtcars_dfs = to.dfs(mtcars); # Put mtcars on HDFS
# a function to get crosstab, "x" by "y"
crosstab_MR = function(dfs_data, x, y, ylevels){
  mapreduce( input = dfs_data,
             map = function(k, v){
               # Split by "x" values as the keys
               return(keyval(key = v[,x], val = v[, y] ));
             },
             reduce = function(k, v){
               tab = rbind(table(factor(v,levels=ylevels) ));
               rownames(tab) = k;
               return(keyval(key=k, val=tab));
             })}
from.dfs(crosstab_MR(mtcars_dfs, x = 'am', y = 'gear', ylevels = c(3,4,5))) # get result from HDFS


ggplot(data=mtcars,mapping = aes(x = wt, y=mpg)) +
  geom_point() + stat_smooth(method=lm) +
  facet_grid( ~ am, labeller = label_both)

summary(lm(mpg ~ wt + am + wt:am , data=mtcars))

summary(lm(mpg ~ wt + am , data=mtcars))

aggregate(mtcars$wt, list(mtcars$am), mean )

# Titanic
#install.packages("epiDisplay");
titanic = as.data.frame(Titanic)
t_glm = glm(Survived ~ Class + Sex + Age, weights = Freq, family = binomial, data = titanic)
epiDisplay::logistic.display(t_glm)
summary(t_glm)





