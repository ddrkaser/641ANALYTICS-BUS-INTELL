library(data.table)
#example 1
a = c(10,1,3,5,12)
b = c(1,1,2,1,2)
dt = data.table(a,b)
y = dt[a<9,sum(b)]

#example 2
a = c(10,1,3,5,12)
b = c(1,1,2,1,2)
dt = data.table(a,b)
y = dt[,sum(a),by=b]
x = y[V1>=14,sum(b)]

#example 3
a = c(10,1,3,5,12)
b = c(1,1,2,1,2)
dt = data.table(a,b)
dt[a<9,c:=sum(a)]
y = dt[is.finite(c),sum(b)]

dt[,mean(c,na.rm = T)]

#example 4
a = c(10,1,3,5,12)
b = c(1,1,2,1,2)
dt = data.table(a,b)
dt[,c:=sum(a),by=b]
y = dt[,.(sum(a),max(c),min(a)),by=b]
x = y[V1>V3,sum(V2)]


#example 5
a = c(10,1,3,5,12)
b = c('mango','cherry','blue','apple','plum')
dt = data.table(a,b)
y=dt[grep('[a-e]',b),sum(a)]


#example 6
a = c(10,1,3,5,12)
b = c('mango','cherry','blue','apple','plum')
dt = data.table(a,b)
y=dt[grep('lu',b),sum(a)]


#example 7
a = c(10,1,3,5,12)
b = c('mango','cherry','blue','apple','plum')
dt = data.table(a,b)
dt[,c:=gsub('lu','oo',b)]
y = dt[grep('[m-z]',c),sum(a)]

a = rep(c(NA,1,2,3),3)
dt = data.table(a)

dt[,c('a1','a2','a3'):=shift(a,n=-1:-3)]
dt[is.na(a),a:=rowMeans(a1,a2,a3)]
