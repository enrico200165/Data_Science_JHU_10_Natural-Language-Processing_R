
myF <- function(x, xtype) {
  varName <- deparse(substitute(x))
  if (!exists(varName)) {
    x <- vector(xtype, 1)
  } else {
    x <- get(varName)
  }
  x <- x[1]
  assign(varName,x,envir=parent.frame(n = 1))
}
if (exists(deparse(substitute(myVar)))) {
  rm(myVar)
}
#myF(myVar, "numeric")
myF(myVar, "character")
print(myVar)


# -------------------------------------------

f <- function(x) {
  varName <- deparse(substitute(x))
  if (!exists(varName)) {
    x <- vector(xtype, 1)
  } 
  eval.parent(substitute({
    x <- data.frame(x=1:10)
  }))
}
if (exists(deparse(substitute(myVar)))) {
  rm(myVar)
}
f(myVar)
print(myVar)


(corp2 <- corpus_subset(data_corpus_inaugural, Year>2004))
corp2_para <- corpus_reshape(corp2, to="paragraphs")
corp2_para


print("----")
x <- "abc\nzzz"
cat(x)
x  <- gsub("\n","\r\n",x)
cat(x)



s1 <- "Aaa\n.Bbb\n.Ccc\n"
s2 <- gsub("\n","\r\n",s1)
print(paste("nr chars change after gsub",nchar(s2)-nchar(s1)))
cat(s2)
corp <- corpus(c(s1,s2))
corp_para <- corpus_reshape(corp, to="paragraphs")
summary(corp)
summary(corp_para)
corp_sent <- corpus_reshape(corp, to="sentences")
summary(corp_sent)

