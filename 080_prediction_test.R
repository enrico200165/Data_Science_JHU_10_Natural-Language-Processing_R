

setwd("./04_shiny_prediction")
source("./08_prediction.R")


#pred_successors_aggregate(c("your","time","on"),F,  5)
ret <- pred_successors_aggregate(c("your","time","on"),F,  2)
s <- result_lines_html(ret)


pred_successors(c("asas","sdsds","your","time","on"),F,  5)


pred_successors(c("a","baby","was","sdsds"),F,  5)


ret <- pred_successors(c("a","baby"),F,  5)
print_pred_result(ret)



ret <- pred_successors(c("sss","asdasda"),F,  5)
print_pred_result(ret)


ret <- pred_successors(c("a£$%£%","%&//"),F,  5)
print_pred_result(ret)


ret <- pred_successors(c("the","usual"),F,  5)
print(ret)


ret <- pred_successors(c("a£$%£%","usual"),F,  5)
print(ret)


ret <- pred_successors(c("a£$%£%","%&//"),F,  5)
print(ret)

pred_successors(NULL, F,  5)
print(ret)

# to make 2pred fail and fall down to 1 prede
predecessors2_nosecond <- setdiff(
  ngrams_freqs[[2]][["primo"]],ngrams_freqs[[3]][["secondo"]])


ret <- pred_successors(c("and","the"), F,5)
print(ret)

ret <- pred_successors(c("and","you"), F,5)
print(ret)

ret <- pred_successors(c("you","are"), F,5)
print(ret)

# last predecessor fails

ret <- pred_successors(c("the","usual"), F,5)
print(ret)

print(format(object.size(ngrams_freqs), units = "MB"), F,5)

print(format(object.size(ngrams_freqs[[3]]), units = "MB"), F,5)

# ------------------ IMPORTANT -----------------------------
setwd("..")
print(getwd())
