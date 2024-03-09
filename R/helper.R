
# CORRS --------

point_biserial = function(x, d, na.rm = T){
  if(na.rm) {
    sel = !is.na(x) & !is.na(d)
    x = x[sel]
    d = d[sel]
  }
  M = tapply(x,d,mean)
  N = tapply(x,d,length)
  S = sqrt(sum((x - mean(x)) ** 2) / length(x))
  diff(M) / S * sqrt(prod(N) / length(x) ** 2)
}

phi = function(x, y) {
  tab = table(x, y) %>% as.matrix()
  num = tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]
  denom = sqrt(prod(c(rowSums(tab), colSums(tab))))
  num / denom
}

reg_r = function(x, d) {
  sqrt(stats::summary.lm(stats::lm(x ~ d))$r.squared)
}

cramer = function(d1, d2) {
  tab = table(d1, d2) %>% as.matrix()
  norm = (rowSums(tab) %*% t(colSums(tab))) / sum(tab)
  chi = sum((tab - norm) ** 2 / norm)
  sqrt(chi / (sum(tab) * min(nrow(tab) - 1, ncol(tab) - 1)))
}


# CHECKS --------

check_object = function(data) {
  if(class(data)[1] != "associatoR") {
    stop("Data is not of class 'associatoR'. Import the data using ar_import().")
  }
}

check_targets = function(data) {
  if(!"targets" %in% names(data)) {
    stop("No targets found. Set targets using ar_set_targets().")
  }
}

check_embeddings = function(data) {
  if(!"target_embeddings" %in% names(data)) {
    stop("No target embeddings found. Embed targets using ar_embed_targets().")
  }
}


check_tidy = function(data, var) {
  if(!missing(var)) {
    test = try(rlang::eval_tidy(var, data))
    if(class(test) == "try-error") stop(paste0("Cannot find one or all columns ", quo_name(var), " in data."))
  }
}

check_tidy_vld = function(data, var){
  if(!missing(var)){
    test = try(rlang::eval_tidy(var, data))
    class(test) != "try-error"
  }
}


# SIMILARITIES --------

cosine = function(x) {
  cos = (x %*% t(x)) / sqrt(rowSums(x**2) %*% t(rowSums(x**2)))
  rownames(cos) = colnames(cos) = rownames(x)
  cos
  }


arccos_sim = function(x){
  x[x > 1] = 1
  x[x < -1] = -1
  1-acos(x)/pi
  }

