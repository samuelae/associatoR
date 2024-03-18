
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
  if(length(unique(x)) == 2 & length(unique(y)) == 2) {
    tab = table(x, y) %>% as.matrix()
    num = tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]
    denom = sqrt(prod(c(rowSums(tab), colSums(tab))))
    num / denom
  } else {
    NA
  }
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
  if(!"target_embedding" %in% names(data)) {
    stop("No target embeddings found. Embed targets using ar_embed_targets().")
  }
}


check_tidy = function(data, var, data_label = "data") {
  if(!missing(var)) {
    test = try(rlang::eval_tidy(var, data))
    if(class(test) == "try-error") stop(paste0("Cannot find one or more columns ", rlang::as_label(var), " in ", data_label, "."))
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

triangle_sim = function(x,y,method="spearman"){
  x = x[upper.tri(x)]
  y = y[upper.tri(y)]
  cor(x, y, method = method)
  }

row_sim = function(x,y,method="spearman"){
  corrs = numeric(nrow(x))
  for(i in 1:nrow(x)){
    corrs[i] = cor(x[i,-i], y[i,-i], method = method)
    }
  mean(corrs, na.rm=T)
  }

# STRING CASING -------

to_frequent <- function(string) {
  str <- tibble::tibble(original = string,
                        lower = tolower(string))
  lookup <- str %>%
    dplyr::count(original) %>%
    dplyr::mutate(lower = tolower(original)) %>%
    dplyr::group_by(lower) %>%
    dplyr::slice_max(n) %>%
    dplyr::slice(1) %>%
    dplyr::select(lower, most_frequent = original)
  out <- str %>%
    dplyr::left_join(lookup, by = "lower") %>%
    dplyr::pull(most_frequent)

  out
}

# OTHER --------

get_id = function(x, y) ifelse(x<y,paste(x,y,sep="_"),paste(y,x,sep="_"))










