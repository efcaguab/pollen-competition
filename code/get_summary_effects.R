
# calculate the effect that each term has on the quantity and quality of pollination
get_summary_effects <- function(tidied_fixed){
  tidied_fixed %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::group_by(pollen_category, scale, fixed_formula, term, model) %>%
    dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by(pollen_category, scale, fixed_formula, term) %>%
    dplyr::sample_n(size = 999, replace = TRUE) %>% 
    dplyr::mutate(sample_n = 1:n()) %>% 
    dplyr::select(-model) %>%
    tidyr::spread(key = pollen_category, value = estimate) %>% 
    dplyr::mutate(quantity = conspecific, 
                  quality = conspecific - heterospecific) %>% 
    dplyr::group_by(scale, fixed_formula, term) %>%
    dplyr::summarise_at(dplyr::vars(quantity, quality), dplyr::funs(freq_t, parametric_t, median))
}

# see if the quantiles of a vector cross zero in a parametric way
parametric_t <- function(x){
  quant <- quantile(x, probs = c(0.05, 0.95))
  if (0 > quant[1] & 0 < quant[2]) {
    return(0)
  } else if (0 < quant[1]) {
    return(1)
  } else {
    return(-1)
  }
}

# see if the mean is different to zero using a t-test
freq_t <- function(x){
  t1 <- t.test(x, alternative = "two.sided")
  if (t1$p.value > 0.05) {
    return(0)
  } else {
    t2 <- t.test(x, alternative = "greater")
    if (t2$p.value < 0.05) {
      return(1)
    } else {
      return(-1)
    }
  }
}
