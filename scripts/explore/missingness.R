
# missingness of indigenous status
fn_miss <- load_concepts(c("indig", "adm_year", "country"), "anzics")
ggplot(
  fn_miss[country == "AU", mean(!is.na(indig)), by = "adm_year"],
  aes(x = adm_year, y = V1)
) + geom_col() + theme_bw()