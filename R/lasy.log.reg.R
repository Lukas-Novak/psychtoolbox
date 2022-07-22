#......................................................................
# Building lasy regression with shiny extension
#......................................................................
# next steps are:
#  - compute confidence intervals 
#  - convert log odds to odds ratios
#  - merge crude and adjusted results into single dataframe 

pokus.var = c("family_status","Gender")
indep.var = c("TEQ","Age")
covariates = c("ethnicity","education")
dat = "data.PAQ"
models.adj= list()
models.crude= list()
print.cov = FALSE 

for (dep.var in  pokus.var) {
  data.func = get(dat)
  # crude effect regression
models.crude[[dep.var]] <- glm(as.formula(paste(dep.var,"~",paste(c(indep.var), collapse="+"))), data = data.func, family = "binomial")
models.crude[[dep.var]] <- summary(models.crude[[dep.var]])
models.crude[[dep.var]] <- models.crude[[dep.var]]$coefficients
models.crude[[dep.var]]<- cbind(models.crude[[dep.var]], adj_pval = NA_character_) %>% 
  as_tibble(models.crude[[dep.var]], rownames = "Var") 
 for (i in seq_along(models.crude)) {
   models.crude[[i]]$adj_pval <- p.adjust(as.numeric(models.crude[[i]]$`Pr(>|z|)`,method = "BH", n = i))
   print(models.crude)
 }
 print(models.crude)
}


for (dep.var in  pokus.var) {
  data.func = get(dat)
  # adj effect regression
models.adj[[dep.var]] <- glm(as.formula(paste(dep.var,"~",paste(c(indep.var, covariates), collapse="+"))), data = data.func, family = "binomial")
models.adj[[dep.var]] <- summary(models.adj[[dep.var]])
models.adj[[dep.var]] <- models.adj[[dep.var]]$coefficients
models.adj[[dep.var]]<- cbind(models.adj[[dep.var]], adj_pval = NA_character_) %>% 
  as_tibble(models.adj[[dep.var]], rownames = "Var") 
if(print.cov == FALSE) {
  models.adj[[dep.var]] <- models.adj[[dep.var]] %>% 
  filter(if_any(everything(.),  ~str_detect(., paste(indep.var, collapse = "|")))) 
  }
 for (i in seq_along(models.adj)) {
   models.adj[[i]]$adj_pval <- p.adjust(as.numeric(models.adj[[i]]$`Pr(>|z|)`,method = "BH", n = i))
   print(models.adj)
 }
 print(models.adj)
}
