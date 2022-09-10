# extract.parametrs = function(fit, name_of_model, 
#                              parameters.to.extract = c('chisq', 'df', 'pvalue',
#                                                        'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                        "rmsea.ci.upper", 'srmr')) {
#   if (exists("mod.fit.indicies")) {
#     mod.fit.indicies = c(name_of_model, as.table(fitMeasures(fit)[parameters.to.extract]) %>% 
#                       round(digits = 3)) %>% 
#     t() %>% 
#       as_tibble() %>% 
#       rename("Model" = "V1") %>% 
#   rbind(mod.fit.indicies) 
#   return(mod.fit.indicies)
#   } 
#   if (!exists("mod.fit.indicies")) {
#       mod.fit.indicies = c(name_of_model, as.table(fitMeasures(fit)[parameters.to.extract]) %>% 
#                       round(digits = 3)) %>% 
#     t() %>% 
#         as_tibble() %>% 
#         rename("Model" = "V1")
#       return(mod.fit.indicies)
#   }
#   if (sum(duplicated(mod.fit.indicies$Model)) >= 1) stop("mode")
# } 
#
# mod.fit.indicies = extract.parametrs(fit = SOCS.S.4.fac.mod.sem, name_of_model = "Pokus")
# 
# mod.fit.indicies
# 
# rm(mod.fit.indicies)
