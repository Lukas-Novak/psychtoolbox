# # this function introduces meeasurement invariance across multiple groups
# # multi.group.MI.R
#
# # this function was edited because for loop is not giving the result after each interation either in the data frame format or list. For this reason, it was necessary to print results into txt output as defined by the function below. This is currently the only way how to save the results.
#
# # Another problem to fix is that number of resulting dataframes exides the number of groups intended to compare, in more detail, there are DUPLICATED comparisons which should be removed from resulting dataset.
#
# data.to.multipe.group.MI = filter(data, !is.na(GSES) & !is.na(Culture_type) & Culture_type %in% "Individualistic_culture") %>%
#   mutate(broker_assignement = as.factor(broker_assignement))
#
# # making this variable dupliacated in dataset
# group.with.mult.levels = "broker_assignement"
#
# levels.of.g.var = data.to.multipe.group.MI %>%
#   mutate_if(is.factor, droplevels) %>%
#   select(all_of(group.with.mult.levels)) %>%
#   summarise_if(is.factor, levels) %>%
#   mutate(group.var2 = str_c(.[[1]])) %>%
#   summarise(tidyr::expand_grid(.[[1]], .[[2]], .name_repair = "unique")) %>%
#   filter(!.[[1]] == .[[2]]) %>%
#   rename_all(~c("group.var1","group.var2")) %>%
#   mutate(comb.group.var = paste0(group.var1,"|",group.var2)) %>%
#   select(comb.group.var) %>%
#   as.matrix() %>%
#   as.character()
#
# # data.to.multipe.group.MI %>% filter(if_any(starts_with(paste(group.with.mult.levels)), ~ str_detect(., paste(as.character(list.of.groups[[1]]), collapse ="|"))))
#
# list.of.dfs = list()
#
# for (i in levels.of.g.var) {
#   list.of.dfs[[i]] <-
#     data.to.multipe.group.MI %>%
#     filter(if_any(starts_with(paste(group.with.mult.levels)), ~ str_detect(., i))) %>%
#     mutate_if(is.factor, droplevels)
#
# }
#
#
# # this function was edited because for loop is not giving the result after each interation either in the data frame format or list. For this reason, it was necessary to print results into txt output as defined by the function below. This is currently the only way how to save the results.
#
# # Another problem to fix is that number of resulting dataframes exides the number of groups intended to compare, in more detail, there are DUPLICATED comparisons which should be removed from resulting dataset.
#
# two.group.invar = function(group1_nam, group2_nam, ordered, model, data, std.lv, meanstructure, group, drop_yes_no_results, yes_no_results, names) {
#   if(!is.factor(data[[group]])) stop("The ´group´ argument has to be type factor" ) # group has to be factor in order to correctly name g.1 and g.2
#   if(!is.data.frame(data)) stop('data must be a data frame')
#
# meas.invar.cfa=cfa(model, data = data, ordered = ordered, std.lv= std.lv,
#                          meanstructure = meanstructure)
#
# tab.fit = matrix(nrow = 7, ncol = 10)
#
# colnames(tab.fit)=c("Model","x2","df","pvalue","CFI","TLI","rmsea","rmsea.ci.lower", "rmsea.ci.upper", "SRMR")
#
# tab.fit[1, ] = c("Overall model", round(fitMeasures(meas.invar.cfa,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
#
#
# # results of this function are the same as from the lavaan
# mult.group.m = eqMI.main(model = model,
#                          data = data,
#                          group = group,
#                          meanstructure = meanstructure,
#                          output = "both",
#                          equivalence.test = T,
#                          adjRMSEA = T,
#                          projection = T,
#                          ordered = ordered)
#
# # male table
# tab.fit[2, ] = c(group1_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g1,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
# # female table
# tab.fit[3, ] = c(group2_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g2,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
#
# # Configure invariance
# tab.fit[4, ] = c("Configural  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.combine.groups,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
# # Metric invariance
# tab.fit[5, ] = c("Metric  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.metric,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
# # Scalar invariance
# tab.fit[6, ] = c("Scalar  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.scalar,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
# # Strict (error) invariance
# tab.fit[7, ] = c("Strict  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.strict.residuals,
#                                                     c('chisq', 'df', 'pvalue',
#                                                    'cfi',"tli",'rmsea',"rmsea.ci.lower",
#                                                    "rmsea.ci.upper", 'srmr')), digits = 3))
#
# # mult.group.m$eqMI.stat # checking chi-square
#
# tab.fit = tab.fit %>%
#   as_tibble() %>%
#   mutate(rmsea = paste0(rmsea, " ","(",rmsea.ci.lower,"-",rmsea.ci.upper,")")) %>%
#   rename(
#          "RMSEA (90% CI)" = "rmsea") %>%
#   select(!starts_with(c("rmsea.ci.","rmsea.ci.lower","rmsea.ci.upper")))
#
# cfi.dif = function(x) {
#   if(!is.data.frame(x)) stop('x must be a data frame')
#   if(length(x) < 1) stop('must be higher length than 1')
#   if(!sum(str_detect(names(x),"CFI"))) stop("There must be a column with CFI values")
#   x = x %>%
#     tibble("CFI.dif" = c(NA, round(diff(as.numeric(x$CFI)),digits = 3))) %>%
#     relocate("CFI.dif", .after = "CFI")
#   if(yes_no_results == TRUE) {
#   x = x %>% mutate("mod.dif" = ifelse(CFI.dif > 0.01, "Yes", "No"))
#   }
#   if(any(names(x) == 'mod.dif')) {x = x %>% rename("Model difference - CFI" = "mod.dif")
#   }
#   x = x %>%
#     rename("CFI difference" = "CFI.dif")
#   print(x)
#   }
# tab.fit = tab.fit %>%
#   cfi.dif() %>%
#   add_row() %>%
#    write.table(file = paste0("estimates_",names, "_.txt"), quote=FALSE,
#                sep=" ", dec=".", na=" ", row.names=TRUE, col.names=TRUE, append = T)
# }
#
# sers = as.list(levels.of.g.var)
#
# for(i in list.of.dfs) {
# df=  two.group.invar(data=filter(i, !is.na(GSES) & !is.na(Culture_type)),
#                                       ordered = T,
#                                       std.lv = T,
#                                       group = "broker_assignement",
#                                       group1_nam = "Collectivistic",
#                                       group2_nam = "Individualisic",
#                                       meanstructure = T,
#                                       model = modified.two.fac.mod,
#                                       yes_no_results = T, names = "g")
#
# }
