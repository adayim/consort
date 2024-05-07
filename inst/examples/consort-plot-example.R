
## Prepare test data
data(dispos.data)

df <- dispos.data[!dispos.data$arm3 %in% "Trt C", ]
p <- consort_plot(data = df,
                  orders = list(c(trialno = "Population"),
                                c(exclusion = "Excluded"),
                                c(arm     = "Randomized patient"),
                                c(arm3     = "", 
                                  subjid_notdosed="Participants not treated"),
                                c(followup    = "Pariticpants planned for follow-up",
                                  lost_followup = "Reason for tot followed"),
                                c(assessed = "Assessed for final outcome"),
                                c(no_value = "Reason for not assessed"),
                                c(mitt = "Included in the mITT analysis")),
                  side_box = c("exclusion", "no_value"),
                  allocation = c("arm", "arm3"),
                  labels = c("1" = "Screening", "2" = "Randomization",
                             "5" = "Follow-up", "7" = "Final analysis"),
                  cex = 0.7)
