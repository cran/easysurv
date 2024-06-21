## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

library(easysurv)
library(cli)

## ----installation, eval=FALSE-------------------------------------------------
#  # First install 'pak' if you haven't already.
#  install.packages("pak")
#  
#  # Then, install easysurv either from GitHub for the latest version:
#  pak::pkg_install("Maple-Health-Group/easysurv")
#  
#  # Or from CRAN for the latest stable version:
#  pak::pkg_install("easysurv")

## ----initiate-session, eval = FALSE-------------------------------------------
#  # Start from a clean environment
#  rm(list = ls())
#  
#  # Attach the easysurv package
#  library(easysurv)
#  
#  # (Optional) load an easysurv analysis template
#  quick_start()

## ----data-import, eval=TRUE---------------------------------------------------
surv_data <- easy_adtte

surv_data

## ----data-structure, eval=TRUE------------------------------------------------
surv_data <- surv_data |>
  dplyr::filter(PARAMCD == "PFS") |> # Filtering may be relevant for your data
  dplyr::mutate(
    time = AVAL,
    event = 1 - CNSR, # Recode status to 0 = censored, 1 = event
    group = TRT01P
  ) |>
  dplyr::mutate_at("group", as.factor) |> # Convert to factor for easier stratification
  dplyr::as_tibble() # Convert to tibble for easier viewing

surv_data

## ----data-labelling, eval=TRUE------------------------------------------------
# Check labels impacted by re-coding
attr(surv_data$event, "label")
# Check levels of the group factor variable
levels(surv_data$group)
# Overwrite the attributes with new labels
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"
levels(surv_data$group) <- c("Tab+Vis", "Tab->Vis", "Tab", "Vis")

## ----inspect, eval=TRUE-------------------------------------------------------
inspect_surv_data(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

## ----km, eval=TRUE, fig.width=6, fig.height=5---------------------------------
km <- get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

km

## ----km-names, eval=TRUE, fig.width=6, fig.height=5---------------------------
km_with_names <- get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group",
  risktable_symbols = FALSE
)

km_with_names$km_plot

## ----ph, eval=TRUE, fig.width=6, fig.height=5---------------------------------
ph <- test_ph(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

ph

## ----parsnip, eval=FALSE------------------------------------------------------
#  # We created a function to return NULL if issues arise in model fitting.
#  pfit <- purrr::possibly(.f = parsnip::fit)
#  
#  # Without easysurv, here's how parsnip might be used to fit models:
#  parsnip::survival_reg(dist = "weibull") |>
#    parsnip::set_engine("flexsurv") |>
#    parsnip::fit(
#      formula = survival::Surv(time, event) ~ group,
#      data = surv_data
#    )
#  
#  # But, in easysurv, the fit_models() function uses pfit() to handle errors.
#  # This looks a bit like:
#  parsnip::survival_reg(dist = "weibull") |>
#    parsnip::set_engine("flexsurv") |>
#    pfit(
#      formula = survival::Surv(time, event) ~ group,
#      data = surv_data
#    )

## ----fit-models-fail, eval=TRUE, warning=FALSE--------------------------------
# Take just two rows of data and expect distributions to fail.
lacking <- surv_data[3:4, ]

suspected_failure <- fit_models(
  data = lacking,
  time = "time",
  event = "event",
  dists = c("exp", "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull")
)
print(suspected_failure)

## ----fit-models, eval=TRUE----------------------------------------------------
models <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group"
)

models

## ----fit-models-joint, eval=TRUE----------------------------------------------
joint_models <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group",
  covariates = "group"
)

joint_models

## ----fit-spline, eval=FALSE---------------------------------------------------
#  spline_models <- fit_models(
#    data = surv_data,
#    time = "time",
#    event = "event",
#    predict_by = "group",
#    engine = "flexsurvspline",
#    k = c(1, 2, 3),
#    scale = "hazard"
#  )

## ----fit-cure, eval=FALSE-----------------------------------------------------
#  cure_models <- fit_models(
#    data = surv_data,
#    time = "time",
#    event = "event",
#    predict_by = "group",
#    engine = "flexsurvcure"
#  )

## ----predict-and-plot, eval=TRUE, fig.width=6, fig.height=5-------------------
# With the "models" object from above...
preds_and_plots <- predict_and_plot(models)

preds_and_plots

## ----export, eval=FALSE-------------------------------------------------------
#  # Create workbook
#  wb <- openxlsx::createWorkbook()
#  
#  # Write easysurv objects to the workbook
#  write_to_xl(wb, km)
#  write_to_xl(wb, ph)
#  write_to_xl(wb, models)
#  write_to_xl(wb, preds_and_plots)
#  
#  # Save and open the workbook
#  openxlsx::saveWorkbook(wb, file = "my_file_name.xlsx", overwrite = TRUE)
#  openxlsx::openXL("my_file_name.xlsx")

