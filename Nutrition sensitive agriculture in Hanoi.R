## Nutrition sensitive agriculture model 
## Should farm household grow and raise for sustainable food in Ba Vi district, Hanoi

 library(decisionSupport)
 
 make_variables <- function(est, n = 1)
 {x <- decisionSupport::random(rho = est, n = n)
 for(i in colnames(x)) assign(i, as.numeric(x[1,i]),
                               envir = .GlobalEnv)}

 make_variables(estimate_read_csv(paste("NSA_input_table.csv")))

 ## Model
 Nutrion_sensitive_function <- function(x,varnames){
 
 ## Cost###
 
   final_cost <- vv(cost, var_CV = CV_value, n = number_of_years)
   
   final_benefit <- vv(benefit, var_CV = CV_value, n = number_of_years)
   
   outcome <- final_benefit - final_cost
   
   NPV_NSA_intervention <-
     discount(x= outcome,
              discount_rate = discount_rate,
              calculate_NPV = TRUE)
   
   return(list(NPV_NSA_intervention = NPV_NSA_intervention,
               Cashflow = outcome))
 }
 household_decision_results <-mcSimulation(
   estimate = estimate_read_csv("NSA_input_table.csv"),
   model_function = Nutrion_sensitive_function,
   numberOfModelRuns = 1000,
   functionSyntax = "plainNames"
 ) 
 plot_distributions(mcSimulation_object = household_decision_results,
                    vars = "NPV_NSA_intervention",
                    method = "smooth_simple_overlay",
                    base_size = 10)

 mcSimulation_table <- data.frame(household_decision_results$x,
                                  household_decision_results$y[1:1])
   
 EVPI <- multi_EVPI(mc = mcSimulation_table,
                    first_out_var = "NPV_NSA_intervention")

 plot_evpi(EVPI, decision_vars = "NPV_NSA_intervention") 
 