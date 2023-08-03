## Nutrition sensitive agriculture model 
## Should farm household grow and raise for sustainable food in Ba Vi district, Hanoi

 library(decisionSupport)
 
 make_variables <- function(est, n = 1)
 {x <- decisionSupport::random(rho = est, n = n)
 for(i in colnames(x)) assign(i, as.numeric(x[1,i]),
                               envir = .GlobalEnv)}

 make_variables(estimate_read_csv(paste("NSA_input_table.csv")))

 ## Model
 Nutrition_sensitive_function <- function(x,varnames){
 
# For the farmer to get started they may need to buy land and cows etc.   
   
   # chance that they need land
   total_land_cost <- chance_event(chance = need_to_buy_land, 
                             value_if = buying_land, 
                             value_if_not = 0)
   
   # chance that they need livestock
   total_livestock_cost <- chance_event(chance = need_to_buy_livestock, 
                                   value_if = livestock_purchase, 
                                   value_if_not = 0)
   
   
   establishment_cost <- training_cost +
     total_land_cost +
     equipment_cost +
     total_livestock_cost 
   
 ## Cost###
  annual_cost <- land_taxes + 
   labor_cost +
   organization_time_cost + # opportunity cost of HH time investment
   irrigation_cost +
   electricity_cost +
   fertilizer_cost +
   planting_material_cost + # seeds, seedlings etc.
   equipment_upkeep_cost +
   animal_feed +
   animal_health_care 
  
  ## Final cost of each year
 final_cost <- vv(var_mean = annual_cost, 
            var_CV = CV_value, 
            n = number_of_years)
 
 ## Final costs of all years
 # assign establishment costs to the first year
 final_cost[1] <- establishment_cost + annual_cost
 
## Benefits

 # After training how well do households do in managing the intervention
 knowledge <- vv(var_mean = training_efficacy, 
                 var_CV = CV_value, 
                 n = number_of_years, 
                 relative_trend = learning_effect)

 ### Still working on this part #####
 
 #  technical + 
 #   food_prep + 
 #   animal_raising + 
 #   crop_cultivation 
 # 
 # 
 # yield
 # sale
 # income
 # 
 # relationship, exchange
 # 
 # 
 # healthy_diet
 # healthy_diet_children
 # healthy_diet_elderly
 # healthy_diet_mothers
 # 
 # sustianable_food
 # 
 # diversity
 # convenient
 # time_savings_on_shopping
 # time_savings_on_travel
 # money_savings
 # 
 # green_space
 # 
 # gender
 # women_status_improved
 # 
 # taste
 # 
   
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
   model_function = Nutrition_sensitive_function,
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
 