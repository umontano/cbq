##################################################
##################################################
## CONSTANT DEFINITIONS
##################################################
##################################################
fnargs_string_lm <- 'list(formula = as.formula(paste(each_response_name, \'~\', each_predictor_name)), data = na.omit(modelee_dataset[c(each_response_name, each_predictor_name)]))'
fnargs_string_correlation <- 'list(formula = as.formula(paste(each_response_name, \'~\', each_predictor_name)), data = na.omit(modelee_dataset[c(each_response_name, each_predictor_name)]))'

##################################################
##################################################
## GENERATE LIST OF LIST OF MODELS
##################################################
##################################################
bayes_factor_regression_models <- function(modelee_dataset, each_response_name, each_predictor_name, ...)
{
	regression_formula <- paste(each_response_name, '~', each_predictor_name) |> as.formula()
	regressionBF(formula = regression_formula, data = modelee_dataset)
}
bayes_factor_correlation_models <- function(modelee_dataset, each_response_name, each_predictor_name, ...)
{
	correlationBF(modelee_dataset[[each_response_name]] , modelee_dataset[[each_predictor_name]])
}
bayes_factor_ttest_models <- function(modelee_dataset, each_response_name, each_predictor_name, ...)
{
	ttestBF(modelee_dataset[[each_response_name]] , modelee_dataset[[each_predictor_name]])
}
bayes_factor_linearmodel_models <- function(modelee_dataset, each_response_name, each_predictor_name, ...)
{
	lm_formula <- paste(each_response_name, '~', each_predictor_name) |> as.formula()
	lmBF(formula = lm_formula, data = modelee_dataset)
}



##################################################
##################################################
## FUNCTIONS TO EXTRACT THE INFORMATIVE COEFFIECIETRS OR PARAMETERS FROM THE FITTED MODES
##################################################
##################################################

recursively_extract_parameters_from_model_fits_list_of_list <- function(resulting_fits_unlistee_list_of_lists, significance_threshold = 0.05)
{
	## DECISIOPN ON TH KIND OF FIT ED MODEL LM OR BF
	## FIRST SET THE BOOLEAN FLAG FOR BAYES FACTOR
	single_fit_model_output_extracting_fn <- ifelse(any(grepl('BF', class(resulting_fits_unlistee_list_of_lists[[1]][[1]]))), bf_model_boolean_significance_evaluator_fn, lm_model_boolean_significance_evaluator_fn)
	## INITIALIZE
	significant_analyses_name_pairs_list <<- NULL
	parameters_matrix <- sapply(names(resulting_fits_unlistee_list_of_lists), \(each_response_name_in_list) sapply(names(resulting_fits_unlistee_list_of_lists[[each_response_name_in_list]]), \(each_predictor_name_in_subitem)
	{
		#paste(each_response_name_in_list, each_predictor_name_in_subitem)
		single_fit_to_extract_pars <- resulting_fits_unlistee_list_of_lists[[each_response_name_in_list]][[each_predictor_name_in_subitem]]
		class(single_fit_to_extract_pars)
		if(length(single_fit_to_extract_pars) < 1) return(NULL)
		else 
		{
			parameter_value <- single_fit_model_output_extracting_fn(single_fit_to_extract_pars, significance_threshold)
			if(!is.na(parameter_value)) 
			{
				name_pair <- paste(each_response_name_in_list, '____', each_predictor_name_in_subitem)
				print(name_pair)
				print(parameter_value)
				significant_analyses_name_pairs_list[[name_pair]] <<- c(each_response_name_in_list, each_predictor_name_in_subitem)
			}
			parameter_value
		}
	}))
	print(parameters_matrix)
	significant_analyses_name_pairs_list
}


lm_model_boolean_significance_evaluator_fn <- function(extractee_single_model, significance_threshold = 0.05)
{
	betas_matrix <- summary(extractee_single_model)[['coefficients']]
	beta_value <- betas_matrix[2, 'Pr(>|t|)']
	if(beta_value > significance_threshold) return(NA)
	else beta_value
}


#bf_slot_bfvalue_item_from_single_bf_model <- function(extractee_single_model)
bf_model_boolean_significance_evaluator_fn <- function(extractee_single_model, significance_threshold = 3.9)
{
	bf_matrix <- extractBF(extractee_single_model)
	bf_value <- bf_matrix[['bf']]
	if(bf_value < significance_threshold) return(NA)
	else bf_value
}



## EMBEDDED FUNCTION TO BE CALLED
## FUCNTIONAL PROGRAMMIND DEFINITIOPN
functionalled_model_fitting_fn <- function(modelee_dataset, each_response_name, each_predictor_name, model_fitting_function = lm, fn_arguments_list = 'list(formula = as.formula(paste(each_response_name, \'~\', each_predictor_name)), data = na.omit(modelee_dataset[c(each_response_name, each_predictor_name)]))')
{
	do.call(model_fitting_function, eval(parse(text = fn_arguments_list)))
}


##################################################
## MOVE THIS FUNCTION TO THE VERY END OF FUNCITON DEFINITIONS
## MAIN NESTED MAPPER
## FUNCTION TO MAKE EMBEDDED MAPPINGS for generatin list of bayeesian models
model_fitting_nested_mapper_main_fn <- function(modelee_dataset, responses_variables_names, predictors_variables_names, model_fitting_fn = lm, fn_arguments_list = 'list(formula = as.formula(paste(each_response_name, \'~\', each_predictor_name)), data = na.omit(modelee_dataset[c(each_response_name, each_predictor_name)]))')
{
	sapply(responses_variables_names,
	       \(each_response_name) sapply(predictors_variables_names,
		\(each_predictor_name) functionalled_model_fitting_fn(modelee_dataset, each_response_name, each_predictor_name, model_fitting_fn, fn_arguments_list), simplify = FALSE, USE.NAMES = TRUE), simplify = FALSE, USE.NAMES = TRUE)
}





## SHOW THE MOST SIGNIFICANT VALUES
## CHECK THA GIVEN LIST OF NAMES
show_variables_of_interest_fn <- function(significat_names_pairs_list, resulting_fits_unlistee_list_of_lists, significance_threshold = 0.05, varnames_of_interest_vector = NULL)#, bayes_factor_boolean_flag = FALSE)
{
	## DECISIOPN ON TH KIND OF FIT ED MODEL LM OR BF
	## FIRST SET THE BOOLEAN FLAG FOR BAYES FACTOR
	bayes_factor_boolean_flag <- any(grepl('BF', class(resulting_fits_unlistee_list_of_lists[[1]][[1]])))
	single_fit_model_output_extracting_fn <- ifelse(bayes_factor_boolean_flag, bf_model_boolean_significance_evaluator_fn, lm_model_boolean_significance_evaluator_fn)
	if(length(varnames_of_interest_vector) > 0) sapply(varnames_of_interest_vector, \(each_interestingvar)
	{
		cat('____ ', each_interestingvar, ' _VAR_OF_INTEREST____', '\n')
		each_interestingvar |> grep(significat_names_pairs_list, value = TRUE) |> print()
		cat('____ends_ ', each_interestingvar, ' _VAR_OF_INTEREST____', '\n')
	})
	## ORDER SIGNIDIVATNTS
	if(length(significat_names_pairs_list) < 1) cat('____NO_SIGNIFICANT_ANALYSES_FOUND____')
	else
	significat_names_pairs_list |> sapply(\(.x) single_fit_model_output_extracting_fn(resulting_fits_unlistee_list_of_lists[[.x[1]]][[.x[2]]], significance_threshold)) |> sort(decreasing = !bayes_factor_boolean_flag) |> print()
}


## TAKE TE PARAMETERS FROM THE MAPPER
#MODEL_FITTING_NESTED_MAPPER_MAIN_FN <- FUNCTION(MODELEE_DATASET, RESPONSES_VARIABLES_NAMES, PREDICTORS_VARIABLES_NAMES, MODEL_FITTING_FN = LM, FN_ARGUMENTS_LIST = 'LIST(FORMULA = AS.FORMULA(PASTE(EACH_RESPONSE_NAME, \'~\', EACH_PREDICTOR_NAME)), DATA = MODELEE_DATASET)')
analyses_embedded_mapper_and_variables_of_interest_shower_main_fn <- function(modelee_dataset, responses_variables_names, predictors_variables_names, model_fitting_fn = lm, fn_arguments_list = 'list(formula = as.formula(paste(each_response_name, \'~\', each_predictor_name)), data = na.omit(modelee_dataset[c(each_response_name, each_predictor_name)]))', significance_threshold = 0.05, varnames_of_interest_vector = NULL)
{
	resulting_fits_unlistee_list_of_lists <- model_fitting_nested_mapper_main_fn(modelee_dataset, responses_variables_names, predictors_variables_names, model_fitting_fn, fn_arguments_list)
	## DECISIOPN ON TH KIND OF FIT ED MODEL LM OR BF
	single_fit_model_output_extracting_fn <- ifelse(any(grepl('BF', class(resulting_fits_unlistee_list_of_lists[[1]][[1]]))), bf_model_boolean_significance_evaluator_fn, lm_model_boolean_significance_evaluator_fn)
	significat_names_pairs_list <- recursively_extract_parameters_from_model_fits_list_of_list(resulting_fits_unlistee_list_of_lists, significance_threshold)
	show_variables_of_interest_fn(significat_names_pairs_list, resulting_fits_unlistee_list_of_lists, significance_threshold, varnames_of_interest_vector)
}



compute_outliers <- function(dataframe)
{
  #outliers <- list()
  #for (col in names(dataframe)) 
lapply(names(dataframe), \(col)
  {
	  copy_of_column <- dataframe[[col]]
    Q1 <- quantile(dataframe[[col]], 0.25)
    Q3 <- quantile(dataframe[[col]], 0.75)
    IQR <- Q3 - Q1
    outlier_indices <- which(dataframe[[col]] < (Q1 - 1.5 * IQR) | dataframe[[col]] > (Q3 + 1.5 * IQR))
    cat('____indices_', outlier_indices, '____\n')
    copy_of_column[outlier_indices]
  dataframe[[col]][dataframe[[col]] < lower_bound | dataframe[[col]] > upper_bound] <- NA
  })
  #return(outliers)
}


single_column_outlier_replace <- function(dataframe, column)
{
  Q1 <- quantile(dataframe[[column]], 0.25)
  Q3 <- quantile(dataframe[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  dataframe[[column]][dataframe[[column]] < lower_bound | dataframe[[column]] > upper_bound] <- NA
  dataframe[[column]]
}
outliers_na_replace <- function(dataframe)
{
	sapply(names(dataframe), \(each_column_name) single_column_outlier_replace(dataframe[each_column_name], each_column_name)) |> as.data.frame()
}



remove_outliers <- function(dataframe) {
  cleaned_data <- dataframe

  for (col in names(dataframe)) {
    if (is.numeric(cleaned_data[[col]])) {
      Q1 <- quantile(cleaned_data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(cleaned_data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1

      # Identify outliers
      outlier_condition <- cleaned_data[[col]] < (Q1 - 1.5 * IQR) | cleaned_data[[col]] > (Q3 + 1.5 * IQR)

      # Replace outliers with NA
      cleaned_data[[col]][outlier_condition] <- NA
    }
  }
  return(cleaned_data)
}


compute_outliers <- function(dataframe)
{
	   outliers <- list()
  for (col in names(dataframe)) {
	      Q1 <- quantile(dataframe[[col]], 0.25)
      Q3 <- quantile(dataframe[[col]], 0.75)
          IQR <- Q3 - Q1
          outlier_indices <- which(dataframe[[col]] < (Q1 - 1.5 * IQR) | dataframe[[col]] > (Q3 + 1.5 * IQR))
	      outliers[[col]] <- outlier_indices
	    }
    return(outliers)
}


## impute by replacing na with median
single_column_outlier_na_by_median_replace <- function(dataframe, column)
{
	col_median <- median(dataframe[[column]], na.rm = TRUE)
	condition_selector <- is.na(dataframe[[column]])
	dataframe[[column]][condition_selector] <- col_median
	dataframe[[column]]
}
impute_dataset_outliers_na_by_median_replace <- function(dataframe)
{
	sapply(names(dataframe), \(each_column_name) single_column_outlier_na_by_median_replace(dataframe[each_column_name], each_column_name)) |> as.data.frame()
}
