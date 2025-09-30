## THIS IS A SIMPLIFIED AND FUNCTIONALLY PIPED-BASED VERSION OF THE SCRIPT IN # ~/u/temperament_ant_vale_karen_mfserrano/CBQ_comandosSPSS_lab_CHyC.R and in github
## https://raw.githubusercontent.com/umontano/CBQ_comandos_SPSS_lab_ChyC/main/CBQ_comandosSPSS_lab_CHyC.R

create_datasets <- function(questionnaire_filename_or_address)
{
	#LOAD DATA
	cbqfn_raw_questionnaire_answers_dataset <<- read.csv(questionnaire_filename_or_address , header=TRUE, stringsAsFactors = FALSE)
	#CREATE DATAFRAMES
variable_names_cbqfn_item_columns <<- grep('^cbq\\d{1,3}', names(cbqfn_raw_questionnaire_answers_dataset), perl=TRUE, value = TRUE)
variable_names_cbqfn_nonitems_columns <<- setdiff(names(cbqfn_raw_questionnaire_answers_dataset), variable_names_cbqfn_item_columns)
	cbqfn_non_item_columns_dataset  <<- cbqfn_raw_questionnaire_answers_dataset[variable_names_cbqfn_nonitems_columns]
	cbqfn_meanee_only_item_answers_columns_dataset  <<- cbqfn_raw_questionnaire_answers_dataset[variable_names_cbqfn_item_columns] |> sapply(as.numeric) |> as.data.frame()
	# NOTE, CBQ ITEMS ARE COLUMNS 5:199
	#REMOVE UNUSED ITEMS 3, 33, 49
	unused_cbqfn_meanee_only_item_answers_columns_dataset <<- c('cbq3', 'cbq33', 'cbq49')
	cbqfn_meanee_only_item_answers_columns_dataset <<- cbqfn_meanee_only_item_answers_columns_dataset[setdiff(variable_names_cbqfn_item_columns, unused_cbqfn_meanee_only_item_answers_columns_dataset)]
	#REMOVE LESS THAN 1 AND ABOVE 7
	cbqfn_meanee_only_item_answers_columns_dataset[cbqfn_meanee_only_item_answers_columns_dataset<1 | cbqfn_meanee_only_item_answers_columns_dataset>7] <<- NA
	## SET THE ROW NAMES USING THE ID COLUMN
	if(length(grep('^ID', names(cbqfn_raw_questionnaire_answers_dataset), ignore.case = TRUE)) > 0) variable_names_cbqfn_id_columns <<- grep('^ID', names(cbqfn_raw_questionnaire_answers_dataset), ignore.case = TRUE, value = TRUE)[1]
	## SAVE
	#write.csv(cbqfn_non_item_columns_dataset, 'xCBQ_OTROS_DATOS.csv', row.names=TRUE)
	cbqfn_meanee_only_item_answers_columns_dataset
}


test_name_difference <- function(processee_dataset)
{
#names(cbqfn_raw_questionnaire_answers_dataset) |> setdiff(names(cbqfn_meanee_only_item_answers_columns_dataset))
	names_starting_with_cbq <- '^cbq\\d' |> grep(names(processee_dataset), value = T)
	names_starting_with_cbq
	setdiff(names(processee_dataset), names_starting_with_cbq)
	setequal(names(processee_dataset), names_starting_with_cbq)
	setequal(names_starting_with_cbq, names_starting_with_cbq)
}




#IMPUTE MISSING VALUES IN ANY DATASET
impute_any_dataset_mice <- function(imputee_dataset, maximum_iterations=50, number_of_imputations=5, method_string='sample', ridge_decimal=0.001)
{
    library(mice)
    temp_data <- mice(imputee_dataset, m=number_of_imputations, maxit=maximum_iterations, seed = 500, method=method_string, ridge=ridge_decimal)
    complete(temp_data, 1)
}

#ITEMS ARE IMPUTED WITH MICE
mice_imputation_cbqfn_meanee_only_item_answers_columns_dataset <- function(maximum_iterations, number_of_imputations)
{
cbqfn_meanee_only_item_answers_columns_dataset <<- impute_any_dataset_mice(cbqfn_meanee_only_item_answers_columns_dataset, maximum_iterations, number_of_imputations)

#SET THE ROW NAMES USING THE ID COLUMN
row.names(fifteen_scales_dataset)  <<- cbqfn_raw_questionnaire_answers_dataset$identificador
row.names(three_factors_dataset) <<- cbqfn_raw_questionnaire_answers_dataset$identificador
row.names(cbqfn_meanee_only_item_answers_columns_dataset) <<- cbqfn_raw_questionnaire_answers_dataset$identificador
row.names(cbqfn_non_item_columns_dataset) <<- cbqfn_raw_questionnaire_answers_dataset$identificador

#SAVE TO DISK
date_time <<- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')
file_name_cbqfn_meanee_only_item_answers_columns_dataset <<- paste0('xCBQ_IMPUTED_ITEMS', date_time, '.csv')
write.csv(cbqfn_meanee_only_item_answers_columns_dataset, file_name_cbqfn_meanee_only_item_answers_columns_dataset, row.names=TRUE)
}



#==========================================
#==========================================
generate_unreversed_cbqfn_meanee_only_item_answers_columns_dataset  <- function(processee_cbqfn_dataset)
{
	within(processee_cbqfn_dataset, {
cbq41 <- 8-cbq41
cbq88 <- 8-cbq88
cbq102 <- 8-cbq102
cbq123 <- 8-cbq123
cbq126 <- 8-cbq126
cbq145 <- 8-cbq145
cbq192 <- 8-cbq192
cbq19 <- 8-cbq19
cbq120 <- 8-cbq120
cbq156 <- 8-cbq156
cbq131 <- 8-cbq131
cbq175 <- 8-cbq175
cbq188 <- 8-cbq188
cbq191 <- 8-cbq191
cbq38 <- 8-cbq38
cbq47 <- 8-cbq47
cbq171 <- 8-cbq171
cbq195 <- 8-cbq195
cbq6 <- 8-cbq6
cbq95 <- 8-cbq95
cbq184 <- 8-cbq184
cbq5 <- 8-cbq5
cbq101 <- 8-cbq101
cbq190 <- 8-cbq190
cbq14 <- 8-cbq14
cbq53 <- 8-cbq53
cbq68 <- 8-cbq68
cbq150 <- 8-cbq150
cbq167 <- 8-cbq167
cbq15 <- 8-cbq15
cbq58 <- 8-cbq58
cbq70 <- 8-cbq70
cbq138 <- 8-cbq138
cbq161 <- 8-cbq161
cbq176 <- 8-cbq176
cbq30 <- 8-cbq30
cbq51 <- 8-cbq51
cbq60 <- 8-cbq60
cbq159 <- 8-cbq159
cbq71 <- 8-cbq71
cbq79 <- 8-cbq79
cbq90 <- 8-cbq90
cbq137 <- 8-cbq137
cbq169 <- 8-cbq169
cbq183 <- 8-cbq183
cbq32 <- 8-cbq32
cbq93 <- 8-cbq93
cbq108 <- 8-cbq108
cbq162 <- 8-cbq162
cbq12 <- 8-cbq12
cbq66 <- 8-cbq66
cbq86 <- 8-cbq86
cbq111 <- 8-cbq111
cbq28 <- 8-cbq28
cbq83 <- 8-cbq83
cbq122 <- 8-cbq122
cbq142 <- 8-cbq142
cbq170 <- 8-cbq170
cbq72 <- 8-cbq72
cbq109 <- 8-cbq109
cbq112 <- 8-cbq112
cbq149 <- 8-cbq149
cbq17 <- 8-cbq17
cbq23 <- 8-cbq23
cbq45 <- 8-cbq45
cbq57 <- 8-cbq57
cbq119 <- 8-cbq119
cbq129 <- 8-cbq129
cbq158 <- 8-cbq158
cbq43 <- 8-cbq43
cbq99 <- 8-cbq99
cbq84 <- 8-cbq84
cbq121 <- 8-cbq121
cbq135 <- 8-cbq135
cbq165 <- 8-cbq165
	})
}


#################################################################
#CALCULATE REVERSED QUESTIONS
make_reversed_trail_r <- function(non_reversed_dataset)
{
	within(non_reversed_dataset, {
cbq41r <- 8-cbq41
cbq88r <- 8-cbq88
cbq102r <- 8-cbq102
cbq123r <- 8-cbq123
cbq126r <- 8-cbq126
cbq145r <- 8-cbq145
cbq192r <- 8-cbq192
cbq19r <- 8-cbq19
cbq120r <- 8-cbq120
cbq156r <- 8-cbq156
cbq131r <- 8-cbq131
cbq175r <- 8-cbq175
cbq188r <- 8-cbq188
cbq191r <- 8-cbq191
cbq38r <- 8-cbq38
cbq47r <- 8-cbq47
cbq171r <- 8-cbq171
cbq195r <- 8-cbq195
cbq6r <- 8-cbq6
cbq95r <- 8-cbq95
cbq184r <- 8-cbq184
cbq5r <- 8-cbq5
cbq101r <- 8-cbq101
cbq190r <- 8-cbq190
cbq14r <- 8-cbq14
cbq53r <- 8-cbq53
cbq68r <- 8-cbq68
cbq150r <- 8-cbq150
cbq167r <- 8-cbq167
cbq15r <- 8-cbq15
cbq58r <- 8-cbq58
cbq70r <- 8-cbq70
cbq138r <- 8-cbq138
cbq161r <- 8-cbq161
cbq176r <- 8-cbq176
cbq30r <- 8-cbq30
cbq51r <- 8-cbq51
cbq60r <- 8-cbq60
cbq159r <- 8-cbq159
cbq71r <- 8-cbq71
cbq79r <- 8-cbq79
cbq90r <- 8-cbq90
cbq137r <- 8-cbq137
cbq169r <- 8-cbq169
cbq183r <- 8-cbq183
cbq32r <- 8-cbq32
cbq93r <- 8-cbq93
cbq108r <- 8-cbq108
cbq162r <- 8-cbq162
cbq12r <- 8-cbq12
cbq66r <- 8-cbq66
cbq86r <- 8-cbq86
cbq111r <- 8-cbq111
cbq28r <- 8-cbq28
cbq83r <- 8-cbq83
cbq122r <- 8-cbq122
cbq142r <- 8-cbq142
cbq170r <- 8-cbq170
cbq72r <- 8-cbq72
cbq109r <- 8-cbq109
cbq112r <- 8-cbq112
cbq149r <- 8-cbq149
cbq17r <- 8-cbq17
cbq23r <- 8-cbq23
cbq45r <- 8-cbq45
cbq57r <- 8-cbq57
cbq119r <- 8-cbq119
cbq129r <- 8-cbq129
cbq158r <- 8-cbq158
cbq43r <- 8-cbq43
cbq99r <- 8-cbq99
cbq84r <- 8-cbq84
cbq121r <- 8-cbq121
cbq135r <- 8-cbq135
cbq165r <- 8-cbq165
	})
#REMOVE THE NON INVERTED ORIGINAL ITEMS
#trailingr <- gsub('', '', names(cbqfn_meanee_only_item_answers_columns_dataset)[grep('cbq\\d{1,3}r', names(cbqfn_meanee_only_item_answers_columns_dataset), perl=T)])
#cbqfn_meanee_only_item_answers_columns_dataset[, trailingr] <- list(NULL)
}



##################################
#PERFILES
insert_profiles_column_from_factors_cbqfn <- function(processee_cbqfn_dataset)
{
	## DEFINE MEDIANS
	cem  <- median(processee_cbqfn_dataset[['CE']])
	anm  <- median(processee_cbqfn_dataset[['AN']])
	within(processee_cbqfn_dataset, {
		perfil <- NA
		perfil[CE >= cem & AN <  anm] <- 'easy'
		perfil[CE >= cem & AN >= anm] <- 'intense'
		perfil[CE <  cem & AN <  anm] <- 'disengaged'
		perfil[CE <  cem & AN >= anm] <- 'risky'
	})
}



## ONCE THE REVERSED ITEMS WERE COMPUTED ONLY THE SCALES AND FACTORS ARE COMPUTED
insert15_scales_from_items_cbqfn <- function(processee_cbqfn_dataset = cbqfn_raw_questionnaire_answers_dataset)
{
within(processee_cbqfn_dataset, {
#CALCULATE DIMENSIONS
act <- rowMeans(data.frame(cbq1, cbq25, cbq41r, cbq48, cbq88r, cbq102r, cbq123r, cbq126r, cbq145r, cbq153, cbq172, cbq187, cbq192r) , na.rm=TRUE )
fru <- rowMeans(data.frame(cbq2, cbq19r, cbq34, cbq62, cbq73, cbq78, cbq120r, cbq128, cbq140, cbq156r, cbq173, cbq181, cbq193) , na.rm=TRUE )
app <- rowMeans(data.frame(cbq10, cbq24, cbq35, cbq69, cbq82, cbq96, cbq117, cbq131r, cbq148, cbq166, cbq175r, cbq188r, cbq191r) , na.rm=TRUE )
attfoc <- rowMeans(data.frame(cbq16, cbq38r, cbq47r, cbq125, cbq144, cbq160, cbq171r, cbq186, cbq195r) , na.rm=TRUE )
attshi <- rowMeans(data.frame(cbq6r, cbq29, cbq95r, cbq180, cbq184r) , na.rm=TRUE )
#suma de las dos atenciones anteriores
attcon <- rowMeans(data.frame(cbq16, cbq38r, cbq47r, cbq125, cbq144, cbq160, cbq171r, cbq186, cbq195r, cbq6r, cbq29, cbq95r, cbq180, cbq184r) , na.rm=TRUE ) 
dis <- rowMeans(data.frame(cbq5r, cbq21, cbq61, cbq87, cbq97, cbq101r, cbq115, cbq132, cbq141, cbq157, cbq178, cbq190r) , na.rm=TRUE )
sth <- rowMeans(data.frame(cbq14r, cbq27, cbq42, cbq53r, cbq68r, cbq85, cbq92, cbq103, cbq118, cbq134, cbq150r, cbq167r, cbq177) , na.rm=TRUE )
fea <- rowMeans(data.frame(cbq15r, cbq40, cbq50, cbq58r, cbq70r, cbq80, cbq91, cbq130, cbq138r, cbq161r, cbq176r, cbq189) , na.rm=TRUE )
hip <- rowMeans(data.frame(cbq8, cbq22, cbq30r, cbq51r, cbq60r, cbq67, cbq77, cbq100, cbq107, cbq124, cbq139, cbq159r, cbq182) , na.rm=TRUE )
imp <- rowMeans(data.frame(cbq13, cbq26, cbq46, cbq59, cbq71r, cbq79r, cbq90r, cbq104, cbq114, cbq137r, cbq155, cbq169r, cbq183r) , na.rm=TRUE )
inh <- rowMeans(data.frame(cbq4, cbq20, cbq32r, cbq63, cbq75, cbq93r, cbq108r, cbq116, cbq136, cbq147, cbq162r, cbq168, cbq185) , na.rm=TRUE )
lip <- rowMeans(data.frame(cbq12r, cbq36, cbq54, cbq66r, cbq76, cbq86r, cbq111r, cbq113, cbq133, cbq146, cbq151, cbq164, cbq174) , na.rm=TRUE )
per <- rowMeans(data.frame(cbq9, cbq28r, cbq31, cbq52, cbq65, cbq84r, cbq98, cbq105, cbq122r, cbq142r, cbq154, cbq170r) , na.rm=TRUE )
sad <- rowMeans(data.frame(cbq18, cbq39, cbq44, cbq55, cbq64, cbq72r, cbq81, cbq94, cbq109r, cbq112r, cbq127, cbq149r) , na.rm=TRUE )
shy <- rowMeans(data.frame(cbq7, cbq17r, cbq23r, cbq37, cbq45r, cbq57r, cbq74, cbq89, cbq106, cbq119r, cbq129r, cbq143, cbq158r) , na.rm=TRUE )
smi <- rowMeans(data.frame(cbq11, cbq43r, cbq56, cbq83r, cbq99r, cbq110, cbq121r, cbq135r, cbq152, cbq163, cbq165r, cbq179, cbq194) , na.rm=TRUE )
	})
}


##CALCULA 3 FACTORES, CE, AN, Y SU  
insert3factors_from_scales_cbqfn<- function(processee_cbqfn_dataset = cbqfn_raw_questionnaire_answers_dataset)
{
within(processee_cbqfn_dataset, {
CE <- rowMeans(data.frame(attcon, lip, inh, per) , na.rm=TRUE )
AN <- rowMeans(data.frame(sad, dis, fru, fea, sth) , na.rm=TRUE )
SU <- rowMeans(data.frame(shy, app, imp, hip, smi, act) , na.rm=TRUE )
	})
}



#==========================================
    #Function to clean-up outlaiers
#==========================================
place_na_in_otlaiers <- function(column_outlaieree) {
        for(iteration_column in 1:10) {
		print(paste0(iteration_column, '===ITER COLUM======'))
            outlaiers <- boxplot.stats(column_outlaieree)$out
            column_outlaieree[which(column_outlaieree %in% outlaiers)] <- NA
            if(! length(boxplot.stats(column_outlaieree)$out) > 0) break else print(paste0('==A COLUMN ITER=== ', iteration_column, '\\n ', as.character(which(column_outlaieree %in% outlaiers))))
        }
	return(column_outlaieree)
}
#==========================================


#==========================================
#FUNCTION TO CONSTRUCT A VECTOR OF BOOLENA VALUES INDICATION IF EACH COLUM HAS OUTLAIERS
#Creates a boolean value checking if there is still outlaiers after being removed, so tgat it can be evaluated
#==========================================
check_is_cleaned <- function(column_outlaieree) {
    return(! length(boxplot.stats(column_outlaieree)$out) > 0)
}

#==========================================
#Loop to identify outlaiers and place NA 
#==========================================
identify_and_make_na_outlaiers <- function(outlaieree_dataset) {
	for(iteration_dataset in 1:10) {
		print(paste0(iteration_dataset, '===ITER WHOLE DATASET======'))
	    na_ed_dataset <- data.frame(lapply(outlaieree_dataset, place_na_in_otlaiers))
	    checked_out_cleaned_vector <- unlist(lapply(na_ed_dataset, check_is_cleaned))
	    if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_dataset))
	}
	if(iteration_dataset > 9) print(paste0(iteration_dataset, '== MAXIMUM I REACHED =========='))
	write.csv(na_ed_dataset, 'xOUTLAIERS_CLEANED_ITEMS.csv')
	return(na_ed_dataset)
}

#==========================================
#SIN INVERTIDOS
#OUTLAIERS NA AND THEN IMPUTES
#==========================================
sin_invertidos_outlaiers_before_impute  <- function(maximum_iterations, number_of_imputations) {
#LOAD()
create_datasets(valkar)
#LOOP OUTLS IMPUTE
#for(iteration_imputation in 1:10) {
	cbqfn_meanee_only_item_answers_columns_dataset <<- identify_and_make_na_outlaiers(cbqfn_meanee_only_item_answers_columns_dataset)
	#IMPUTE()
	#check there are not outs left and  stop the loop
	checked_out_cleaned_vector <- unlist(lapply(cbqfn_meanee_only_item_answers_columns_dataset, check_is_cleaned))
    #if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_imputation))
	#}
#Remaining of the original impute fvgunction
mice_imputation_cbqfn_meanee_only_item_answers_columns_dataset(maximum_iterations, number_of_imputations)
generate_unreversed_cbqfn_meanee_only_item_answers_columns_dataset()
insert15_scales_from_items_cbqfn()
}


#==========================================
#IN MAKING THE ORIGINAL CBQ CALCULATION IT MAKES OUTLAIERS NA AND THEN IMPUTES, STOPINGG THE LOOP WHRE THERE ARE NO OUTL ANY MORE
#==========================================
outlaiers_before_impute <- function(questionnaire_filename_or_address, maximum_iterations, number_of_imputations) {
#LOAD()
create_datasets(questionnaire_filename_or_address)
#LOOP OUTLS IMPUTE
#for(iteration_imputation in 1:10) {
	cbqfn_meanee_only_item_answers_columns_dataset <<- identify_and_make_na_outlaiers(cbqfn_meanee_only_item_answers_columns_dataset)
	#IMPUTE()
	#check there are not outs left and  stop the loop
	#checked_out_cleaned_vector <- unlist(lapply(cbqfn_meanee_only_item_answers_columns_dataset, check_is_cleaned))
    #if(all(checked_out_cleaned_vector)) break else print(paste0('===== CLEANING ========', iteration_imputation))
	#}
#Remaining of the original impute fvgunction
mice_imputation_cbqfn_meanee_only_item_answers_columns_dataset(maximum_iterations, number_of_imputations)
insert15_scales_from_items_cbqfn()
}



#==========================================
#==========================================
#==========================================
#==========================================
cbq <- function(questionnaire_filename_or_address)
{
	create_datasets(questionnaire_filename_or_address) |>
		make_reversed_trail_r() |>
		insert15_scales_from_items_cbqfn() |>
		insert3factors_from_scales_cbqfn() |>
		insert_profiles_column_from_factors_cbqfn()
}

imputed_cbq  <- function(questionnaire_filename_or_address, maximum_iterations, number_of_imputations) {
create_datasets(questionnaire_filename_or_address)
mice_imputation_cbqfn_meanee_only_item_answers_columns_dataset(maximum_iterations, number_of_imputations)
insert15_scales_from_items_cbqfn()
}

#==========================================
#==========================================
sin_invertidos_val_kar_mfs <- function(questionnaire_filename_or_address)
{
	create_datasets(valkar) |>
		generate_unreversed_cbqfn_meanee_only_item_answers_columns_dataset() |>
		make_reversed_trail_r() |>
		insert15_scales_from_items_cbqfn() |>
		insert3factors_from_scales_cbqfn() |>
		insert_profiles_column_from_factors_cbqfn()
}


imputed_sin_invertidos  <- function(maximum_iterations, number_of_imputations) {
create_datasets(valkar)
mice_imputation_cbqfn_meanee_only_item_answers_columns_dataset(maximum_iterations, number_of_imputations)
generate_unreversed_cbqfn_meanee_only_item_answers_columns_dataset()
insert15_scales_from_items_cbqfn()
}


#==========================================
#==========================================
#==========================================
valkar <- 'https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/cbqLab_serrano2022.csv'

raven_url <- 'https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/ferserrano2022_raven.csv'

mfs <- 'https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/ferserrano2022_cbq.csv'
('https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/cbqLab_serrano2022.csv')
#sin_invertidos_val_kar_mfs(valkar)
#sin_invertidos_outlaiers_before_impute(50, 5) 
