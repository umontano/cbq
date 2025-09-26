#==========================================================================
# CONTAINS THE FUNCTION DEFINITIONS TO CHECK FOR CORRELATIONS
# IN THE MA FER SERRANO DATASET TORRANCE_CREATIVITY CBQ_TEMPERAMENT RAVEN_ABSTRACT_THOUGHT 
# ONCE THE SIGNIFICANT CORRELATIONS HAVE BEEN IDENTIFIED THEIR  P ARE PRINTED
# AND THEY ARE PLOTTED USING GGPLOT
#==========================================================================
#DEFINE THE FUNCTIONS to generate SCATTER PLOTS AND SUBROUTINES
#==========================================================================
scatterp_with_regression_lines <- function(plotee_pair, rows_dataset, columns_dataset)
{
variablex <- plotee_pair[2]
variabley <- plotee_pair[1]
merged_dataset <- cbind(rows_dataset, columns_dataset)
library(ggplot2)
ggplot(merged_dataset, aes(
	merged_dataset[, variablex],
	merged_dataset[, variabley]
	)
) +
geom_jitter(alpha=0.5) +
geom_smooth(method="lm", se=TRUE, fullrange=TRUE) +
         annotate( "point", x=mean(merged_dataset[, variablex]), y=mean(merged_dataset[, variabley]), col="red4", size=3) +
labs(title = paste(variablex, '~', variabley)) +
xlab(variablex) +
ylab(variabley)
}

#================================================================
#rows_dataset <- torrance
#================================================================
#================================================================
#================================================================
# Function to find significant correlation in pairs of items 
#================================================================

find_significant_correlations_from_rows_and_cols_datasets <- function(rows_dataset, columns_dataset, sign=0.05)
{
    #Matrices for correlations and pvalues
    cor_mat <- round(cor(rows_dataset, columns_dataset), 8)
    library('psych')
    #option adjust='bonferroni'
    cor_test_mat <- round(corr.test(rows_dataset, columns_dataset, adjust='bonferroni')$p, 3)
	#Extract rows and columns
        rows <- rownames(cor_mat)
        cols <- colnames(cor_mat)
	#Initialize empty list
	pairs_list <<- NULL
    #Implementation of search by currying nested mapping functions
    lapply(cols, function(eachcol) lapply(rows, function(eachrow){ #if(pmat[y,x]<0.05) llll[[length(llll)+1]] <<- c(y,x) 
        #for(eachcol in cols) {
            #for(eachrow in rows) {
			if(
                !is.na(cor_test_mat[eachrow , eachcol]) &&
                eachrow != eachcol &&
                cor_test_mat[eachrow , eachcol] < sign
                ) {
                    pairs_list[[ length(pairs_list) +1 ]] <<- c(eachrow, eachcol);
                    print(paste(eachrow, eachcol, '_ r =', cor_mat[eachrow, eachcol], '_ pval =', cor_test_mat[eachrow, eachcol]));
                    }
                #}
            #}
    #nested lapply ends here
    } ) )
return(pairs_list)
}

#scatterp_with_regression_lines <- function(plotee_pair) { return(plot(torrance[, variablex], torrance[, variabley]) ) }
scatterplot_significant_correlations <- function(rows_dataset, columns_dataset, sign=0.05)
{
cor_mat <- cor(rows_dataset, columns_dataset)
library(psych)
cor_test_mat <- corr.test(rows_dataset, columns_dataset)$p    # Apply corr.test function
pairs_list <- find_significant_correlations_from_rows_and_cols_datasets(rows_dataset, columns_dataset, sign)
		#Show results only in case there exists significant values
		if(length(pairs_list) > 0)
		{
			#Show p-values text
			showr <- cor_mat
			showpv <- cor_test_mat
			showr[showpv > sign] <- NA
			#print(showr)
			showpv[showpv > sign] <- NA

			#Send list of names to generate scatterplots
			scatters_list <- lapply(pairs_list, scatterp_with_regression_lines, rows_dataset, columns_dataset)
			#Show a single graph from the list of scatters
			library('gridExtra')
			print(gridExtra::marrangeGrob(grobs = scatters_list, ncol = 3, nrow = 2))

			#Graphic correlation matrix
			library(ggcorrplot)
			corr_graph <- ggcorrplot(t(cor_mat), ggtheme = ggplot2::theme_dark, lab=TRUE, p.mat=t(cor_test_mat), insig='blank')
			print(corr_graph)

			return(list(corr_graph, scatters_list))
		}
		else
		{
			#Message no significants
			lapply(1:4, function(x) print('=== NO SIGNIFICANT CORRELATIONS FOUND ==='));
            print('==== THESE ARE THE P-VALUES: ====')
			print(cor_test_mat)
			return(NULL)
		}
}


## MAIN FUNCTION WICH WRAPPES THE ORIGINAL SCATEERPLOT TO BE FORWATD COMPATIBLE WITH TIDYVERSE AND NEWER SCRIPTS
correlation_scatterplot_mapper_df2names_wrap_up_main_fn <- function(correlationee_dataset, rows_names, columns_names, significance_threshold = 0.05)
{
	scatterplot_significant_correlations(rows_dataset = correlationee_dataset[rows_names], 
columns_dataset = correlationee_dataset[columns_names], sign = significance_threshold)
}
