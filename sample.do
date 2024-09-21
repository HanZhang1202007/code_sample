/******************************************************************************	
	
--------------	
	Input: cleaned data on registered projects   
	Output: summary stats
--------------
	
******************************************************************************/


// describe data

// Load the dataset
use "df_cleaned.dta", clear

capture program drop stats_summary
program define stats_summary
	// Initialize LaTeX document
	texdoc init stats_summary.tex, replace force
	texdoc write \begin{table}[h]
	texdoc write \centering
	texdoc write \caption{Summary statistics for key variables}
	texdoc write \begin{tabular}{lcc} \hline \hline
	texdoc write  & Value \\ \hline \\

	// Most frequent country
	egen most_freq_country = mode(reg_country)
	local most_freq_country = most_freq_country[1]
	texdoc write most frequent country: & `most_freq_country' \\


	// Mean and median of reg_smaple
	quietly summarize reg_sample
	local mean_reg_smaple = round(r(mean))
	//local median_reg_smaple = r(p50)
	texdoc write Mean registry sample size: & `mean_reg_smaple' \\
	//texdoc write Median registry sample size: & `median_reg_smaple' \\

	texdoc write  &  \\ \hline \hline
	texdoc write \end{tabular}
	texdoc write \end{table}
	texdoc close

	// end


	// Run the program
	tabulate_top5_to_latex

end


capture program drop stats_summary
program define stats_summary

    // Load the dataset
    use "df_cleaned.dta", clear

    // Initialize LaTeX document
    texdoc init stats_summary2.tex, replace force

    // Write the beginning of the table
    texdoc write \begin{table}[h]
    texdoc write \caption{Summary statistics for key variables, separated by found status}
    texdoc write \begin{tabular}{lcc} \hline \hline
    texdoc write  & Found & Not Found \\ \hline \\

    // Most frequent country for found == 1
    preserve
    keep if found == 1
    egen most_freq_country_found = mode(reg_country)
    local most_freq_country_found = most_freq_country_found[1]
    restore

    // Most frequent country for found == 0
    preserve
    keep if found == 0
    egen most_freq_country_not_found = mode(reg_country)
    local most_freq_country_not_found = most_freq_country_not_found[1]
    restore

    // Mean and median of reg_sample for found == 1
    preserve
    keep if found == 1
    quietly summarize reg_sample
    local mean_reg_sample_found = round(r(mean))
    local median_reg_sample_found = r(p50)
    restore

    // Mean and median of reg_sample for found == 0
    preserve
    keep if found == 0
    quietly summarize reg_sample
    local mean_reg_sample_not_found = round(r(mean))
    local median_reg_sample_not_found = r(p50)
    restore

    texdoc write Mean Sample size: & `mean_reg_sample_found' & `mean_reg_sample_not_found' \\
    // texdoc write Median registry sample size: & `median_pub_sample_found' & `median_reg_sample_not_found' \\

    texdoc write  &  \\ \hline \hline
    texdoc write \end{tabular}
    texdoc write \end{table}
    texdoc close

end













