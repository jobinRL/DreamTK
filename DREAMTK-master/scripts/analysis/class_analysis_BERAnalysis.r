# Basic statistical data  --------------------------------------------


#v0.7

#class contains basic statistical data and data calculating functions
# - minac50, minoed, target family counts, min and avg ac50 per target family, scalar top

#the design goal with this is that this is part of a composition where the basicstats data is all managed by another class and all the plotting is done by this class.
#The reason I am doing this is that it allows for easy expension. Since all you need is to include the data. 
Class.Analysis.BERAnalysis <- R6Class("Class.Analysis.BERAnalysis",
  #private variables and functions
  private = list(
  
	#BER plot variables
	pltBER = NULL,
	tblBER = NULL,
	tblMeanBER = NULL,#
	pltBERvsAc50 = NULL
),

#public variables and functions
  public = list(
	BERData =  NULL,
	basicData = NULL,
    #constructor
    initialize = function(  ) {
		self$BERData = Class.Analysis.BERData$new(); #Done here because it will be shared across all instances of the object otherwise.
		self$basicData <- Class.Analysis.Data$new(  ); #Done here because it will be shared across all instances of the object otherwise.
	},
    #finalizer
    finalize = function() {
    },
	
	
	
	#BER Analysis Region
	
	#computes the Agregated table for BER analysis
    computerMeanTableBER = function(){
      if (self$BERData$calcBERStatsDataExists() && self$basicData$basicStatsDataExists()) {
		private$tblMeanBER = tribble(~casn,~name,~average_of_oral_consumer_products_exposure, ~min_ac50_over_oral_consumer_products_exposure, ~mean_ac50_over_oral_consumer_products_exposure);
		#getting our private data
        calc_BER_stat_tbl <- self$BERData$getCalcBERStatsTable();
		chemical_casn_list <- unique(calc_BER_stat_tbl[["casn"]]);
		Ac50_stat_tbl <- self$basicData$getBasicStatsTable();
		
		for(chemical_casn in chemical_casn_list){
			casn_tbl <- filter(calc_BER_stat_tbl, casn == chemical_casn);
			ac50_tbl <- filter(Ac50_stat_tbl, casn == chemical_casn);
			avg_mean = mean(casn_tbl$direct_ingestion + casn_tbl$direct_vapor + casn_tbl$direct_aerosol);
			oral_ber = (casn_tbl$direct_ingestion + casn_tbl$direct_vapor + casn_tbl$direct_aerosol);
			avg_ac50 = mean(10^ac50_tbl$ac50);
			min_ac50 = min(10^ac50_tbl$ac50);
			
			
			theorical_95th_percentile = quantile(oral_ber,0.95);
			closest_to_95th = absolute.min(oral_ber - theorical_95th_percentile) + theorical_95th_percentile ; #gets the closest value to the 95th percentile.
			
			
			private$tblMeanBER <- add_row(private$tblMeanBER, casn = chemical_casn, 
						name = as.character(filter(calc_BER_stat_tbl, casn == chemical_casn) %>% select(name) %>% distinct(name)),
						average_of_oral_consumer_products_exposure = signif(avg_mean, digits = 5),
						min_ac50_over_oral_consumer_products_exposure = signif(ifelse(is.nan(min_ac50) || is.infinite(min_ac50),0,min_ac50 / closest_to_95th), digits = 5),
						mean_ac50_over_oral_consumer_products_exposure = signif(ifelse(is.nan(avg_ac50) || is.infinite(avg_ac50),0,avg_ac50 / closest_to_95th), digits = 5)  
						);
				
			
		}

		invisible(private$tblMeanBER);
     }   
    },
	#plots the box chart comparing the values of the oral exposure vs the AC50 of chemicals
	plotBERvsAc50 = function( label_by = "casn"){
      if (self$BERData$calcBERStatsDataExists() && self$basicData$basicStatsDataExists()) {
		BER_data = self$BERData$getCalcBERStatsTable();
		BER_data$oral_ber =BER_data$direct_ingestion + BER_data$direct_vapor + BER_data$direct_aerosol;
		basic_data = self$basicData$getBasicStatsTable();
		if(label_by == "casn"){
			private$pltBERvsAc50 <- plot_ly() %>%
			  add_trace(data = BER_data, x = ~casn, y = ~signif(log10(oral_ber), digits = 5), type = 'box', name = 'BE',) %>%
			  add_trace(data = basic_data, x = ~casn, y = ~signif(ac50, digits = 5), type = 'box', name = 'Ac50') %>%
			  layout(title = 'Log Consumer Product Exposure vs  Log Ac50',
					 xaxis = list(title = ""),
					 yaxis = list(title = ""));
	
		}else{
			private$pltBERvsAc50 <- plot_ly() %>%
			  add_trace(data = BER_data, x = ~name, y = ~signif(log10(oral_ber), digits = 5), type = 'box', name = 'BE') %>%
			  add_trace(data = basic_data, x = ~name, y =~signif(ac50, digits = 5), type = 'box', name = 'Ac50') %>%
			  layout(title = 'Log Consumer Product Exposure vs Log Ac50',
					 xaxis = list(title = ""),
					 yaxis = list(title = "Log 10"));
		
		}
		
		invisible(private$pltBERvsAc50);
     }   
    },
	
	#plots the BER box chart
    plotBER = function( label_by = "casn" ){
      if (self$BERData$calcBERStatsDataExists() && self$basicData$basicStatsDataExists()) {
		tbl = tribble(~casn,~name,~product_use_category, ~average_of_oral_BER);
		#getting our private data
        calc_BER_stat_tbl <- self$BERData$getCalcBERStatsTable();
		chemical_casn_list <- unique(calc_BER_stat_tbl[["casn"]]);
		Ac50_stat_tbl <- self$basicData$getBasicStatsTable();
		
		for(chemical_casn in chemical_casn_list){
			
			ac50_tbl <- filter(Ac50_stat_tbl, casn == chemical_casn);
			
			avg_ac50 = mean(10^ac50_tbl$ac50);
			casn_tbl <- filter(calc_BER_stat_tbl, casn == chemical_casn);
			if(!(is.nan(avg_ac50) || is.infinite(avg_ac50))){
				for ( puc in casn_tbl$product_use_category ){
					puc_tbl <- filter(casn_tbl, product_use_category == puc);
					oral_ber = (casn_tbl$direct_ingestion + casn_tbl$direct_vapor + casn_tbl$direct_aerosol);
					tbl <- add_row(tbl, casn = chemical_casn, 
								name = as.character(filter(calc_BER_stat_tbl, casn == chemical_casn) %>% select(name) %>% distinct(name)),
								product_use_category = puc,
								average_of_oral_BER = signif(avg_ac50/oral_ber, digits = 5),
								);
					
				}
			}
		}
		if(label_by == "casn"){
			private$pltBER <- plot_ly() %>%
			  add_trace(data = tbl, x = ~casn, y = ~signif(log10(average_of_oral_BER), digits = 5), type = 'box') %>%
			  layout(title = 'Log BER',
					 xaxis = list(title = ""),
					 yaxis = list(title = ""));
	
		}else{
			private$pltBER <- plot_ly() %>%
			  add_trace(data = tbl, x = ~name, y = ~signif(log10(average_of_oral_BER), digits = 5), type = 'box') %>%
			  layout(title = 'Log BER',
					 xaxis = list(title = ""),
					 yaxis = list(title = ""));
		
		}
		
		invisible(private$pltBER);
     }   
    },

	getMeanBERTable = function (){
		return (private$tblMeanBER);
    },
	getBERvsAc50plot = function(){
		return (private$pltBERvsAc50);
	},
	getBERplot = function(){
		return (private$pltBER);
	}
 
	)
)