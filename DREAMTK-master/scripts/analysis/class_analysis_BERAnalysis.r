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
	tblBER = NULL#

	
),

#public variables and functions
  public = list(
	BERData =  NULL,

    #constructor
    initialize = function(  ) {
		self$BERData = Class.Analysis.BERData$new(); #Done here because it will be shared across all instances of the object otherwise.
	},
    getBERData = function(){
		return (self$BERData);
	},
    #finalizer
    finalize = function() {
    },
	
	
	
	#Basic Analysis Region
	
	 #plots pie chart of target family counts
	 #preconditions: target_family_counts is calculated
    plotBER = function(){
      if ( self$BERData$calcBERStatsDataExists()) {
		
		#getting our private data
        calc_BER_stat_tbl <- self$BERData$getCalcBERStatsTable();
		
          if( length( unique(calc_BER_stat_tbl_tbl_local$casn) )>20 ){
          sl <- FALSE;
        } else {
          sl <- TRUE;
        }
        
        private$pltBER <- plot_ly(calc_BER_stat_tbl, x = ~scalar_top, y = ~oed, color = scalar_top_tbl_local[[label_by]], alpha = 0.5,
                                 colors = rainbow(10, s = 3/4, v=3/4),
                                 type = "scatter", mode = "markers",
                                 symbol = above_cutoff_labels, symbols = c("circle", "x"),
                                 hoverinfo = "text",
                                 text = ~paste( "</br> Name: ", name,
                                                "</br> Casn: ", casn,
                                                "</br> Assay endpoint: ", assay_component_endpoint_name,
                                                "</br> Above cutoff: ", above_cutoff,
                                                "</br> OED (mg/kg/day): ", signif(oed, digits=5),
                                                "</br> Scalar top: ", signif(scalar_top, digits=5))  ) %>% 
          
          layout(title = "OED vs Log scalar top",
                 xaxis = list(title = "Log scalar top",
                              titlefont = list(size = 14),
                              type = "log"),
                 yaxis = list(title = "OED (mg/kg/day)",
                              titlefont = list(size = 14)),
                 showlegend = sl,
                 legend = list(orientation = "h")   );
        
      } else {
        
        private$plttfc <- NULL;

      }
      
      invisible ( private$plttfc );
      
    },

	getTargetFamilyCountsPlot = function (){
      return (private$plttfc);
    }
 
 
	)
)