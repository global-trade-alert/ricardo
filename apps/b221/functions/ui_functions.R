generate_initial_hints <- function(df, initSingle = F) {
  
  added <- ifelse(initSingle, " ", " added")
  official <- ifelse(is.na(df$official), ""," official")
  df$checkbox <- '<img class="official" src="www/b221/official.svg"><img class="nonofficial" src="www/b221/nonofficial.svg">'
  
  df$processed <- ifelse(df$hint.state.id>2, '<img class="processed" src="www/b221/processed.svg">', '<img class="unprocessed" src="www/b221/unprocessed.svg">')
  output <- paste0('<div data-tooltip-content="#top-tooltip_',df$hint.id,'" id="hintId_',df$hint.id,'" class="hint-item tooltip-create-top',added,official,'"><div class="top"><div class="hint-title">',df$hint.title,'</div><div class="remove" value="',df$hint.id,'"><img src="www/b221/cancel.svg"></div><span class="material-icons">stars</span></div><div class="bottom-options"><div class="is-official">',df$checkbox,'</div><a class="url" href="',df$url,'" target="_blank"><img src="www/b221/urlwhite.svg"></a><div class="is-processed">',df$processed,'</div></div></div>',df$tpcontent)
  return(output)
}