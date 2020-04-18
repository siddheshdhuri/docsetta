

// javascript for addTrendToSearch icon button
function addTrendToSearch(trendName) {
  var searchval = document.getElementById('search').value;
  
  document.getElementById('search').value = searchval.concat(',',trendName) ;
  Shiny.onInputChange('search',document.getElementById('search').value);
  document.getElementById(trendName).style.visibility = 'hidden';
  
}


// javascript for trendQuickView icon button
function trendQuickView(trendUrl) {
  
  document.getElementById('quickViewTrendPanel').style.visibility = 'visible';
  
  document.getElementById('hiddenTrendURL').value = trendUrl ;
  Shiny.onInputChange('hiddenTrendURL',trendUrl);
  //document.getElementById(trendName).style.visibility = 'hidden';
  
}


function closeTrendView() {
  
  document.getElementById('hiddenTrendURL').value = null ;
  document.getElementById('quickViewTrendPanel').style.visibility = 'hidden';
  Shiny.onInputChange('hiddenTrendURL',null);
}


jQuery(document).ready(function () {
$("#trend-quick-view").scrollTop(10).scrollLeft(750);
});