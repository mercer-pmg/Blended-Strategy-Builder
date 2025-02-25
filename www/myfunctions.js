
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "#C00686"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}