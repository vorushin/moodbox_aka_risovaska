function toggleSearch()
{
	$('#ssel1').toggle();
	var a=$('#simple_link a');
	var b=$('input[name=search-type]');
	if (a.html()=="Больше") {a.html("Меньше");b.val('advanced');$('#ssel1 select').removeAttr("disabled");}else{a.html("Больше");b.val('simple');$('#ssel1 select').attr("disabled","disabled");}
	return false;
}

var jsReady = false;
function isReady() {
    return jsReady;
}

function pageInit() {
    jsReady = true;
}

function thisMovie(movieName) {
    if (navigator.appName.indexOf("Microsoft") != -1) {
        return window[movieName];
    } else {
        return document[movieName];
    }
}

function sendToActionScript() {
	$(".framepic").removeClass("act");
	$(this).addClass("act");
    thisMovie("moodwidget").sendToActionScript(($(this).attr("meta")-1));
}

function sendToJavaScript(value) {
	$(".framepic").removeClass("act");
	$(".framepic[meta="+(value+1)+"]").addClass("act");
}
     
jQuery(function( $ ){
	$(".framepic").click(sendToActionScript);
	$(window).load(pageInit());

});
