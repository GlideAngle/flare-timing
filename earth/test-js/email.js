//var siteaddr = "anit" + "amo" + "rse.org";
var siteaddr = "stev" + "emo" + "rse.org";
var emailaddr = "s" + "teve" + "@ste" + "ve" + "mors" + "e.org";
var emailurl = "mail" + "to:" + emailaddr;
function emailhref() {
  document.write('<a href="' + emailurl + '">');
}

function email(me) {
  document.write('<a href="' + emailurl + '">' + me + '</a>');
}

function WebCopier() {
  if (navigator.userAgent.indexOf ("WebCopier") != -1) {
    document.location.replace("http://www.maximumsoft.com/");
  }
}

function SiteSucker() {
  if (navigator.userAgent.indexOf ("SiteSucker") != -1) {
    document.location.replace("http://www.sitesucker.us/home.html");
  }
}

WebCopier();
SiteSucker();
