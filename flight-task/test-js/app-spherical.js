'use strict';

require('./spherical.html');
require('./email.js');
var $ = require('jquery');
var Dms = require('./dms.js');
var LatLon = require('./latlon-spherical.js');

var outputLine = "";

function FlushOutput() {
document.form.database.value = outputLine;
outputLine = "";
}

var compute = function () {
var text = document.form.rawdatabase.value;

if (text.length == 0) {
  alert ("Nothing to process");
  return;
}
var lines = text.split('\n');
for (var i=0; i<lines.length; i++) {
  var line = lines[i];
  // strip spaces from either end of line
  while (line.charAt(0) == " ") {
    line = line.substr(1);
  }
  var length = line.length;
  while (line.charAt(--length) == " ") {
    line = line.substr(0,length);
  }

  // clean up line a little more
  if (line.length == 0 && i == lines.length-1) {
    break;
  }
  if (line.charAt(line.length-1) == '\r') {
    line = line.substr(0,line.length-1);
  }

  // parse the line
  let latlonArray = line.split(",");
  let lat1 = latlonArray[0];
  let lon1 = latlonArray[1];
  let lat2 = latlonArray[2];
  let lon2 = latlonArray[3];

  if (lat1 > 90) {
    lat1 = 90;
  } else if (lat1 < -90) {
    lat1 = -90;
  }
  if (lon1 > 180) {
    lon1 = 180;
  } else if (lon1 < -180) {
    lon1 = -180;
  }
  if (lat2 > 90) {
    lat2 = 90;
  } else if (lat2 < -90) {
    lat2 = -90;
  }
  if (lon2 > 180) {
    lon2 = 180;
  } else if (lon2 < -180) {
    lon2 = -180;
  }

  // get the distance
  const p1 = new LatLon(Dms.parseDMS(lat1), Dms.parseDMS(lon1));
  const p2 = new LatLon(Dms.parseDMS(lat2), Dms.parseDMS(lon2));
  const meters = parseFloat(p1.distanceTo(p2).toPrecision(4));

  var kilometers = meters / 1000; // converts meters to kilometers
  var statute = meters * 0.000621371192; // converts meters to statute miles
  var nautical = 0.86897624 * statute; // converts miles to nautical miles

  for (var j=0; j<document.form.units.length; j++) {
    if (document.form.units[j].value == "kilometers" && document.form.units[j].checked) {
      outputLine = outputLine + kilometers + "\n";
      break;
    } else if (document.form.units[j].value == "statute" && document.form.units[j].checked) {
      outputLine = outputLine + statute + "\n";
      break;
    } else if (document.form.units[j].value == "nautical" && document.form.units[j].checked) {
      outputLine = outputLine + nautical + "\n";
      break;
    }
  }
}
FlushOutput();
}

function clear() {
document.form.database.value =
  "Do not enter anything here\n" +
  "Use the left-side box";
document.form.rawdatabase.value = "";
}

$().ready(function () {
    clear();
    $("#process-btn").click(compute);
});

