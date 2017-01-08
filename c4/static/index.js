'use strict';

window.onload = function() {
    var apiUrl = 'https://api.koeln.ccc.de/';
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        var res = JSON.parse(this.responseText);
        var last = res.state.lastchange;
        var now = Date.now() / 1000;
        var delta = now - last;

        var description = 'Seit etwa ';
        if (delta >= 2*365*24*60*60) {
            description += Math.round(delta/(365*24*60*60)) + ' Jahren';
        } else if (delta >= 365*24*60*60) {
            description += 'einem Jahr';
        } else if (delta >= 2*30*24*60*60) {
            description += Math.round(delta/(30*24*60*60)) + ' Monaten';
        } else if (delta >= 30*24*60*60) {
            description += 'einem Monat';
        } else if (delta >= 2*24*60*60) {
            description += Math.round(delta/(24*60*60)) + ' Tagen';
        } else if (delta >= 24*60*60) {
            description += 'einem Tag';
        } else if (delta >= 2*60*60) {
            description += Math.round(delta/(60*60)) + ' Stunden';
        } else if (delta >= 60*60) {
            description += 'einer Stunde';
        } else if (delta >= 2*60) {
            description += Math.round(delta/60) + ' Minuten';
        } else if (delta >= 60) {
            description += 'einer Minute';
        } else {
            description += Math.round(delta) + ' Sekunden';
        }

        var detail = document.createElement('h2');
        detail.appendChild(document.createTextNode(description));
        var body = document.getElementsByTagName('body')[0];
        body.appendChild(detail);
    }
    xhr.open('GET', apiUrl);
    xhr.send();
}
