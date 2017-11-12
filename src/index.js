var jQueryScript = document.createElement('script');
jQueryScript.setAttribute('src', 'https://code.jquery.com/jquery-3.2.1.min.js');
document.head.appendChild(jQueryScript);

var materialize = document.createElement('materialize');
materialize.setAttribute('src', 'https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js');
document.head.appendChild(materialize);

import './main.css';
import {
    Main
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var myapp = Main.embed(document.getElementById('root'));

let wods = localStorage.getItem('wods')

if (wods != null)
    myapp.ports.getWods.send(JSON.parse(wods));

myapp.ports.saveWods.subscribe(function(wods) {
    localStorage.setItem('wods',  JSON.stringify(wods))
    Materialize.toast('Results saved', 4000) // 4000 is the duration of the toast
});

registerServiceWorker();
