var jQueryScript = document.createElement('script');
jQueryScript.setAttribute('src', 'https://code.jquery.com/jquery-3.2.1.min.js');
document.head.appendChild(jQueryScript);

var materialize = document.createElement('materialize');
materialize.setAttribute(
    'src',
    'https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js');
document.head.appendChild(materialize);

import './main.css';
import {
    Main
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var myapp = Main.embed(document.getElementById('root'));
var provider = new firebase.auth.FacebookAuthProvider();
var database = firebase.database();
provider.addScope('user_birthday');
firebase.auth().useDeviceLanguage();
registerServiceWorker();

firebase.auth().onAuthStateChanged((user) => {
    if (user) {
        myapp.ports.signedIn.send({
            displayName: user.displayName,
            profilePic: user.photoURL,
            identifier: user.email,
            userUid: user.uid
        });

        firebase.database().ref('users/' + user.uid + '/results/').once('value')
            .then(function (snapshot) {
                var wods = snapshot.val();

                if (wods)
                    myapp.ports.getWods.send(wods);
            });
    }
});

firebase.auth()
    .getRedirectResult()
    .then(function (result) {
        if (result.credential) {
            var token = result.credential.accessToken;
        }

        var user = result.user;
    })
    .catch(function (error) {
        var errorCode = error.code;
        var errorMessage = error.message;
        var email = error.email;
        var credential = error.credential;
    });

myapp.ports.saveWods.subscribe(function ([uuid, wods]) {
    firebase.database().ref('users/' + uuid + '/results/').set(wods);
    Materialize.toast('Results saved', 4000);
});

myapp.ports.signIn.subscribe(function () {
    firebase.auth().signInWithRedirect(provider);
});

myapp.ports.updateInputFields.subscribe(function () {
    setTimeout(function () {
        Materialize.updateTextFields();
    });
});