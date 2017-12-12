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
            profilePic: user.photoURL
        });
    }
});

firebase.database().ref('/users/' + '0').once('value')
    .then(function (snapshot) {
        var wods = snapshot.val();
        myapp.ports.getWods.send(wods);
    });

firebase.auth()
    .getRedirectResult()
    .then(function (result) {
        if (result.credential) {
            // This gives you a Google Access Token. You can use it to access the
            // Google API.
            var token = result.credential.accessToken;
            // ...
        }
        // The signed-in user info.
        var user = result.user;
    })
    .catch(function (error) {
        // Handle Errors here.
        var errorCode = error.code;
        var errorMessage = error.message;
        // The email of the user's account used.
        var email = error.email;
        // The firebase.auth.AuthCredential type that was used.
        var credential = error.credential;
        // ...
    });

myapp.ports.saveWods.subscribe(function (wods) {
    firebase.database().ref('users/' + '0').set(wods);
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
