import gs_client from './gs_client.js';
import './jquery.min.js';

const wsHost = "ws://localhost:8080/websocket";

$(document).ready(init);

function init() {
    if(!("WebSocket" in window)) {  
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
        
        // Set up the websocket client
        gs_client.connect(wsHost).then(() => {
            showScreen('<b>Connecting to: ' +  wsHost + '</b>');
            gs_client.on('open', evt => onOpen(evt));
            gs_client.on('close', evt => onClose(evt));
            gs_client.on('message', evt => onMessage(evt));
            gs_client.on('error', evt => onError(evt));
        });
    };

    // Register click event handlers
    $("#toggle-connection").click(() => gs_client.toggle_connection(wsHost)
        .then(connectionState => {
            if (connectionState === 'connected') {
                gs_client.on('open', evt => onOpen(evt));
                gs_client.on('close', evt => onClose(evt));
                gs_client.on('message', evt => onMessage(evt));
                gs_client.on('error', evt => onError(evt));
            }
        })
    );

    $("#send-txt").click(
        () => gs_client
            .sendTxt($("#nick-name").val(), $("#txt").val())
            .then((msg) => showScreen('sending: ' + msg))
            .catch((error) => showScreen(error)));

    $("#clear-screen").click(() => clearScreen());

    $("#join-chat-group").click(
        () => gs_client
            .joinChatGroup($("#nick-name").val(), $("#chat-group").val())
            // .then(resp => console.log(resp))
            .catch((error) => showScreen(error)));
    
    $("#leave-chat-group").click(
        () => gs_client
            .leaveChatGroup($("#nick-name").val(), $("#chat-group").val())
            // .then(resp => console.log(resp))
            .catch((error) => showScreen(error)));

    // Hide ui elements that aren't ready yet
    $("#connected").hide();

    $("#content").hide();
};

function onOpen(evt) { 
    showScreen('<span style="color: green;">CONNECTED </span>'); 
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
}

function onClose(evt) {
    console.log(evt);
    showScreen('<span style="color: red;">DISCONNECTED </span>');
}

function onJoinGroup(evt) {
    gs_client.joinGroup(evt);
}

function onMessage({data}) {
    let resp = JSON.parse(data);
    if (resp.users) {
        updateChatGroup(resp.users);
    }
    showScreen('<span style="color: blue;">RESPONSE: ' + data + '</span>');
}

function onError(evt) {
    showScreen('<span style="color: red;">ERROR: ' + JSON.parse(evt.data).msg+ '</span>');
}

function showScreen(txt) {
    $('#output').prepend('<p>' + txt + '</p>');
} 

function clearScreen() {
    $('#output').html("");
}

function updateChatGroup(users) {
    let chatGroup = $("#users");
    chatGroup.empty();
    for (let user of users) {
        chatGroup
            .append(`<li id="user">${username}</li>`);
    }
}
