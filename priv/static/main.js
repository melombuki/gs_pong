import gs_client from './gs_client.js';
import './jquery.min.js';

const wsHost = "wss://www.gsserver.com:8081/websocket";

$(document).ready(init);

function init() {
    if(!("WebSocket" in window)) {  
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
    };

    // Register click event handlers
    $("#toggle-connection").click(() => gs_client.toggle_connection(wsHost)
        .then(connectionState => {
            if (connectionState === 'connected') {
                gs_client.on('open', evt => onOpen(evt));
                gs_client.on('close', evt => onClose(evt));
                gs_client.on('message', evt => onMessage(evt));
                gs_client.on('error', evt => onError(evt));
            } else if(connectionState == 'closed') {
                hideConnectedOnlyElements();
            }
        })
    );

    $("#change-nickname").click(
        () => gs_client
            .changeNickname($("#nick-name").val())
            // .then((ret) => console.log(ret))
            .catch((error) => showScreen(error)));

    $("#send-txt").click(
        () => gs_client
            .sendTxt($("#nick-name").val(), $("#chat-room").val(), $("#txt").val())
            .then((msg) => showScreen('sending: ' + msg))
            .catch((error) => showScreen(error)));

    $("#clear-screen").click(() => clearScreen());

    $("#join-chat-room").click(
        () => gs_client
            .joinChatRoom($("#nick-name").val(), $("#chat-room").val())
            // .then(resp => console.log(resp))
            .catch((error) => showScreen(error)));
    
    $("#leave-chat-room").click(
        () => gs_client
            .leaveChatRoom($("#nick-name").val(), $("#chat-room").val())
            // .then(resp => console.log(resp))
            .catch((error) => showScreen(error)));

    // Hide ui elements that aren't ready yet
    hideConnectedOnlyElements();
};

function onOpen(evt) {
    showScreen('<span style="color: green;">CONNECTED </span>'); 
    fadeInConnectedOnlyElements();
}

function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
    hideConnectedOnlyElements();
}

function onJoinGroup(evt) {
    gs_client.joinGroup(evt);
}

function onMessage({data}) {
    let resp = JSON.parse(data);
    if (resp.users) {
        updateChatGroup(resp.users);
    }
    showScreen(`<span style="color: blue;">RESPONSE: ${data}</span>`);
}

function onError(evt) {
    let errorMessage = evt.data ? JSON.parse(evt.data).msg : 'You\'re doing it wrong.';
    showScreen(`<span style="color: red;">ERROR: ${errorMessage}</span>`);
}

function showScreen(txt) {
    $('#output').prepend(`<p>${txt}</p>`);
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

function hideConnectedOnlyElements() {
    $("#user").hide();
    $("#chat").hide();
    $("#connected").hide();
    $("#content").hide();
}

function fadeInConnectedOnlyElements() {
    $("#user").fadeIn('slow');
    $("#chat").fadeIn('slow');
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
}
