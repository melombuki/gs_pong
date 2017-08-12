import './jquery.min.js';

let websocket;

async function connect(wsHost) {
    websocket = new WebSocket(wsHost);
    return 'connected';
}

function disconnect() {
    websocket.close();
    return 'closed';
}

async function toggle_connection(wsHost) {
    if (websocket.readyState == websocket.OPEN) {
        return disconnect();
    } else {
        return connect(wsHost);
    }
}

async function sendTxt(nickname, text) {
    if (websocket.readyState == websocket.OPEN) {
        let msg = JSON.stringify({type: "chat_msg", msg: text, nick: nickname});
        websocket.send(msg);
        return msg;
    } else {
        throw new Error('websocket is not connected'); 
    }
}

async function joinChatGroup(nickname, group) {
    if (websocket.readyState == websocket.OPEN) {
        let msg = JSON.stringify({
            type: "join_chat_group",
            group: group, 
            nick: nickname});
        websocket.send(msg);
        return msg;
    } else {
        throw new Error('websocket is not connected'); 
    }
}

async function leaveChatGroup(nickname, group) {
    if (websocket.readyState == websocket.OPEN) {
        let msg = JSON.stringify({
            type: "leave_chat_group",
            group: group, 
            nick: nickname});
        websocket.send(msg);
        return msg;
    } else {
        throw new Error('websocket is not connected'); 
    }
}

function on(name, callback) {
    if(name === 'open') {
        websocket.onopen = callback;
    } else if (name === 'close') {
        websocket.onclose = callback;
    } else if (name === 'message') {
        websocket.onmessage = callback;
    } else if (name === 'error') {
        websocket.onerror = callback;
    } else {
        throw new Error('Unsupported callback');
    }
}

function off(name, callback) {
    if(name === 'open') {
        websocket.onopen = null;
    } else if (name === 'close') {
        websocket.onclose = null;
    } else if (name === 'message') {
        websocket.onmessage = null;
    } else if (name === 'error') {
        websocket.onerror = null;
    } else {
        throw new Error('Unsupported callback');
    }
}

function sendJoin(group) {
    let joinMessage = {join: group};
    websocket.send(joinMessage);
}

export default Object.freeze({
    connect: connect,
    disconnect: disconnect,
    toggle_connection: toggle_connection,
    sendTxt: sendTxt,
    websocket: websocket,
    on: on,
    off: off,
    joinChatGroup: joinChatGroup,
    leaveChatGroup: leaveChatGroup,
});