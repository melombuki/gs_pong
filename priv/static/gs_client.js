import './jquery.min.js';

let websocket;

async function connect(wsHost) {
    websocket = new WebSocket(wsHost);
    return 'connected';
}

async function disconnect() {
    websocket.close();
    return 'closed';
}

async function toggle_connection(wsHost) {
    if (websocket && websocket.readyState == websocket.OPEN) {
        return disconnect();
    } else {
        return connect(wsHost);
    }
}

async function changeNickname(nickname) {
    return handleWsCallback(
        () => send({
            type: "select_username",
            nick: nickname,
        })
    );
}

async function sendTxt(nickname, text) {
    return handleWsCallback(
        () => send({
            type: "chat_msg",
            msg: text,
            nick: nickname,
        })
    );
}

async function joinChatRoom(nickname, room) {
    return handleWsCallback(
        () => send({
            type: "join_chat_room",
            room: room, 
            nick: nickname,
        })
    );
}

async function leaveChatRoom(nickname, room) {
    return handleWsCallback(
        () => send({
            type: "leave_chat_room",
            room: room, 
            nick: nickname,
        })
    );
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

export default Object.freeze({
    connect: connect,
    disconnect: disconnect,
    toggle_connection: toggle_connection,
    sendTxt: sendTxt,
    websocket: websocket,
    on: on,
    off: off,
    joinChatRoom: joinChatRoom,
    leaveChatRoom: leaveChatRoom,
    changeNickname: changeNickname,
});

async function handleWsCallback(callback) {
    if (websocket && websocket.readyState == websocket.OPEN) {
        return callback();
    } else {
        throw new Error('websocket is not connected');
    }
}

async function send(message) {
    let msg = JSON.stringify(message)
    websocket.send(msg);
    return msg;
}