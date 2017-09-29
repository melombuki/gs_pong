(function(window) {
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
            })
        );
    }

    async function sendTxt(room, text) {
        return handleWsCallback(
            () => send({
                type: "chat_msg",
                room: room,
                msg: text,
            })
        );
    }

    async function joinChatRoom(room) {
        return handleWsCallback(
            () => send({
                type: "join_game_room",
                room: room,
            })
        );
    }

    async function leaveChatRoom(room) {
        return handleWsCallback(
            () => send({
                type: "leave_game_room",
                room: room,
            })
        );
    }

    async function startGame(room) {
        return handleWsCallback(
            () => send({
                type: "start_game",
                room: room,
            })
        )
    }

    async function sendInput(key, room) {
        return handleWsCallback(
            () => send({
                type: "input",
                key: key,
                room: room,
            })
        )
    }

    function on(name, callback) {
        if (name === 'open') {
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
        if (name === 'open') {
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

    window.gs_client = Object.freeze({
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
        startGame: startGame,
        sendInput: sendInput,
    });
})(window);