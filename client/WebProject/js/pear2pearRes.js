/**
 * Created by vladimirgorbenko on 09.11.14.
 */

//TODO !!!! send file as chunks - http://www.html5rocks.com/en/tutorials/webrtc/basics/

//init custom elements
(function() {

    var SmartImageProto = Object.create(HTMLImageElement.prototype);

    SmartImageProto.createdCallback = function() {

        var imageThis = this;
        var attr = imageThis.attributes["csrc"];

        if (attr) {
            var value = attr.value;

            try {
                imageLoader(value, function(newSrc) {
                    var attr = imageThis.attributes["csrc"];
                    if (attr.value == value) {
                        imageThis.src = newSrc;
                    }
                })
            } catch (ex) {
                console.log("image load error: " + ex);
                imageThis.src = value;
            }
        }
    };

    /*var SmartImage = */document.registerElement('smart-img', {
        prototype: SmartImageProto,
        extends: 'img'
    });
}());

var cachedImageBlobsByURL = {};
var loadingCallbacksByURL = {};

function cachedLoader(loader, key, resultCache, pendingCallbacks, onload, onerror)
{
    var imageBlob = resultCache[key];

    if (imageBlob) {
        onload(createImageBlobURL(imageBlob));
        return
    }

    var callbacksAr = pendingCallbacks[key];

    var callbacks = { onload: onload, onerror: onerror };

    if (callbacksAr && callbacksAr.length > 0) {
        callbacksAr.push(callbacks);
        return
    }

    pendingCallbacks[key] = [callbacks];

    function onLoadWrapper(result) {

        resultCache[key] = result;

        var callbacksAr = pendingCallbacks[key];
        pendingCallbacks[key] = null;
        for (var i=0; i<callbacksAr.length; i++) {
            if (callbacksAr[i].onload) {
                callbacksAr[i].onload(result)
            }
        }
    }

    function onErrorWrapper(e) {

        var callbacksAr = pendingCallbacks[key];
        pendingCallbacks[key] = null;
        for (var i=0; i<callbacksAr.length; i++) {
            if (callbacksAr[i].onerror) {
                callbacksAr[i].onerror(e)
            }
        }
    }

    loader(key, onLoadWrapper, onErrorWrapper)
}

function imageLoader(url, onload, onerror)
{
    function onLoadWrapper(imageBlob) {

        var imageURL = createImageBlobURL(imageBlob);
        onload(imageURL);
        processOnLoadImages([url]);
    }

    cachedLoader(networkImageLoader, url, cachedImageBlobsByURL, loadingCallbacksByURL, onLoadWrapper, onerror)
}

function createImageBlobURL(blob)
{
    var urlCreator = window.URL || window.webkitURL;
    var result = urlCreator.createObjectURL(blob);
    return result
}

function networkImageLoader(url, onload, onerror)
{
    //TODO try load from pear first

    function createCORSRequest(method, url) {
        var xhr = new XMLHttpRequest();
        if ("withCredentials" in xhr) {

            // Check if the XMLHttpRequest object has a "withCredentials" property.
            // "withCredentials" only exists on XMLHTTPRequest2 objects.
            xhr.open(method, url, true);
        } else if (typeof XDomainRequest != "undefined") {

            // Otherwise, check if XDomainRequest.
            // XDomainRequest only exists in IE, and is IE's way of making CORS requests.
            xhr = new XDomainRequest();
            xhr.open(method, url);
        } else {
            // Otherwise, CORS is not supported by the browser.
            xhr = null;
        }
        return xhr;
    }

    // Simulate a call to Dropbox or other service that can
    // return an image as an ArrayBuffer.
    var xhr = createCORSRequest("GET", url);
    //xhr.setRequestHeader('Access-Control-Allow-Credentials', '*');
    //xhr.head
    //xhr.withCredentials = true;

    // Ask for the result as an ArrayBuffer.
    xhr.responseType = "arraybuffer";

    xhr.onload = function(res) {
        // Obtain a blob: URL for the image data.
        var arrayBufferView = new Uint8Array(this.response);
        var imageBlob = new Blob([arrayBufferView], {type: "image/jpeg"});

        onload(imageBlob);
    };

    xhr.onerror = function(e) {
        onerror(e)
    };

    //TODO implement progress and other callbacks

    xhr.onloadend = function() {
    };

    xhr.send();
}

var roomSocket = null;//new WebSocket("ws://localhost:27001/ws");
var sentSrvCachedURLs = [];

function containsObjInArr(a, obj) {
    var i = a.length;
    while (i--) {
        if (a[i] === obj) {
            return true;
        }
    }
    return false;
}

function processOnLoadImages(urls)
{
    if (roomSocket == null) {
        roomSocket = new WebSocket("ws://localhost:27001/ws");

        roomSocket.onopen = function() {

            var urlsToSendAll = Object.keys(cachedImageBlobsByURL);

            var urlsToSend = [];

            for (var i=0; i<urlsToSendAll.length; i++) {

                var url = urlsToSendAll[i];
                if (!containsObjInArr(sentSrvCachedURLs, url)) {
                    urlsToSend.push(url);
                }
            }

            for (var i=0; i<urlsToSend.length; i++) {

                var url = urlsToSend[i];
                var obj = {msgType: "ImageAdded", url: url};
                roomSocket.send(JSON.stringify(obj));
            }

            sentSrvCachedURLs = urlsToSend;
        };

        roomSocket.onmessage = function(event) {

            processServerSocketMessage(roomSocket, event.data);
        };

        function reconnectSocket() {

            roomSocket = null;
            sentSrvCachedURLs = [];
            function func() {
                processOnLoadImages([])
            }
            setTimeout(func, 3*1000);
        }

        roomSocket.onerror = function(error) {
            console.log("room web socket error: " + error);
            reconnectSocket()
        };

        roomSocket.onclose = function(event) {
            console.log("connection closed code: " +  + event.code + ' reason: ' + event.reason + ' event.wasClean: ' + event.wasClean);
            reconnectSocket()
        };
    } else {

        if (roomSocket.readyState == WebSocket.OPEN) {

            for (var i=0; i<urls.length; i++) {

                var url = urls[i];
                if (!containsObjInArr(sentSrvCachedURLs, url)) {
                    var obj = {msgType: "ImageAdded", url: url};
                    roomSocket.send(JSON.stringify(obj));
                    sentSrvCachedURLs.push(url)
                }
            }
        } else if (roomSocket.readyState != WebSocket.CONNECTING) {

            console.log("error: roomSocket.readyState: " + roomSocket.readyState)
        }
    }
}

function processServerSocketMessage(socket, msg)
{
    var incomingMessage = JSON.parse(msg);
    if (incomingMessage.msgType == 'RequestOffer') {

        console.log("got offer request: " + msg);
        var imageBlob = cachedImageBlobsByURL[incomingMessage.url];

        if (imageBlob != null) {
            processRequestOffer(imageBlob, incomingMessage)
        } else {
            var obj = {msgType: "NoRequestedURL", cpid: incomingMessage.cpid};
            socket.send(JSON.stringify(obj));
        }
    }
}

function processRequestOffer(imageBlob, incomingMessage)
{
    var dataToSend = [];

    var serverSocket = new WebSocket("ws://localhost:27003/ws");

    serverSocket.onopen = function() {
        for (var i=0; i<dataToSend.length; i++) {
            serverSocket.send(dataToSend[i]);
        }
        dataToSend = [];
    };

    serverSocket.onmessage = function(event) {
    };

    serverSocket.onerror = function(error) {
        serverSocket = null;
    };

    serverSocket.onclose = function(event) {
        serverSocket = null;
    };

    function socketSend(msg)
    {
        if (serverSocket.readyState == WebSocket.OPEN) {
            serverSocket.send(msg);
        } else {
            dataToSend.push(msg);
        }
    }

    var localPeerConnection = new webkitRTCPeerConnection(null, {optional: [{RtpDataChannels: true}]});
    try {
        // Reliable Data Channels not yet supported in Chrome
        var sendChannel = localPeerConnection.createDataChannel("sendDataChannel", {reliable: false});
        console.log('Created send data channel');
        console.log('Created local peer connection object localPeerConnection');

        function gotLocalCandidate(event) {
            console.log('local ice callback');
            if (event.candidate) {
                //console.log('Local ICE candidate: \n' + event.candidate.candidate);
                var obj = {msgType: "SendIceCandidate", candidate: JSON.stringify(event.candidate), cpid: incomingMessage.cpid};
                socketSend(JSON.stringify(obj));
            }
        }

        localPeerConnection.onicecandidate = gotLocalCandidate;

        function handleSendChannelStateChange() {
            var readyState = sendChannel.readyState;
            trace('Send channel state is: ' + readyState);
            if (readyState == "open") {
                console.log('!!!!! READY TO SEND !!!!!!');
            }
        }

        sendChannel.onopen  = handleSendChannelStateChange;
        sendChannel.onclose = handleSendChannelStateChange;

        function gotLocalDescription(desc) {
            localPeerConnection.setLocalDescription(desc);
            //console.log('Offer from localPeerConnection \n' + desc.sdp);
            var obj = {msgType: "SendOffer", offer: JSON.stringify(desc), cpid: incomingMessage.cpid};
            socketSend(JSON.stringify(obj));
        }

        localPeerConnection.createOffer(gotLocalDescription);
    } catch (e) {
        console.log('createDataChannel() failed with exception: ' + e.message);
    }
}

function fromPearLoader(url, onload, onerror)
{
    var clientSocket = new WebSocket("ws://localhost:27002/ws");

    var remotePeerConnection = null;

    clientSocket.onopen = function() {

        var cmd = {msgType: "RequestOffer", url: url};
        clientSocket.send(JSON.stringify(cmd));
        console.log("cmd sent: " + JSON.stringify(cmd));
    };

    clientSocket.onmessage = function(event) {

        function lazyRemotePeerConnection() {

            if (remotePeerConnection != null) {
                return remotePeerConnection;
            }

            remotePeerConnection = new webkitRTCPeerConnection(null, {optional: [{RtpDataChannels: true}]});
            console.log('Created remote peer connection object remotePeerConnection');

            function gotReceiveChannel(event) {
                console.log('Receive Channel Callback');
            }

            function gotRemoteIceCandidate(event) {
                console.log('remote ice callback');
            }

            remotePeerConnection.onicecandidate = gotRemoteIceCandidate;
            remotePeerConnection.ondatachannel  = gotReceiveChannel;

            return remotePeerConnection
        }

        console.log("msg: " + event.data);
        var incomingMessage = JSON.parse(event.data);
        if (incomingMessage.msgType == 'NoRequestedURL') {
            onerror('no image for url: ' + url)
        } else if (incomingMessage.msgType == 'Error') {
            onerror('no image for url: ' + incomingMessage.msg)
        } else if (incomingMessage.msgType == 'Offer') {

            var pear = lazyRemotePeerConnection();

            function gotRemoteDescription(desc) {

                if (remotePeerConnection) {
                    remotePeerConnection.setLocalDescription(desc);
                    console.log('!!!!! Answer from remotePeerConnection \n' + desc.sdp);
                    var cmd = {msgType: "SendAnswer", answer: JSON.stringify(desc)};
                    clientSocket.send(JSON.stringify(cmd));
                }
            }

            var desc = new RTCSessionDescription(JSON.parse(incomingMessage.offer));

            pear.setRemoteDescription(desc);
            pear.createAnswer(gotRemoteDescription);
        } else if (incomingMessage.msgType == 'Candidate') {

            var pear = lazyRemotePeerConnection();

            var candidate = new RTCIceCandidate(JSON.parse(incomingMessage.candidate));

            pear.addIceCandidate(candidate);
        }
    };

    clientSocket.onerror = function(error) {
        console.log("client web socket error: " + error);
        remotePeerConnection = null;
        clientSocket = null;
    };

    clientSocket.onclose = function(event) {
        console.log("client connection closed code: " +  + event.code + ' reason: ' + event.reason + ' event.wasClean: ' + event.wasClean);
        remotePeerConnection = null;
        clientSocket = null;
    };
}

//var sendChannel, receiveChannel;
//
//function sendData() {
//    var data = document.getElementById("dataChannelSend").value;
//    sendChannel.send(data);
//    trace('Sent data: ' + data);
//}
//
//function closeDataChannels() {
//    trace('Closing data channels');
//    sendChannel.close();
//    trace('Closed data channel with label: ' + sendChannel.label);
//    receiveChannel.close();
//    trace('Closed data channel with label: ' + receiveChannel.label);
//    localPeerConnection.close();
//    remotePeerConnection.close();
//    localPeerConnection  = null;
//    remotePeerConnection = null;
//    trace('Closed peer connections');
//    startButton.disabled = false;
//    sendButton.disabled = true;
//    closeButton.disabled = true;
//    dataChannelSend.value = "";
//    dataChannelReceive.value = "";
//    dataChannelSend.disabled = true;
//    dataChannelSend.placeholder = "Press Start, enter some text, then press Send.";
//}
//
//function handleMessage(event) {
//    trace('Received message: ' + event.data);
//    document.getElementById("dataChannelReceive1").value = event.data;
//}
//
//function handleReceiveChannelStateChange() {
//    var readyState = receiveChannel.readyState;
//    trace('Receive channel state is: ' + readyState);
//}
