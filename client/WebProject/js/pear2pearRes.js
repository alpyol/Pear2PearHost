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

function processOnLoadImages(urls)
{
    function contains(a, obj) {
        var i = a.length;
        while (i--) {
            if (a[i] === obj) {
                return true;
            }
        }
        return false;
    }

    if (roomSocket == null) {
        roomSocket = new WebSocket("ws://localhost:27001/ws");

        roomSocket.onopen = function() {

            var urlsToSendAll = Object.keys(cachedImageBlobsByURL);

            var urlsToSend = [];

            for (var i=0; i<urlsToSendAll.length; i++) {

                var url = urlsToSendAll[i];
                if (!contains(sentSrvCachedURLs, url)) {
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
                if (!contains(sentSrvCachedURLs, url)) {
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
        //var allURLs = Object.keys(cachedImageBlobsByURL);

        //if (contains(allURLs, incomingMessage.url)) {


        //} else
        {
            var obj = {msgType: "NoRequestedURL", cpid: incomingMessage.cpid};
            roomSocket.send(JSON.stringify(obj));
        }
    }
}

function fromPearLoader(url, onload, onerror)
{
    var roomSocket = new WebSocket("ws://localhost:27002/ws");

    roomSocket.onopen = function() {

        var cmd = {msgType: "RequestOffer", url: url};
        roomSocket.send(JSON.stringify(cmd));
        console.log("cmd sent: " + JSON.stringify(cmd));
    };

    roomSocket.onmessage = function(event) {

        var incomingMessage = JSON.parse(event.data);
        if (incomingMessage.msgType == 'NoRequestedURL') {
            onerror('no image for url: ' + url)
        }
    };

    roomSocket.onerror = function(error) {
        console.log("client web socket error: " + error);
        roomSocket = null;
    };

    roomSocket.onclose = function(event) {
        console.log("client connection closed code: " +  + event.code + ' reason: ' + event.reason + ' event.wasClean: ' + event.wasClean);
        roomSocket = null;
    };
}

//var sendChannel, receiveChannel;
//
//function createConnection() {
//    var servers = null;
//    window.localPeerConnection = new webkitRTCPeerConnection(servers,
//        {optional: [{RtpDataChannels: true}]});
//    trace('Created local peer connection object localPeerConnection');
//
//    try {
//        // Reliable Data Channels not yet supported in Chrome
//        sendChannel = localPeerConnection.createDataChannel("sendDataChannel",
//            {reliable: false});
//        trace('Created send data channel');
//    } catch (e) {
//        alert('Failed to create data channel. ' +
//        'You need Chrome M25 or later with RtpDataChannel enabled');
//        trace('createDataChannel() failed with exception: ' + e.message);
//    }
//    localPeerConnection.onicecandidate = gotLocalCandidate;
//    sendChannel.onopen  = handleSendChannelStateChange;
//    sendChannel.onclose = handleSendChannelStateChange;
//
//    window.remotePeerConnection = new webkitRTCPeerConnection(servers,
//        {optional: [{RtpDataChannels: true}]});
//    trace('Created remote peer connection object remotePeerConnection');
//
//    remotePeerConnection.onicecandidate = gotRemoteIceCandidate;
//    remotePeerConnection.ondatachannel  = gotReceiveChannel;
//
//    localPeerConnection.createOffer(gotLocalDescription);
//    startButton.disabled = true;
//    closeButton.disabled = false;
//}
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
//function gotLocalDescription(desc) {
//    localPeerConnection.setLocalDescription(desc);
//    trace('Offer from localPeerConnection \n' + desc.sdp);
//    remotePeerConnection.setRemoteDescription(desc);
//    remotePeerConnection.createAnswer(gotRemoteDescription);
//}
//
//function gotRemoteDescription(desc) {
//    remotePeerConnection.setLocalDescription(desc);
//    trace('Answer from remotePeerConnection \n' + desc.sdp);
//    localPeerConnection.setRemoteDescription(desc);
//}
//
//function gotLocalCandidate(event) {
//    trace('local ice callback');
//    if (event.candidate) {
//        remotePeerConnection.addIceCandidate(event.candidate);
//        trace('Local ICE candidate: \n' + event.candidate.candidate);
//    }
//}
//
//function gotRemoteIceCandidate(event) {
//    trace('remote ice callback');
//    if (event.candidate) {
//        localPeerConnection.addIceCandidate(event.candidate);
//        trace('Remote ICE candidate: \n ' + event.candidate.candidate);
//    }
//}
//
//function gotReceiveChannel(event) {
//    trace('Receive Channel Callback');
//    receiveChannel = event.channel;
//    receiveChannel.onmessage = handleMessage;
//    receiveChannel.onopen  = handleReceiveChannelStateChange;
//    receiveChannel.onclose = handleReceiveChannelStateChange;
//}
//
//function handleMessage(event) {
//    trace('Received message: ' + event.data);
//    document.getElementById("dataChannelReceive1").value = event.data;
//}
//
//function handleSendChannelStateChange() {
//    var readyState = sendChannel.readyState;
//    trace('Send channel state is: ' + readyState);
//    if (readyState == "open") {
//        dataChannelSend.disabled = false;
//        dataChannelSend.focus();
//        dataChannelSend.placeholder = "";
//        sendButton.disabled = false;
//        closeButton.disabled = false;
//    } else {
//        dataChannelSend.disabled = true;
//        sendButton.disabled = true;
//        closeButton.disabled = true;
//    }
//}
//
//function handleReceiveChannelStateChange() {
//    var readyState = receiveChannel.readyState;
//    trace('Receive channel state is: ' + readyState);
//}
