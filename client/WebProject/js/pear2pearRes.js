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
                var obj = {type: "imageAdded", url: url};
                roomSocket.send(JSON.stringify(obj));
            }

            sentSrvCachedURLs = urlsToSend;
        };

        roomSocket.onmessage = function(event) {
            //TODO process income pears
        };

        function reconnectSocket() {

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
                    var obj = {type: "imageAdded", url: url};
                    roomSocket.send(JSON.stringify(obj));
                    sentSrvCachedURLs.push(url)
                }
            }
        } else if (roomSocket.readyState != WebSocket.CONNECTING) {

            console.log("error: roomSocket.readyState: " + roomSocket.readyState)
        }
    }
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

    xhr.onload = function(e) {
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

function fromPearLoader(url, onload, onerror)
{

}
