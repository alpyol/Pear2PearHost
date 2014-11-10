/**
 * Created by vladimirgorbenko on 09.11.14.
 */

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

function imageLoader(url, onload, onerror)
{
    var imageBlob = cachedImageBlobsByURL[url];

    if (imageBlob) {
        onload(createImageBlobURL(imageBlob));
        return
    }

    var callbacksAr = loadingCallbacksByURL[url];

    var callbacks = { onload: onload, onerror: onerror };

    if (callbacksAr && callbacksAr.length > 0) {
        callbacksAr.push(callbacks);
        return
    }

    loadingCallbacksByURL[url] = [callbacks];

    networkImageLoader(url)
}

function createImageBlobURL(blob)
{
    var urlCreator = window.URL || window.webkitURL;
    var result = urlCreator.createObjectURL(blob);
    return result
}

var roomSocket = null;//new WebSocket("ws://localhost:27001/ws");
var roomSocketQueue = [];
//var roomSocket = new WebSocket("ws://localhost:27001/ws");

function processOnLoadImage(imageBlob, url)
{
    cachedImageBlobsByURL[url] = imageBlob;

    if (roomSocket == null) {
        roomSocket = new WebSocket("ws://localhost:27001/ws");

        roomSocket.onopen = function() {

            var roomSocketQueueCopy = roomSocketQueue;
            roomSocketQueue = [];

            for (var i=0; i<roomSocketQueueCopy.length; i++) {
                roomSocket.send(roomSocketQueueCopy[i])
            }
        };

        roomSocket.onmessage = function(event) {

            //TODO process income pears
        };

        roomSocket.onerror = function(error) {

            console.log("room web socket error: " + error);
            //TODO try recconect after delay
        };
    }

    var imageAdded = { type: "imageAdded", url: url };

    if (roomSocket.readyState == WebSocket.OPEN) {

        roomSocket.send(imageAdded)
    } else if (roomSocket.readyState == WebSocket.CONNECTING) {

        roomSocketQueue.push(imageAdded)
    }
}

function networkImageLoader(url)
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

    xhr.onload = function( e ) {
        // Obtain a blob: URL for the image data.
        var arrayBufferView = new Uint8Array(this.response);
        var imageBlob = new Blob([arrayBufferView], {type: "image/jpeg"});

        processOnLoadImage(imageBlob, url);

        var imageURL = createImageBlobURL(imageBlob);

        var callbacksAr = loadingCallbacksByURL[url];
        loadingCallbacksByURL[url] = null;
        for (var i=0; i<callbacksAr.length; i++) {
            if (callbacksAr[i].onload) {
                callbacksAr[i].onload(imageURL)
            }
        }
    };

    xhr.onerror = function(e) {
        var callbacksAr = loadingCallbacksByURL[url];
        loadingCallbacksByURL[url] = null;
        for (var i=0; i<callbacksAr.length; i++) {
            if (callbacksAr[i].onload) {
                callbacksAr[i].onerror(e)
            }
        }
    };

    //TODO implement progress and other callbacks

    xhr.onloadend = function() {
    };

    xhr.send();
}
