/**
 * Created by vladimirgorbenko on 09.11.14.
 */

var SmartImageProto = Object.create(HTMLImageElement.prototype);

SmartImageProto.createdCallback = function() {
    var attr = this.attributes["csrc"];
    if (attr) {
        this.src = attr.value;
    }
};

var SmartImage = document.registerElement('smart-img', {
    prototype: SmartImageProto,
    extends: 'img'
});

//DOMContentLoaded

document.body.onload = function() {

};

//examples - http://jsfiddle.net/Jan_Miksovsky/yy7Zs/

function imageLoader(url, callbacl)
{

}

function image_upload_action()
{
    alert("image_upload_action");
}