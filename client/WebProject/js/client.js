/**
 * Created by vladimirgorbenko on 12.11.14.
 */

var url = "https://pp.vk.me/c624225/v624225301/731f/nSoKQRKd17c.jpg";

//var url = "https://no_data_url.jpg";

function onLoadImg(blob)
{
    var imageUrl = createImageBlobURL(blob);
    var image = document.getElementById("client_image");
    image.src = imageUrl
}

function onErrorImg(e)
{
    console.log("load image error: " + e)
}

fromPearLoader(url, onLoadImg, onErrorImg);
