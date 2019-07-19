function switchTheme(){
    var button = document.getElementById("theme-button");
    var html = document.getElementsByTagName("html")[0];
    html.classList.toggle("dark");

    if(button.innerHTML == "DAY"){
        button.innerHTML = "NIGHT";
        // Expire in two months
        setCookie("theme", "night", 60*24*60*60*1000);
    } else {
        button.innerHTML = "DAY";
        // Expire in two months
        setCookie("theme", "day", 60*24*60*60*1000);
    }
    return;
}

function setCookie(cname,cvalue,extime)
{
    var d = new Date();
    d.setTime(d.getTime()+(extime));
    var expires = "expires="+d.toGMTString();
    document.cookie = cname + "=" + cvalue + ";" + expires + ";" + "path=/"; 
}

function getCookie(cname)
{
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++)
    {
        var c = ca[i].trim();
        if (c.indexOf(name)===0) return c.substring(name.length,c.length);
    }
    return "";
}

// Switch theme if cookie is set
if (getCookie('theme')=='night') {
    switchTheme();
}

// Switch theme if button is clicked.
var button = document.getElementById("theme-button");
button.addEventListener('click', switchTheme);
