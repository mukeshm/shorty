document.getElementById("subButton").addEventListener("click", shortenUrl);
document.getElementById("closeButton").addEventListener("click", urlForm);

function postRequest(data, cb) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', "/url", true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  xhr.send(data);
  xhr.onreadystatechange = processRequest;
  function processRequest(e) {
    if (xhr.readyState == 4 && xhr.status == 200) {
      var resData = JSON.parse(xhr.responseText);
      cb(null, resData);
    } else if(xhr.readyState == 4) {
      var resData = JSON.parse(xhr.responseText);
      cb(resData);
    }
  }
}

function shortenUrl() {
  var domain = window.location.href,
      url = document.getElementById("urlField").value,
      urlRegex = /(http)s?/;
  if(url === '') {
    alert("Invalid url")
  } else {
    if(urlRegex.test(url) === false) {
      url = 'http' + url;
    }
    var data = "uri=" + url + "&domain=" + domain
    postRequest(data, function(err, resData) {
      if (err) alert(err.errorText)
      else document.getElementById("shortenedUrl").innerHTML = resData.shortURL
    })
    document.getElementById("urlForm").className = "hide";
    document.getElementById("shortUrl").className = "display";
  }
}

function urlForm() {
  document.getElementById("urlField").value = "";
  document.getElementById("urlForm").className = "display";
  document.getElementById("urlForm").className += " url-form-style"
  document.getElementById("shortUrl").className = "hide";
}
