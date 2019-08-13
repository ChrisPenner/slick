 var ws = new WebSocket('ws://localhost:3030');
 ws.onopen = function() {
    ws.send("Browser client connected!");
  }
 ws.onmessage = function (event) {
   console.log(event.data);
   window.location.reload(true);
 };
