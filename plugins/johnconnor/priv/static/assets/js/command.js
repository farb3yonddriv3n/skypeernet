var ws = undefined;

var server = [
    ["message",              message]
];

function connection_status(status)
{
    var cn = document.getElementById("connected");

    if(status) {
        cn.innerHTML = "Connected";
    } else {
        cn.innerHTML = "Disconnected";
    }
}

function delayed_request(func, cmd, payload, delay)
{
    setTimeout(function() {
               func(cmd, payload);
               }, delay);
}

function getdate(ts)
{
    var currentdate = new Date(ts);
    var datetime = currentdate.getDate() + "/"
        + (currentdate.getMonth()+1)  + "/"
        + currentdate.getFullYear() + " "
        + currentdate.getHours() + ":"
        + currentdate.getMinutes() + ":"
        + currentdate.getSeconds();
    return datetime;
}

function menuFiles(src)
{
    var filesLocalList  = document.getElementById("filesLocalList");
    var filesRemoteList = document.getElementById("filesRemoteList");
    filesLocalList.style.display = "none";
    filesRemoteList.style.display = "none";

    if (src == "local")       filesLocalList.style.display = "block";
    else if (src == "remote") filesRemoteList.style.display = "block";


    var fileItemBody = document.getElementById("fileItemBody");
    fileItemBody.innerHTML = "none";

    var fileItem = document.getElementById("fileItem");
    fileItem.style.display = "none";
}

function showFile(file)
{
    var filesLocalList = document.getElementById("filesLocalList");
    filesLocalList.style.display = "none";
    var filesRemoteList = document.getElementById("filesRemoteList");
    filesRemoteList.style.display = "none";
    var fileItemBody = document.getElementById("fileItemBody");
    var fileItemTitle = document.getElementById("fileItemTitle");
    fileItemBody.innerHTML = "";
    fileItemTitle.innerHTML = "";
    var desc = document.createElement("div");
    if (file.endsWith("mp3")) {
        desc.innerHTML = "<audio controls><source src=\"assets/media/"+file+"\" type=\"audio/mpeg\"></audio>";
        desc.setAttribute("class", "fileAudio");
    } else if(file.endsWith("mp4")) {
        desc.innerHTML = "<video width=\"800\" height=\"600\" controls><source src=\"assets/media/"+file+"\" type=\"video/mp4\"></video>";
        desc.setAttribute("class", "fileVideo");
    } else {
        desc.innerHTML = "<a href=\"assets/media/"+file+"\">"+file+"</a>";
        desc.setAttribute("class", "fileGeneric");
    }
    fileItemBody.appendChild(desc);
    fileItemTitle.innerHTML = file;
    var fileItem = document.getElementById("fileItem");
    fileItem.style.display = "block";
}

function file_onclick(element, desc, type)
{
    element.onclick = function() {
        if (type == "show_files")
            showFile(desc);
        else if (type == "job_add")
            job_add(desc);
    }
}

function message_files(parsed)
{
    var files = document.getElementById("filesLocal");
    files.innerHTML = "";
    for (i = 0; i < parsed["payload"]["blocks"].length; i++) {
        for (t = 0; t < parsed["payload"]["blocks"][i]["transactions"].length; t++) {
            var name = document.createElement("div");
            name.setAttribute("class", "filesItem");
            var desc = parsed["payload"]["blocks"][i]["transactions"][t]["description"];
            file_onclick(name, desc, "show_files");
            if ("description" in parsed["payload"]["blocks"][i]["transactions"][t])
                name.innerHTML = parsed["payload"]["blocks"][i]["transactions"][t]["description"];
            else
                name.innerHTML = parsed["payload"]["blocks"][i]["transactions"][t]["name"];
            var filename = document.createElement("div");
            filename.style.width = "100%";
            filename.appendChild(name);
            files.appendChild(filename);
        }
    }
}

function message_files_remote(parsed)
{
    var files = document.getElementById("filesRemote");
    files.innerHTML = "";
    for (i = 0; i < parsed["payload"]["roots"].length; i++) {
        for (b = 0; b < parsed["payload"]["roots"][i]["blocks"].length; b++) {
            for (t = 0; t < parsed["payload"]["roots"][i]["blocks"][b]["transactions"].length; t++) {
                var name = document.createElement("div");
                name.setAttribute("class", "filesItem");
                var filename = parsed["payload"]["roots"][i]["blocks"][b]["transactions"][t]["name"];
                name.innerHTML = filename;
                file_onclick(name, filename, "job_add");
                var filename = document.createElement("div");
                filename.style.width = "100%";
                filename.appendChild(name);
                files.appendChild(filename);
            }
        }
    }
}

function message(payload)
{
    var parsed = JSON.parse(payload);
    if (parsed["command"] == 2 && "blocks" in parsed["payload"]) {
        message_files(parsed);
    } else if (parsed["command"] == 3 && "roots" in parsed["payload"]) {
        message_files_remote(parsed);
    } else if (parsed["command"] == 1) {
    } else {
    }
}

function files_get(src)
{
    var payload = new Object();
    payload.src = src;
    send("files_get", payload);
}

function job_add(name)
{
    var payload = new Object();
    payload.name = name;
    send("job_add", payload);
}

function send(cmd, payload)
{
    var json = new Object();
    json.command = cmd;
    json.payload = payload;
    json.version = 1;
    var text = JSON.stringify(json);
    ws.send(text);
}

function status(cmd, payload)
{
    if (ws == undefined) return "disconnected";
    else                 return "connected";
}

function connect(cmd, payload)
{
    if (!("WebSocket" in window))
        return "this browser doesn't support websocket";

    if (ws != undefined)
        return "already connected";

    ws = new WebSocket("ws://localhost:8080/websocket");

    ws.onopen = function() {
        connection_status(true);
    };

    ws.onmessage = function (evt) {
        var json = JSON.parse(evt.data);
        for (var i = 0; i < server.length; i++) {
            if (server[i][0] == json.command) {
                server[i][1](json["payload"]);
                return;
            }
        }
    };

    ws.onclose = function() {
        ws = undefined;
        connection_status(false);
        delayed_request(connect, cmd, payload, 1000);
    };
}
