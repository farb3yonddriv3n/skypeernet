var ws = undefined;
var files_local = [];
var files_remote = [];
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

function menu_files(src)
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

function search_remote()
{
    var search = document.getElementById("searchRemote");
    files_show("remote", search.value);
}

function files_show(src, filter)
{
    if (src == "local") {
        var files = document.getElementById("filesLocal");
        files.innerHTML = "";
        for (i = 0; i < files_local.length; i++) {
            var name = document.createElement("div");
            name.setAttribute("class", "filesItem");
            var desc = files_local[i]["description"];
            file_onclick(name, desc, "show_files");
            if ("description" in files_local[i])
                name.innerHTML = files_local[i]["description"];
            else
                name.innerHTML = files_local[i]["name"];
            var filename = document.createElement("div");
            filename.style.width = "100%";
            filename.appendChild(name);
            files.appendChild(filename);
        }
    } else if (src == "remote") {
        var files = document.getElementById("filesRemote");
        files.innerHTML = "";
        for (added = 0, i = 0; i < files_remote.length; i++) {
            var transaction = files_remote[i];
            if (filter.length > 0 &&
                !(transaction["description"].indexOf(filter) != -1 ||
                  transaction["tags"].indexOf(filter) != -1)) continue;
            added++;
            var transactionDiv = document.createElement("div");
            transactionDiv.setAttribute("class", "filesItem");
            if (transaction["decryptable"])
                var name = files_item_sub(transactionDiv, transaction["description"]);
            else
                var name = files_item_sub(transactionDiv, transaction["name"]);
            name.classList.add("fileShowName");
            var size = files_item_sub(transactionDiv, transaction["size"]);
            size.classList.add("fileShowSize");
            var complete = files_item_sub(transactionDiv, transaction["complete"]);
            if (!transaction["complete"]) {
                file_onclick(complete, transaction["name"], "job_add");
                complete.setAttribute("class", "filesItemSubClick");
            }
            complete.classList.add("fileShowComplete");
            var finalized;
            if (transaction["finalized"]) {
                    finalized = files_item_sub(transactionDiv, "View");
                    file_onclick(finalized, transaction["description"], "show_files");
                    finalized.setAttribute("class", "filesItemSubClick");
            } else {
                finalized = files_item_sub(transactionDiv, "Encrypted");
            }
            finalized.classList.add("fileShowFinalized");
            files_item_sub(transactionDiv, transaction["tags"]);
            files.appendChild(transactionDiv);
            if (!added) files.innerHTML = "No files found.";
        }
    }
}

function message_files_local(parsed)
{
    files_local = [];
    for (i = 0; i < parsed["payload"]["blocks"].length; i++)
        for (t = 0; t < parsed["payload"]["blocks"][i]["transactions"].length; t++)
            files_local.push(parsed["payload"]["blocks"][i]["transactions"][t]);
    files_show("local", "");
}

function files_item_sub(dst, innerHtml)
{
    var div = document.createElement("div");
    div.setAttribute("class", "filesItemSub");
    div.innerHTML = innerHtml;
    dst.appendChild(div);
    return div;
}

function message_files_remote(parsed)
{
    files_remote = [];
    for (i = 0; i < parsed["payload"]["roots"].length; i++)
        for (b = 0; b < parsed["payload"]["roots"][i]["blocks"].length; b++)
            for (t = 0; t < parsed["payload"]["roots"][i]["blocks"][b]["transactions"].length; t++)
                files_remote.push(parsed["payload"]["roots"][i]["blocks"][b]["transactions"][t]);
    files_show("remote", "");
}

function message_file_job_done(parsed)
{
    for (i = 0; i < files_remote.length; i++)
        if (files_remote[i]["name"] == parsed["payload"]["name"]) {
            files_remote[i]["complete"] = true;
            job_finalize(files_remote[i]["name"]);
            break;
        }
    files_show("remote", "");
}

function message_file_job_finalized(parsed)
{
    for (i = 0; i < files_remote.length; i++)
        if (files_remote[i]["name"] == parsed["request"]["name"]) {
            files_remote[i]["finalized"] = true;
            break;
        }
    files_show("remote", "");
}

function message(payload)
{
    var parsed = JSON.parse(payload);
    if (parsed["command"] == 2 && "blocks" in parsed["payload"]) {
        message_files_local(parsed);
    } else if (parsed["command"] == 3 && "roots" in parsed["payload"]) {
        message_files_remote(parsed);
    } else if (parsed["command"] == 1) {
    } else if (parsed["command"] == 7) {
        message_file_job_done(parsed);
    } else if (parsed["command"] == 9) {
        message_file_job_finalized(parsed);
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

function job_finalize(name)
{
    var payload = new Object();
    payload.name = name;
    send("job_finalize", payload);
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
        menu_files("remote");
        files_get("remote");
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
