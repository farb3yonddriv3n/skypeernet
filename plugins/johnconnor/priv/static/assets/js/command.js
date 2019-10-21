var ws = undefined;
var files_local = [];
var files_remote = [];
var filter = "";
var outbound = [];
var request_id = 0;
var peers = [];
var tunnels = [];
var endpoints = [];
var server = [
    ["message",              message]
];

function connection_status(status)
{
    var cn = document.getElementById("connected");
    if(status) cn.innerHTML = "Connected";
    else       cn.innerHTML = "Disconnected";
    show_msg(cn.innerHTML);
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
    var menuList        = document.getElementById("menu");
    var filesRemoteList = document.getElementById("filesRemoteList");
    menuList.style.display        = "none";
    filesRemoteList.style.display = "none";

    //if (src == "local")       filesLocalList.style.display = "block";
    if (src == "remote") filesRemoteList.style.display = "block";


    var fileItemBody = document.getElementById("fileItemBody");
    fileItemBody.innerHTML = "none";

    var fileItem = document.getElementById("fileItem");
    fileItem.style.display = "none";
}

function showFile(file)
{
    var menuList = document.getElementById("menu");
    menuList.style.display = "block";
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
        else if (type == "job_finalize")
            job_finalize(desc);
        else if (type == "tunnel_open")
            tunnel_open(desc);
    }
}

function search_remote()
{
    filter = document.getElementById("searchRemote").value;
    files_show();
}

function format_size(size)
{
    if (size < 1024)                      return size + "b";
    else if (size < (1024 * 1024))        return (size / 1024).toFixed(2) + "kb";
    else if (size < (1024 * 1024 * 1024)) return (size / (1024 * 1024)).toFixed(2) + "mb";
    else                                  return (size / (1024 * 1024 * 1024)).toFixed(2) + "gb";
}

function files_show_sub(src)
{
    var files = document.getElementById("filesRemote");
    for (added = 0, i = 0; i < src.length; i++) {
        var transaction = src[i];
        if (filter.length > 0 &&
            !(transaction["description"].toLowerCase().indexOf(filter) != -1 ||
                transaction["tags"].toLowerCase().indexOf(filter) != -1)) continue;
        added++;
        var transactionDiv = document.createElement("div");
        transactionDiv.setAttribute("class", "filesItem");
        var name;
        var local = (transaction["src"] == "local") ? " (local)" : "";
        var reachable = transaction["reachable"] ? " (r)" : " (u)";
        if (transaction["decryptable"])
            name = append_bodyclass(transactionDiv, transaction["description"] + local + reachable, "filesItemSub");
        else
            name = append_bodyclass(transactionDiv, transaction["name"] + local + reachable, "filesItemSub");
        name.classList.add("fileShowName");
        var size = append_bodyclass(transactionDiv, format_size(transaction["size"]), "filesItemSub");
        size.classList.add("fileShowSize");
        var complete = append_bodyclass(transactionDiv, transaction["complete"], "filesItemSub");
        if (!transaction["complete"]) {
            file_onclick(complete, transaction["name"], "job_add");
            complete.setAttribute("class", "filesItemSubClick");
        }
        complete.classList.add("fileShowComplete");
        var finalize = append_bodyclass(transactionDiv, "Finalize", "filesItemSub");
        file_onclick(finalize, transaction["name"], "job_finalize");
        finalize.setAttribute("class", "filesItemSubClick");
        finalize.classList.add("fileShowSize");
        var finalized;
        if (transaction["finalized"]) {
            finalized = append_bodyclass(transactionDiv, "View", "filesItemSub");
            file_onclick(finalized, transaction["description"], "show_files");
            finalized.setAttribute("class", "filesItemSubClick");
        } else {
            finalized = append_bodyclass(transactionDiv, "Encrypted", "filesItemSub");
        }
        finalized.classList.add("fileShowFinalized");
        var tags = append_bodyclass(transactionDiv, transaction["tags"], "filesItemSub");
        tags.classList.add("fileShowTags");
        var jobs;
        if ("jobs" in transaction)
            jobs = append_bodyclass(transactionDiv, transaction["jobs"], "filesItemSub");
        else
            jobs = append_bodyclass(transactionDiv, transaction["chunks_done"] + "/" +
                                                  transaction["chunks_total"], "filesItemSub");
        jobs.classList.add("fileShowTags");
        var tasks;
        if ("tasks" in transaction)
            tasks = append_bodyclass(transactionDiv, transaction["tasks"].length, "filesItemSub");
        else
            tasks = append_bodyclass(transactionDiv, 0, "filesItemSub");
        tasks.classList.add("fileShowTags");
        files.appendChild(transactionDiv);
    }
    return added;
}

function files_show()
{
    var added = 0;
    var files = document.getElementById("filesRemote");
    files.innerHTML = "";
    added += files_show_sub(files_remote);
    added += files_show_sub(files_local);
    if (!added) files.innerHTML = "No files found.";
}

function message_files_local(payload)
{
    files_local = [];
    for (i = 0; i < payload["blocks"].length; i++)
        for (t = 0; t < payload["blocks"][i]["transactions"].length; t++) {
            payload["blocks"][i]["transactions"][t]["src"] = "local";
            files_remote.push(payload["blocks"][i]["transactions"][t]);
        }
}

function append_bodyclass(dst, innerHtml, classname)
{
    var div = document.createElement("div");
    div.setAttribute("class", classname);
    div.innerHTML = innerHtml;
    dst.appendChild(div);
    return div;
}

function message_files_remote(parsed)
{
    files_remote = [];
    for (i = 0; i < parsed["payload"]["roots"].length; i++)
        for (b = 0; b < parsed["payload"]["roots"][i]["blocks"].length; b++)
            for (t = 0; t < parsed["payload"]["roots"][i]["blocks"][b]["transactions"].length; t++) {
                parsed["payload"]["roots"][i]["blocks"][b]["transactions"][t]["src"] = "remote";
                parsed["payload"]["roots"][i]["blocks"][b]["transactions"][t]["reachable"] = parsed["payload"]["roots"][i]["reachable"];
                files_remote.push(parsed["payload"]["roots"][i]["blocks"][b]["transactions"][t]);
            }
    message_files_local(parsed["payload"]["local"]);
    files_show();
}

function message_file_job_done(parsed)
{
    for (i = 0; i < files_remote.length; i++)
        if (files_remote[i]["name"] == parsed["payload"]["name"]) {
            files_remote[i]["complete"] = true;
            show_msg("Job done: " + files_remote[i]["name"]);
            job_finalize(files_remote[i]["name"]);
            break;
        }
    files_show();
}

function message_file_job_finalized(parsed)
{
    if (parsed["error"] != 0) {
        show_msg("File not finalized with error " + parsed["error"]);
        return;
    }
    for (i = 0; i < files_remote.length; i++)
        if (files_remote[i]["name"] == parsed["request"]["name"]) {
            show_msg("Job finalized: " + files_remote[i]["name"]);
            files_remote[i]["finalized"] = true;
            break;
        }
    files_show();
}

function message_traffic(parsed)
{
    var traffic = document.getElementById("traffic");
    traffic.innerHTML = "Download: " + parsed["payload"]["download"] +
                        " Upload: " + parsed["payload"]["upload"];
}

function message_tasks_sub(dst, parsed)
{
    for (i = 0; i < dst.length; i++) {
        dst[i]["tasks"] = [];
        for (t = 0; t < parsed["payload"]["tasks"].length; t++) {
            if (parsed["payload"]["tasks"][t]["name"] == dst[i]["name"]) {
                dst[i]["tasks"].push(parsed["payload"]["tasks"][t]["size"]);
            }
        }
    }
}

function message_tasks(parsed)
{
    message_tasks_sub(files_local, parsed);
    message_tasks_sub(files_remote, parsed);
    files_show();
}

function tunnels_show()
{
    var tunnelsDiv = document.getElementById("tunnels");
    tunnelsDiv.innerHTML = "";
    for (i = 0; i < tunnels.length; i++) {
        var t = document.createElement("div");
        t.setAttribute("class", "tunnelUser");
        append_bodyclass(t, "localhost:" + tunnels[i]["src_port"] + " -> " + tunnels[i]["pubkeyhash"] + ":" + tunnels[i]["dst_port"], "none");
        tunnelsDiv.append(t);
    }
}

function endpoints_show()
{
    var endpointsDiv = document.getElementById("endpoints");
    endpointsDiv.innerHTML = "";
    for (i = 0; i < endpoints.length; i++) {
        var e = document.createElement("div");
        e.setAttribute("class", "tunnelUser");
        append_bodyclass(e, endpoints[i]["src_port"] + " -> localhost:" + endpoints[i]["dst_port"]);
        endpointsDiv.append(e);
    }
}

function message_tunnel_dump(parsed)
{
    if (!("tunnels" in parsed["payload"])) return;
    tunnels = parsed["payload"]["tunnels"];
    tunnels_show();
}

function message_endpoint_dump(parsed)
{
    if (!("endpoints" in parsed["payload"])) return;
    endpoints = parsed["payload"]["endpoints"];
    endpoints_show();
}

function message_endpoint_open(parsed)
{
    endpoints.push(parsed["payload"]);
    endpoints_show();
}

function message_endpoint_close(parsed)
{
    for (i = 0; i < endpoints.length; i++) {
        if (endpoints[i]["dst_port"] == parsed["payload"]["dst_port"] &&
            endpoints[i]["src_port"] == parsed["payload"]["src_port"]) {
            endpoints.splice(i, 1);
            endpoints_show();
            break;
        }
    }
}

function message_tunnel_open(parsed)
{
    if (parsed["payload"]["success"] != true) {
        show_msg("Cannot create tunnel " + parsed["payload"]["pubkeyhash"] + ":" + parsed["payload"]["dst_port"]);
        return;
    }
    show_msg("Tunnel open: " + parsed["payload"]["pubkeyhash"] + ":" + parsed["payload"]["dst_port"]);
    tunnels.push(parsed["payload"]);
    tunnels_show();
}

function message_jobs_dump(parsed)
{
    for (i = 0; i < files_remote.length; i++) {
        for (j = 0; j < parsed["payload"]["jobs"].length; j++) {
            if (files_remote[i]["name"] == parsed["payload"]["jobs"][j]["name"]) {
                files_remote[i]["jobs"] = parsed["payload"]["jobs"][j]["counter_done"] + "/" +
                                          parsed["payload"]["jobs"][j]["chunks_size"];
                break;
            }
        }
    }
    files_show();
}

function peers_show()
{
    var peersDiv = document.getElementById("peers");
    peersDiv.innerHTML = "";
    for (i = 0; i < peers.length; i++) {
        if (peers[i]["type"] == 1) continue;
        var np = document.createElement("div");
        np.setAttribute("class", "tunnelUser");
        append_bodyclass(np, peers[i]["pubkeyhash"], "peerOnline");
        var input = document.createElement("input");
        input.setAttribute("class", "tunnelPort");
        input.setAttribute("id", "port_" + peers[i]["pubkeyhash"]);
        np.appendChild(input);
        type = append_bodyclass(np, "OPEN", "tunnelOpen");
        file_onclick(type, peers[i]["pubkeyhash"], "tunnel_open");
        peersDiv.append(np);
    }
}

function message_peers(parsed)
{
    peers = parsed["payload"]["peers"];
    peers_show();
}

function message_peer_online(parsed)
{
    for (i = 0; i < peers.length; i++)
        if (peers[i]["pubkeyhash"] == parsed["payload"]["pubkeyhash"])
            return;
    peers.push(parsed["payload"]);
    show_msg("Peer online: " + parsed["payload"]["pubkeyhash"]);
    peers_show();
}

function message_peer_offline(parsed)
{
    for (i = 0; i < peers.length; i++)
        if (peers[i]["pubkeyhash"] == parsed["payload"]["pubkeyhash"]) {
            show_msg("Peer offline: " + parsed["payload"]["pubkeyhash"]);
            peers.splice(i, 1);
            peers_show();
            break;
        }
}

function message_message(parsed)
{
    if ("message" in parsed["payload"])
        show_msg(parsed["payload"]["message"]);
}

function message(payload)
{
    var parsed = JSON.parse(payload);
    if (parsed["command"] == 0) {
        message_peers(parsed);
    } else if (parsed["command"] == 1) {
        message_message(parsed);
    } else if (parsed["command"] == 2 && "blocks" in parsed["payload"]) {
        message_files_local(parsed["payload"]);
    } else if (parsed["command"] == 3 && "roots" in parsed["payload"]) {
        message_files_remote(parsed);
    } else if (parsed["command"] == 4) {
        message_peer_online(parsed);
    } else if (parsed["command"] == 5) {
        message_peer_offline(parsed);
    } else if (parsed["command"] == 6) {
        message_jobs_dump(parsed);
    } else if (parsed["command"] == 7) {
        message_file_job_done(parsed);
    } else if (parsed["command"] == 9) {
        message_file_job_finalized(parsed);
    } else if (parsed["command"] == 16) {
        message_traffic(parsed);
    } else if (parsed["command"] == 17) {
        message_tasks(parsed);
    } else if (parsed["command"] == 18) {
        message_tunnel_open(parsed);
    } else if (parsed["command"] == 20) {
        message_tunnel_dump(parsed);
    } else if (parsed["command"] == 22) {
        message_endpoint_dump(parsed);
    } else if (parsed["command"] == 21) {
        message_endpoint_open(parsed);
    } else if (parsed["command"] == 23) {
        message_endpoint_close(parsed);
    } else {
        console.log("unsupported");
    }
}

function peers_get()
{
    var payload = new Object();
    send("peers_get", payload);
}

function tunnels_get()
{
    var payload = new Object();
    send("tunnel_dump", payload);
}

function endpoints_get()
{
    var payload = new Object();
    send("endpoint_dump", payload);
}

function files_get(src)
{
    var payload = new Object();
    payload.src = src;
    send("files_get", payload);
}

function tunnel_open(pubkeyhash)
{
    var payload = new Object();
    payload.pubkeyhash = pubkeyhash;
    var input = document.getElementById("port_" + pubkeyhash);
    if (!input) return;
    payload.port = input.value;
    send("tunnel_open", payload);
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
    json.request_id = request_id;
    var text = JSON.stringify(json);
    var rq = new Object();
    rq.id = request_id++;
    rq.time = Date.now();
    rq.json = json;
    outbound.push(rq);
    ws.send(text);
}

function status(cmd, payload)
{
    if (ws == undefined) return "disconnected";
    else                 return "connected";
}

function confirm_message(payload)
{
    var json = JSON.parse(payload);
    if (!("request" in json) || ("request" in json && json["request"] == null))
        return;
    for (i = 0; i < outbound.length; i++) {
        if (json["request"]["request_id"] == outbound[i].id) {
            outbound.splice(i, 1);
            break;
        }
    }
}

function show_msg(msg)
{
    term.echo(getdate(Date.now()) + " " + msg);
    /*
    var todiv = document.getElementById("timedout");
    todiv.innerHTML = msg;
    $("#timedout").fadeIn(2000);
    $("#timedout").fadeOut(2000);
    */
}

function confirmed_check(p1, p2)
{
    for (i = 0; i < outbound.length; i++) {
        if (outbound[i].time < (Date.now() - 5000)) {
            show_msg("Request id " + outbound[i].id + " timed out.");
            show_msg(JSON.stringify(outbound[i].json));
            outbound.splice(i, 1);
        }
    }
    delayed_request(confirmed_check, "", "", 1000);
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
        peers_get("");
        tunnels_get("");
        endpoints_get("");
        delayed_request(confirmed_check, "", "", 1000);
    };

    ws.onmessage = function (evt) {
        var json = JSON.parse(evt.data);
        for (var i = 0; i < server.length; i++) {
            if (server[i][0] == json.command) {
                if ("payload" in json)
                    confirm_message(json["payload"]);
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
