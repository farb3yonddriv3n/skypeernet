var ws = undefined;
var commands = [
    [["help"],        0, "Displays available commands", help ],
    [["connect"],     0, "Connect to server",           connect ],
    [["status"],      0, "Get connection status",       status ]
];

var server = [
    ["account_signup_reply", account_signup_reply],
    ["account_login_reply",  account_login_reply],
    ["account_logout_reply", account_logout_reply],
    ["broadcast",            broadcast],
    ["devices_online",       devices_online],
    ["device_offline",       device_offline],
    ["camera_response",      camera_response],

    ["message",              message]
];

var entities = new Array();

function show_loginsignup(showlogin)
{
    var login  = document.getElementById("panel_login");
    var signup = document.getElementById("panel_signup");
    var cam    = document.getElementById("panel_cam");
    var logged = document.getElementById("logged");

    if(showlogin) {
        login.style.display  = "block";
        signup.style.display = "none";
    } else {
        login.style.display  = "none";
        signup.style.display = "block";
    }

    cam.style.display = "none";
    logged.innerHTML = "";
}

function form_signup()
{
    if (ws == undefined) return;
    var user = document.getElementById("signup_username").value;
    var pass = document.getElementById("signup_password").value;
    signup(user, pass);
}

function form_login()
{
    if (ws == undefined) return;
    var user = document.getElementById("username").value;
    var pass = document.getElementById("password").value;
    login(user, pass, "admin_device");
}

function connection_status(status)
{
    var cn = document.getElementById("connected");

    if(status) {
        cn.innerHTML = "Connected";
    } else {
        cn.innerHTML = "Disconnected";
        gui_reset();
    }
}

function account_login_reply(payload)
{
    var error = document.getElementById("login_error");
    if(payload["error"] == "ok") {
        var logged = document.getElementById("logged");
        var login  = document.getElementById("panel_login");
        var cam    = document.getElementById("panel_cam");
        login.style.display = "none";
        cam.style.display   = "block";
        logged.innerHTML = "Logout";
    } else if(payload["error"] == "invalid_login") {
        error.innerHTML = "Invalid login";
    } else {
        error.innerHTML = "General failure";
    }
}

function camera_request(pid, snapshot)
{
    var payload = new Object();
    payload.pid = pid;
    payload.snapshot = snapshot;
    send("camera_request", payload);
    var wait = document.getElementById("wait_"+pid);
    if(wait) wait.style.display = "block";
}

function sound_select(div, pid)
{
    //Create array of options to be added
    var array = ["none", "electro","droplet"];

    //Create and append select list
    var selectList = document.createElement("select");
    selectList.id = "sound_list_"+pid;
    selectList.style.width = "100%";
    div.appendChild(selectList);

    //Create and append the options
    for (var i = 0; i < array.length; i++) {
        var option = document.createElement("option");
        option.value = array[i];
        option.text = array[i];
        selectList.appendChild(option);
    }
}

function show_container(container, pid)
{
    var live    = document.getElementById("live_"+pid);
    var history = document.getElementById("history_"+pid);

    if(container == "live") {
        live.style.display    = "block";
        history.style.display = "none";
    } else {
        live.style.display    = "none";
        history.style.display = "block";
    }
}

function history_request(pid)
{
    var val = document.getElementById("history_input_"+pid).value;
    camera_request(pid, parseInt(val));
}

function history_frame(op, pid)
{
    var input = document.getElementById("history_input_"+pid);
    var frame = parseInt(input.value);
    if(op == "+" && frame < 5)      frame++;
    else if(op == "-" && frame > 1) frame--;
    input.setAttribute("value", frame);
}

function update_toggle(element)
{
    if(element.value == "Update: On")
        element.value = "Update: Off";
    else
        element.value = "Update: On";
}

function update_onclick(element, pid, type)
{
    element.onclick = function() {
        if(type == "live")
            show_container("live", pid);
        else if(type == "history")
            show_container("history", pid);
        else if(type == "-")
            history_frame("-", pid);
        else if(type == "+")
            history_frame("+", pid);
        else if(type == "history_submit")
            history_request(pid);
        else if(type == "reinit")
            camera_reinit(pid);
        else if(type == "toggle_update")
            update_toggle(element);
    };
}

/*
function update_history(history, pid)
{
    history.onclick = function() {
        show_container("history", pid);
    };
}

function update_history_minus(history_minus, pid)
{
    history_minus.onclick = function() {
        history_frame("-", pid);
    };
}
*/

function devices_online(payload)
{
    for(i = 0; i < payload["devices"].length; i++) {
        if(payload["devices"][i]["device"] == "admin_device")
            continue;

        var pid  = payload["devices"][i]["pid"];

        var top = document.createElement("div");
        top.style.height = "40px";
        var name = document.createElement("div");
        name.innerHTML = "Node: " + payload["devices"][i]["device"];
        name.style.marginTop  = "10px";
        name.style.marginLeft = "10px";
        name.style.float = "left";
        name.style.width = "250px";
        var load = document.createElement("div");
        load.style.float = "left";
        var wait = document.createElement("div");
        wait.setAttribute("id", "wait_"+pid);
        wait.style.width  = "40px";
        wait.style.height = "40px";
        wait.style.backgroundImage = "url('assets/images/spinner.gif')";
        wait.style.backgroundSize  = "40px 40px";
        load.appendChild(wait);
        top.appendChild(name);
        top.appendChild(load);

        var device = document.createElement("div");
        device.setAttribute("id", "device_"+pid);
        device.style.overflow = "hidden";
        device.style.cssFloat = "left";
        device.style.width    = "450px";
        device.style.height   = "580px";
        device.style.backgroundColor = "#efefef";
        device.style.border   = "1px solid grey";
        device.style.borderRadius = "10px";
        device.style.margin = "5px";
        var image = document.createElement("img");
        image.setAttribute("id", "image_"+pid);
        image.style.width    = "450px";
        image.style.height   = "370px";
        image.style.overflow = "hidden";

        var navbar = document.createElement("div");
        navbar.style.width = "100%";
        navbar.style.overflow = "hidden";
        var live = document.createElement("input");
        live.setAttribute("type", "submit");
        live.style.float = "left";
        live.style.width = "50%";
        live.value = "Live";
        update_onclick(live, pid, "live");
        var history = document.createElement("input");
        history.setAttribute("type", "submit");
        history.style.float = "left";
        history.style.width = "50%";
        history.value = "History";
        update_onclick(history, pid, "history");
        navbar.appendChild(live);
        navbar.appendChild(history);

        var container_history = document.createElement("div");
        container_history.style.display = "none";
        container_history.setAttribute("id", "history_"+pid);
        var history_input_all = document.createElement("div");
        history_input_all.style.width = "100%";
        history_input_all.style.overflow = "hidden";
        history_input_all.style.marginTop = "10px";
        history_input_all.style.marginBottom = "10px";
        var history_minus = document.createElement("div");
        history_minus.innerHTML = "-";
        history_minus.style.fontSize = "22pt"
        update_onclick(history_minus, pid, "-");
        history_minus.style.float = "left";
        history_minus.style.width = "25%";
        history_minus.style.textAlign = "center";
        var history_input = document.createElement("input");
        history_input.setAttribute("type", "text");
        history_input.style.width = "20px";
        history_input.setAttribute("id", "history_input_"+pid);
        history_input.setAttribute("value", "1");
        history_input.style.float = "left";
        history_input.style.width = "20%";
        history_input.style.textAlign = "center";
        var history_delimit = document.createElement("div");
        history_delimit.style.float = "left";
        history_delimit.innerHTML = "/";
        history_delimit.style.fontSize = "22pt"
        var history_max = document.createElement("input");
        history_max.setAttribute("id", "history_max_"+pid);
        history_max.value = "0";
        history_max.readOnly = true;
        history_max.style.float = "left";
        history_max.style.width = "20%";
        history_max.style.textAlign = "center";
        var history_plus = document.createElement("div");
        history_plus.innerHTML = "+";
        update_onclick(history_plus, pid, "+");
        history_plus.style.float = "left";
        history_plus.style.width = "20%";
        history_plus.style.textAlign = "center";
        history_plus.style.fontSize = "22pt"
        history_input_all.appendChild(history_minus);
        history_input_all.appendChild(history_input);
        history_input_all.appendChild(history_delimit);
        history_input_all.appendChild(history_max);
        history_input_all.appendChild(history_plus);
        var history_submit = document.createElement("input");
        history_submit.style.width = "100%";
        history_submit.setAttribute("type", "submit");
        history_submit.setAttribute("value", "Get");
        update_onclick(history_submit, pid, "history_submit");
        container_history.appendChild(history_input_all);
        container_history.appendChild(history_submit);

        var container_live = document.createElement("div");
        container_live.style.display = "block";
        container_live.setAttribute("id", "live_"+pid);
        var objects = document.createElement("div");
        objects.style.width = "100%";
        objects.style.overflow = "hidden";
        var objects_label = document.createElement("div");
        var objects_input = document.createElement("input");
        objects_input.setAttribute("id", "objects_"+pid);
        objects_label.style.width = "50%";
        objects_label.innerHTML   = "Objects";
        objects_label.style.textAlign = "center";
        objects_label.style.float = "left";
        objects_input.setAttribute("type", "text");
        objects_input.style.width = "45%";
        objects_input.readOnly    = true;
        objects_input.style.float = "left";
        objects_input.style.textAlign = "center";
        objects.appendChild(objects_label);
        objects.appendChild(objects_input);
        var ts = document.createElement("div");
        ts.style.width = "100%";
        ts.style.overflow = "hidden";
        var ts_label = document.createElement("div");
        var ts_input = document.createElement("input");
        ts_input.setAttribute("id", "timestamp_"+pid);
        ts_label.innerHTML = "Updated";
        ts_label.style.float = "left";
        ts_label.style.width = "50%";
        ts_label.style.textAlign = "center";
        ts_input.setAttribute("type", "text");
        ts_input.style.width = "45%";
        ts_input.readOnly    = true;
        ts_input.style.float = "left";
        ts_input.style.textAlign = "center";
        ts.appendChild(ts_label);
        ts.appendChild(ts_input);
        device.appendChild(top);
        device.appendChild(image);
        device.appendChild(navbar);
        var toggle = document.createElement("div");
        toggle.style.width = "100%";
        toggle.style.overflow = "hidden";
        toggle.style.marginTop = "10px";
        toggle.style.marginBottom = "10px";
        var reinit = document.createElement("input");
        reinit.style.width = "50%";
        reinit.style.float = "left";
        reinit.setAttribute("type", "submit");
        reinit.value = "Reinit";
        update_onclick(reinit, pid, "reinit");
        var toggle_update = document.createElement("input");
        toggle_update.setAttribute("type", "submit");
        toggle_update.style.float = "left";
        toggle_update.style.width = "50%";
        toggle_update.value = "Update: On";
        toggle_update.setAttribute("id", "toggle_update_"+pid);
        update_onclick(toggle_update, pid, "toggle_update");
        toggle.appendChild(reinit);
        toggle.appendChild(toggle_update);
        var toggle_update_data = document.createElement("div");
        toggle_update_data.style.width = "100%";
        toggle_update_data.style.overflow = "hidden";
        toggle_update_data.setAttribute("id", "toggle_update_data_"+pid);
        toggle_update_data.style.display = "none";
        var sound = document.createElement("div");
        sound.style.width = "100%";
        sound.style.overflow = "hidden";
        var sound_label = document.createElement("div");
        var sound_input = document.createElement("input");
        sound_label.innerHTML = "Temperature";
        sound_label.style.float = "left";
        sound_label.style.width = "50%";
        sound_label.style.textAlign = "center";
        sound_input.setAttribute("id", "temp_"+pid);
        sound_input.setAttribute("type", "text");
        sound_input.style.width = "45%";
        sound_input.readOnly    = true;
        sound_input.style.float = "left";
        sound_input.style.textAlign = "center";
        //sound_select(sound_input, pid);
        sound.appendChild(sound_label);
        sound.appendChild(sound_input);
        container_live.appendChild(toggle);
        container_live.appendChild(objects);
        container_live.appendChild(ts);
        container_live.appendChild(sound);
        container_live.appendChild(toggle_update_data);

        device.appendChild(container_live);
        device.appendChild(container_history);
        document.getElementById("panel_cam").appendChild(device);

        camera_request(payload["devices"][i]["pid"], 0);
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

function update_onclick1(element, desc)
{
    element.onclick = function() {
        showFile(desc);
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
            //name.onclick = function(){showFile(desc);};
            update_onclick1(name, desc);
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

function message(payload)
{
    var parsed = JSON.parse(payload);
    if (parsed["command"] == 2 && "blocks" in parsed["payload"]) {
        message_files(parsed);
    } else if (parsed["command"] == 1) {
        console.log("Message " + parsed["payload"]["message"] + " from " + parsed["payload"]["host"]);
    } else {
        console.log(parsed);
    }
}

function camera_response(payload)
{
    if("error" in payload && payload["error"] != 0) return;
    /*
    var sound = document.getElementById("sound_list_"+payload["pid"]);
    var sound_selection = sound.options[sound.selectedIndex].value;
    if(sound_selection != "none") {
        var audio = new Audio('./assets/images/'+sound_selection+'.mp3');
        audio.play();
    }
    */

    var image = document.getElementById("image_"+payload["pid"]);
    image.src = "data:image/png;base64," + payload["image"];
    var objects = document.getElementById("objects_"+payload["pid"]);
    objects.value = payload["objects"];
    var timestamp = document.getElementById("timestamp_"+payload["pid"]);
    timestamp.value = getdate(parseInt(payload["timestamp"]) * 1000);
    var historymax = document.getElementById("history_max_"+payload["pid"]);
    historymax.value = payload["snapshots"];
    var temp = document.getElementById("temp_"+payload["pid"]);
    if (payload["temp"].length > 0)
        temp.value = parseInt(payload["temp"][0]["temp"], 10) / 100;
    var wait = document.getElementById("wait_"+payload["pid"]);
    wait.style.display = "none";
}

function device_offline(payload)
{
    var devid = "device_"+payload["pid"];
    var dev   = document.getElementById(devid);
    if (dev) dev.parentNode.removeChild(dev);
}

function account_signup_reply(payload)
{
    var error = document.getElementById("signup_error");
    if(payload["error"] == "ok") {
        var login  = document.getElementById("panel_login");
        var signup = document.getElementById("panel_signup");
        login.style.display  = "block";
        signup.style.display = "none";
    } else if(payload["error"] == "already_exists") {
        error.innerHTML = "Account already exists";
    } else {
        error.innerHTML = "General failure";
    }
}

function gui_reset()
{
    /*
    var logged = document.getElementById("logged");
    var cams   = document.getElementById("panel_cam");
    logged.innerHTML = "";
    cams.innerHTML = '';
    show_loginsignup(true);
    */
}

function account_logout_reply(payload)
{
    if(payload["error"] == "ok") gui_reset();
}

function repid(pid)
{
    return pid.replace(">","").replace("<","").replace(/\./g,"");
}

function broadcast(payload)
{
    var update = document.getElementById("toggle_update_"+payload["pid"]);
    if(update.value == "Update: Off") return;
    if("pid" in payload &&
       "msg" in payload &&
       payload["msg"] == "freshmeat") {
        var wait = document.getElementById("wait_"+payload["pid"]);
        var live = document.getElementById("live_"+payload["pid"]);
        if(wait && wait.style.display == "none" &&
           live && live.style.display == "block")
            camera_request(payload["pid"], 0);
    }
}

function list()
{
    send("list", new Object());
    return "List requested";
}

function files_get()
{
    var payload  = new Object();
    send("files_get", payload);
}

function camera_reinit(pid)
{
    var payload  = new Object();
    payload.pid = pid;
    send("camera_reinit", payload);
}

function account_set(user, password)
{
    var payload  = new Object();
    payload.pass = password;
    payload.user = user;
    send("account_set", payload);

    return "Requested account set";
}

function login(user, password, device)
{
    var payload    = new Object();
    payload.device = device;
    payload.pass   = password;
    payload.user   = user;
    send("account_login", payload);
}

function logout()
{
    var payload    = new Object();
    send("account_logout", payload);
}

function signup(user, password)
{
    var payload  = new Object();
    payload.pass = password;
    payload.user = user;
    send("account_signup", payload);
}

function help(tokens, cmd)
{
    var r = "";
    for (var i = 0; i < commands.length; i++) {
        r += "Command: " + commands[i][0] + "/" + commands[i][1];
        r += " " + commands[i][2] + "\n";
    }

    return r;
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
        console.log("Received:");
        console.log(json);
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
