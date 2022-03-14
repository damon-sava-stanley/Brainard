function getRememberances(limit, offset, onSuccess, onError) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/rememberances'
        + '?limit=' + encodeURIComponent(Number(limit))
        + '&offset=' + encodeURIComponent(Number(offset)), true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function () {
        var res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            }
            else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onSuccess(res);
            }
            else {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onError(res);
            }
        }
    };
    xhr.send(null);
}
;
function postRememberances(body, onSuccess, onError) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/rememberances', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onreadystatechange = function () {
        var res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            }
            else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onSuccess(res);
            }
            else {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onError(res);
            }
        }
    };
    xhr.send(JSON.stringify(body));
}
;
function postRememberancesByIdUpvote(id, onSuccess, onError) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/rememberances/' + encodeURIComponent(id.toString()) + '/upvote', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function () {
        var res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            }
            else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onSuccess(res);
            }
            else {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onError(res);
            }
        }
    };
    xhr.send(null);
}
;
function postRememberancesByIdDownvote(id, onSuccess, onError) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/rememberances/' + encodeURIComponent(id.toString()) + '/downvote', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function () {
        var res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            }
            else if (xhr.status >= 200 && xhr.status < 300) {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onSuccess(res);
            }
            else {
                try {
                    res = JSON.parse(xhr.responseText);
                }
                catch (e) {
                    onError(e);
                }
                if (res)
                    onError(res);
            }
        }
    };
    xhr.send(null);
}
;
function listRememberances(limit, offset, container) {
    getRememberances(limit, offset, function (rs) {
        rs.forEach(function (r) {
            var n = createRememberanceElement(r);
            container.appendChild(n);
        });
    }, function (e) {
        throw new Error(e);
    });
}
function listRememberancesOnto(containerId) {
    var container = document.getElementById(containerId);
    var limit = parseInt(container.getAttribute("data-limit"));
    var offset = parseInt(container.getAttribute("data-offset"));
    listRememberances(limit, offset, container);
}
function createRememberanceElement(r) {
    var outside = document.createElement('div');
    outside.id = "out" + r.key.toString();
    outside.className = 'rememberanceHolder';
    var n = document.createElement('div');
    n.id = "r" + r.key.toString();
    n.className = "rememberance";
    var body = document.createElement('div');
    body.textContent = r.val.body;
    body.className = "rText";
    n.appendChild(body);
    var upvote = document.createElement('a');
    upvote.id = "r" + r.key.toString() + "Upvote";
    upvote.className = "upvote";
    upvote.textContent = "+" + r.val.upvotes.toString();
    upvote.title = "Click to Upvote";
    upvote.href = "#";
    upvote.onclick = function (e) {
        postRememberancesByIdUpvote(r.key, function () {
            var upvotes = parseInt(upvote.textContent.slice(1));
            upvote.textContent = "+" + (upvotes + 1).toString();
        }, function (e) { throw Error(e); });
    };
    n.appendChild(upvote);
    var downvote = document.createElement('a');
    downvote.id = "r" + r.key.toString() + "Upvote";
    downvote.className = "downvote";
    downvote.textContent = "-" + r.val.downvotes.toString();
    downvote.title = "Click to Downvote";
    downvote.href = "#";
    downvote.onclick = function (e) {
        postRememberancesByIdDownvote(r.key, function () {
            var downvotes = parseInt(downvote.textContent.slice(1));
            downvote.textContent = "-" + (downvotes + 1).toString();
        }, function (e) { throw Error(e); });
    };
    n.appendChild(downvote);
    outside.append(n);
    return outside;
}
function submitRememberanceForm(textBoxId, containerId) {
    var textBox = document.getElementById(textBoxId);
    var body = textBox.value;
    var container = document.getElementById(containerId);
    postRememberances(body, function (id) {
        alert(id);
        textBox.value = "I remember ";
        var r = {
            key: id,
            val: {
                body: body,
                upvotes: 0,
                downvotes: 0
            }
        };
        var n = createRememberanceElement(r);
        container.prepend(n);
    }, function (e) {
        throw new Error(e);
    });
}
//# sourceMappingURL=main.js.map