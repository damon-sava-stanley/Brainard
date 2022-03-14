interface KVRememberance {
  key: number,
  val: Rememberance
}

interface Rememberance {
body: string,
upvotes: number,
downvotes: number
}

function getRememberances (limit : number, offset : number, 
  onSuccess : (a?: KVRememberance[]) => void , onError : (e: any) => void) : void {
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
};
function postRememberances (body: string, 
    onSuccess : (a?: number) => void, onError : (e: any) => void) : void {
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
};
function postRememberancesByIdUpvote (id: number, 
  onSuccess : (a?: any) => void, onError : (e: any) => void) : void {
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
};
function postRememberancesByIdDownvote (id : number, 
  onSuccess : (a? : any) => void, onError : (e:any) => void) : void {
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
};

function listRememberances(limit: number, offset: number, container: HTMLElement): void {
  getRememberances(limit, offset, (rs) => {
    rs.forEach((r) => {
      let n = createRememberanceElement(r);
      container.appendChild(n);
    })
  }, (e) => {
    throw new Error(e);
  });
}

function listRememberancesOnto(containerId: string): void {
  let container = document.getElementById(containerId);
  let limit = parseInt(container.getAttribute("data-limit"));
  let offset = parseInt(container.getAttribute("data-offset"));
  listRememberances(limit, offset, container);
}

function createRememberanceElement(r: KVRememberance): HTMLElement {
  let outside = document.createElement('div') as HTMLElement;
  outside.id = "out" + r.key.toString();
  outside.className = 'rememberanceHolder';
  let n = document.createElement('div') as HTMLElement;
  n.id = "r" + r.key.toString();
  n.className = "rememberance";
  let body = document.createElement('div') as HTMLElement;
  body.textContent = r.val.body;
  body.className = "rText"
  n.appendChild(body);
  let upvote = document.createElement('a') as HTMLAnchorElement;
  upvote.id = "r" + r.key.toString() + "Upvote";
  upvote.className = "upvote";
  upvote.textContent = "+" + r.val.upvotes.toString();
  upvote.title = "Click to Upvote";
  upvote.href = "#";
  upvote.onclick = function (e) {
    postRememberancesByIdUpvote(r.key, () => {
      let upvotes = parseInt(upvote.textContent.slice(1));
      upvote.textContent = "+" + (upvotes + 1).toString();
    }, (e) => {throw Error(e);})
  }
  n.appendChild(upvote);
  let downvote = document.createElement('a') as HTMLAnchorElement;
  downvote.id = "r" + r.key.toString() + "Upvote";
  downvote.className = "downvote";
  downvote.textContent = "-" + r.val.downvotes.toString();
  downvote.title = "Click to Downvote";
  downvote.href = "#";
  downvote.onclick = function (e) {
    postRememberancesByIdDownvote(r.key, () => {
      let downvotes = parseInt(downvote.textContent.slice(1));
      downvote.textContent = "-" + (downvotes + 1).toString();
    }, (e) => {throw Error(e);})
  }
  n.appendChild(downvote);
  outside.append(n);
  return outside;
}


function submitRememberanceForm(textBoxId: string, containerId: string) {
  let textBox = document.getElementById(textBoxId) as HTMLInputElement;
  let body = textBox.value;
  let container = document.getElementById(containerId);
  postRememberances(body, (id) => {
    textBox.value = "I remember ";
    let r : KVRememberance = {
      key: id,
      val: {
        body: body,
        upvotes: 0,
        downvotes: 0
      }
    }
    let n = createRememberanceElement(r);
    container.prepend(n);
  }, (e) => {
    throw new Error(e);
  });

}