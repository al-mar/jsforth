function attachInput(fieldSet) {
  var onData = [];

  function onClick() {
    if (!input.value.length) return;
    var val = input.value;
    input.value = "";
    input.focus();
    onData.forEach(function(e) { e(val) });
  }

  fieldSet.querySelector('button').onclick = onClick;
  var input = fieldSet.querySelector('textarea');
  input.onkeypress = function(ev) {
    ev = ev || window.event;
    if (ev.keyCode == 10 && ev.ctrlKey)
      onClick();
  }

  return onData;
}

function attachOutput(fieldSet) {
  var output = fieldSet.querySelector('.output');
  var timeout, buffer = "";

  return {
    write: function(text) {
      buffer += text;
      if (!timeout) timeout = setTimeout(function() {
        timeout = null;
        flush();
      }, 100);
    },
    clear: function() {
      stopTimer();
      output.innerHTML = buffer = "";
    },
    flush: flush
  }

  function stopTimer() {
    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    }
  }

  function flush() {
    stopTimer();
    if (buffer) output.innerHTML += buffer;
    buffer = "";
    output.scrollTop = output.scrollHeight;
  }
}

function htmlEscape(text) { return ("" + text).replace(/&/g, '&amp;').replace(/</g, '&lt;') }
