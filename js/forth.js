/// <reference path="io.js" />
function forth(parameters) {
  var emptyFn = (function() {});

  var returnCodes = {
    waitForInput:    1,
    waitForCallback: 2
  }
  var code2Flags = -2, name2Link = 2, name2Code = 3, name2Data = 4;
  var fImmediate = 1, fSmudge = 2, fVoc = 4;
  var stepsBeforeTimeout = 100;

  parameters = parameters || {};
  var output = parameters.output || {};
  var inputEvents = parameters.input || [];
  var wlSize = parameters.wordListSize || 100000;
  var sSize = parameters.stackSize || 10000;
  var rSize = parameters.returnStackSize || 10000;
  
  var writeOutput = output.write || emptyFn
  var clearOutput = output.clear || emptyFn
  var flushOutput = output.flush || emptyFn;

  var stream = "", waitingForInput; // State
  var m, s, r, here; // memory, stack, returnStack, here;

  inputEvents.push(function(text) { // Subscribe for new data
    stream += text + "\n";
    checkInputStatus();
  });

  function checkInputStatus() {
    if (!waitingForInput) return;
    waitingForInput = false;
    run();
  }

  function throwError(msg) {
    writeOutput("\n<span class='error'>Error: " + htmlEscape(msg.message) + "</span>");
    resetStacks();
    stream = "";
    waitingForInput = true;
    setEntryPoint();
  }

  /** Runs a Forth program */
  function run() {
    try {
      for(var steps = stepsBeforeTimeout; steps--;) {
        var addr = r.pop(), value = m[addr];
        if (typeof(value) === 'number') { // Goto addr
          r.push(addr + 1);
          r.push(value);
        } else if (typeof(value) === 'function') { // Run a function
          var result = value();
          if (result > 0) {
            waitingForInput = result == returnCodes.waitForInput;
            if (waitingForInput) r.push(addr);
            return;
          }
        } else { throw Error("Invalid instruction") }
      }
      setTimeout(run, 1);
    } catch(e) { throwError(e) }
  }

  /** Reads 'n' symbols from the stream and skips next 'skip' symbols. */
  function readStream(n, skip) {
    var res = stream.substr(0, n);
    stream = stream.substr(n + skip);
    return res;
  }

  /** Reads a word from the stream till the stopPattern */
  function getWord(stopPattern) {
    var m = new RegExp(stopPattern || "(^\\s+)|(\\s)").exec(stream);
    return m === null ? null : readStream(m.index, m[0].length);
  }

  /**
   * Executes 'fn' for each word in a vocabulary.
   * @param {number} context A value of 'CONTEXT' variable.
   * @param {function} fn A function that will be executed for each word in a vocabulary.
  */
  function forEachWord(context, fn) {
    for(var l = Math.max(0, context), i = m[context]; i >= l || !i; i = m[i + name2Link] )
      if (!hasFlag(i + name2Code, fSmudge) && fn(m[i].toLowerCase(), i)) return i;
  }

  /** Finds a word in a current vocabulary. */
  function findWord(word) {
    word = word.toLowerCase();
    for(var context = m[indContext]; context >= 0; context = m[context - 1]) {
      var res = forEachWord(context, function(w) { return w === word });
      if (typeof(res) === typeof(0)) return res + name2Code;
    }
  }

  /** Resets stacks */
  function resetStacks() { s = newStack("Stack", s0, s9, m[s0]); r = newStack("ReturnStack", r0, r9, m[r0]) }
  
  /** Creates a new stack in the memory */
  function newStack(name, bottom, limit, pointer) {
    function check(offset) {
      if (offset < 0 && pointer - offset > m[bottom]) throw Error(name + " underflow");
      else if (offset > 0 && pointer - offset < m[limit]) throw Error(name + " overflow");
    }

    return {
      check: check,
      push:  function(val) { check(1); m[--pointer] = val },
      pop:   function() { check(-1); return m[pointer++]; },
      popN:  function(val) { check(-val); return m.slice(pointer, pointer += val).reverse(); },
      pick:  function(index) { check(-index-1); return m[pointer + index] },
      roll:  function(index) {
        check(-index-1);
        var v = m[pointer + index];
        for(var i = pointer + index; i > pointer; i--) m[i] = m[i-1];
        m[pointer] = v;
      },
      pointer: function(addr) { return arguments.length ? (pointer = addr) : pointer }
    }
  }

  /** Checks if a vocabulary entry has a flag */
  function hasFlag(codeAddr, flag) { return m[codeAddr + code2Flags] & flag }
  
  /** Sets the entry point for the interpreter. */
  function setEntryPoint() { r.push(indInterpret) }

  var indForth = 5, indContext = 10, indState, indLit, indInterpret, r0, r9, s0, s9;
  function initWordList() {
    var latest = 0;
    /** Adds a new variable */
    function addVar(name) {
      var addr = m.length + name2Data;
      addCmd(name, function() { s.push(addr); });
      m.push(0);
      return latest + name2Data;
    }
    /** Adds a new command */
    function addCmd(name, fn) {
        m.push(name, 0, latest, fn);
        latest = m.length - name2Data;
        return latest + name2Code;
    }

    // Word entry structure [ "wordName", flags, prevWordAddress, code1, code2, ... ]
    
    m = [ "FORTH", fVoc, -1, function() { m[indContext] = indForth }, -1, 0 ];
    addVar("CONTEXT");
    m[addVar("CURRENT")] = indForth;
    indState = addVar("STATE");
    r0 = addVar("R0"); r9 = addVar("R9"); s0 = addVar("S0"); s9 = addVar("S9");
    m[r9] = wlSize;
    m[r0] = m[s9] = wlSize + rSize;
    m[s0] = wlSize + rSize + sSize;
    addCmd("HERE",   function() { s.push(here) });
    addCmd("ALLOT",  function() { here += s.pop() });
    addCmd("=",      function() { var v = s.pop(); s.push(s.pop() == v) });
    addCmd("<",      function() { var v = s.pop(); s.push(s.pop() < v) });
    addCmd(">",      function() { var v = s.pop(); s.push(s.pop() > v) });
    addCmd("+",      function() { var v = s.pop(); s.push(s.pop() + v) });
    addCmd("-",      function() { var v = s.pop(); s.push(s.pop() - v) });
    addCmd("*",      function() { var v = s.pop(); s.push(s.pop() * v) });
    addCmd("/",      function() { var v = s.pop(); s.push(s.pop() / v) });
    addCmd("MOD",    function() { var v = s.pop(); s.push(s.pop() % v) });
    addCmd("OR",     function() { var v = s.pop(); s.push(s.pop() | v) });
    addCmd("AND",    function() { var v = s.pop(); s.push(s.pop() & v) });
    addCmd("XOR",    function() { var v = s.pop(); s.push(s.pop() ^ v) });
    addCmd("NOT",    function() { s.push(!s.pop()) });
    addCmd("INVERT", function(){ s.push(~s.pop()) });
    addCmd("PICK",   function() { s.push(s.pick(Math.max(s.pop(), 0))) });
    addCmd("ROLL",   function() { s.roll(Math.max(s.pop(), 0)) });
    addCmd("DUP",    function() { s.push(s.pick(0)) });
    addCmd("SWAP",   function() { var v1 = s.pop(), v2 = s.pop(); s.push(v1); s.push(v2) });
    addCmd("OVER",   function() { s.push(s.pick(1)) });
    addCmd("ROT",    function() { s.roll(2) });
    addCmd("DROP",   function() { s.pop() });
    addCmd(">R",     function() { var v = r.pop(); r.push(s.pop()); r.push(v) });
    addCmd("R>",     function() { var v = r.pop(); s.push(r.pop()); r.push(v) });
    addCmd("R@",     function() { s.push(r.pick(1)) });
    addCmd("SP@",    function() { s.push(s.pointer()) });
    addCmd("SP!",    function() { s.pointer(s.pop()) });
    addCmd("RP@",    function() { s.push(r.pointer()) });
    addCmd("RP!",    function() { r.pointer(s.pop()) });
    addCmd("@",      function() { s.push(m[s.pop()]) });
    addCmd("!",      function() { m[s.pop()] = s.pop() });
    addCmd(",",      function() { m[here++] = s.pop() });
    addCmd("TYPE",   function() { writeOutput(htmlEscape(s.pop())) });
    addCmd("HTML",   function() { writeOutput(s.pop()) });
    addCmd("PAGE",   clearOutput );
    addCmd("SFLUSH", flushOutput );
    addCmd(">STR",   function() { stream = s.pop() + stream; checkInputStatus() });
    addCmd("STR>",   function() { stream += s.pop(); checkInputStatus() });
    addCmd("KEY",    function() {
        if (!stream) return returnCodes.waitForInput;
        s.push(readStream(1, 0));
      });
    addCmd("WORD", function() {
        var word = getWord(s.pick(0));
        if (word === null) return returnCodes.waitForInput;
        m[s.pointer()] = word;
      });
    addCmd("EVAL",   function() { s.push(eval(s.pop())) });
    addCmd(">ARRAY", function() { s.push(s.popN(s.pop())) });
    addCmd("APPLY",  function() { var args = s.pop(); var obj = s.pop(); s.push(s.pop().apply(obj, args)) });
    addCmd("PROP>",  function() { s.push(s.pop()[s.pop()]) });
    addCmd(">PROP",  function() { s.pop()[s.pop()] = s.pop() });
    addCmd("EXIT",   function() { r.pop() });
    indLit = addCmd("LIT",    function() {
         var ret = r.pop();
         s.push(m[ret]);
         r.push(ret + 1)
      });
    addCmd(".ABORT", function() { throw Error(s.pop()) });
    addCmd("?BRANCH", function() { var addr = r.pop(); r.push(s.pop() ? addr + 1 : m[addr]) });
    addCmd("FIND", function() {
        var word = s.pop(), wordIndex = findWord(word);
        if (wordIndex === undefined) { s.push(word); s.push(0) }
        else { s.push(wordIndex); s.push(hasFlag(wordIndex, fImmediate) * 2 - 1) }
      });
    indInterpret = addCmd("INTERPRET", function() {
        for (var word = getWord(); !word; word = getWord()) // Skip empty words
          if (word === null) return returnCodes.waitForInput;

        setEntryPoint(); // Infinite loop
        var addr = findWord(word), state = m[indState];
        if (addr === undefined) {
          if (state) { m[here++] = indLit; m[here++] = eval(word) }
          else s.push(eval(word));
        } else {
          if (state && !hasFlag(addr, fImmediate)) m[here++] = addr;
          else r.push(addr);
        }
      });
    addCmd("WORDS", function() {
        forEachWord(m[indContext], function(w, i) {
          writeOutput((hasFlag(i + name2Code, fVoc) ? "<span class='branch'>" + w + "</span>" : w) + " ");
        });
      });
    addCmd("SLEEP", function() { setTimeout(run, s.pop()); return returnCodes.waitForCallback });
    addCmd("HTTP", function() {
      var xhr = new (window.XDomainRequest || window.XMLHttpRequest);
      var method = s.pop(), address = s.pop(), prm = s.pop(), header = (prm && prm.headers) || {}, data = prm && prm.data;
      xhr.open(method, address, true);
      for(var name in header) {
        if (!header.hasOwnProperty(name)) continue;
        xhr.setRequestHeader(name, header[name]);
      }
      xhr.onreadystatechange = function() {
        if (xhr.readyState != 4) return;
        s.push(xhr.getAllResponseHeaders());
        s.push(xhr.responseText);
        s.push(xhr.statusText);
        s.push(xhr.status);
        run()
      };
      xhr.send(data);
      return returnCodes.waitForCallback;
    });
    m[indContext] = indForth;
    m[indForth] = latest;
    here = m.length;
  }

  /** Initializes and runs a Forth system */
  function init() {
    initWordList();
    resetStacks();

    stream = parameters.initialStream ? parameters.initialStream + "\n" : "";
    setEntryPoint();
    run();
  }
  init();
}
