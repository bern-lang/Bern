/*
 * Per-example "Show written form" toggle for the Bern docs.
 *
 * Every Bern operator has a written-word synonym (e.g. >= is-greater-or-equal,
 * \ -> lambda, -> such-that). This script adds a small toggle above each code
 * example that rewrites the symbolic form into the written form on the fly, so
 * readers can see both. It works on all examples automatically.
 */
(function () {
  "use strict";

  // Multi-character operators, longest first so prefixes don't win.
  var MULTI = [
    ["</>", "difference"],
    [">=", "is-greater-or-equal"],
    ["<=", "is-less-or-equal"],
    ["==", "equals"],
    ["!=", "not-equals"],
    ["&&", "and"],
    ["||", "or"],
    ["<>", "concat"],
    ["<|", "union"],
    ["|>", "intersect"],
    [":>", "length"],
    ["::", "typeof"],
    [")|", "and-do"]
  ];
  // `->` is handled separately: it reads `such-that` after a lambda (\), but
  // `returns` after a function definition.

  // Convert symbolic Bern source to its written-word form.
  function toWritten(src) {
    // Loops: `loop x : coll` reads `loop x in coll` (also the `for` alias).
    src = src.split("\n").map(function (line) {
      return /^\s*(loop|for)\b/.test(line) ? line.replace(/\s:\s/, " in ") : line;
    }).join("\n");

    var isCase = /\bcase\b/.test(src) && /\bis\b/.test(src);
    var out = "";
    var i = 0;
    var n = src.length;
    var lastSig = "\n"; // last non-space char emitted (for minus vs negate)
    var lambdaPending = 0; // open lambdas awaiting their arrow (for -> meaning)

    function emitOp(word, nextChar) {
      var prev = out.length ? out[out.length - 1] : "\n";
      if (prev !== " " && prev !== "\n" && prev !== "(" && prev !== "[") out += " ";
      out += word;
      if (nextChar && nextChar !== " " && nextChar !== "\n" &&
          nextChar !== ")" && nextChar !== "]" && nextChar !== "," && nextChar !== ";") {
        out += " ";
      }
      lastSig = "w";
    }

    while (i < n) {
      var c = src[i];
      var two = src.substr(i, 2);

      // Comments are copied verbatim.
      if (two === "--" || two === "//") {
        var e = src.indexOf("\n", i);
        if (e === -1) e = n;
        out += src.slice(i, e);
        i = e;
        continue;
      }
      if (two === "/*") {
        var be = src.indexOf("*/", i + 2);
        be = be === -1 ? n : be + 2;
        out += src.slice(i, be);
        i = be;
        continue;
      }
      // Strings are copied verbatim.
      if (c === '"' || c === "'") {
        var j = i + 1;
        while (j < n) {
          if (src[j] === "\\") { j += 2; continue; }
          if (src[j] === c) { j++; break; }
          j++;
        }
        out += src.slice(i, j);
        i = j;
        lastSig = c;
        continue;
      }
      if (c === "\n") { out += "\n"; i += 1; lastSig = "\n"; continue; }
      if (c === " " || c === "\t") { out += c; i += 1; continue; }

      // ':=' (global assignment) stays as-is.
      if (two === ":=") { out += ":="; i += 2; lastSig = "="; continue; }

      // '<-' (list-comprehension generator) is not an operator; keep it literal
      // so it isn't split into `< -`.
      if (two === "<-") { out += "<-"; i += 2; lastSig = "<"; continue; }

      // The arrow: `such-that` inside a lambda, `returns` after a definition.
      if (two === "->") {
        var arrowWord = lambdaPending > 0 ? "such-that" : "returns";
        if (lambdaPending > 0) lambdaPending--;
        emitOp(arrowWord, src[i + 2]);
        i += 2;
        continue;
      }

      // Multi-character operators.
      var matched = false;
      for (var k = 0; k < MULTI.length; k++) {
        var sym = MULTI[k][0];
        if (src.substr(i, sym.length) === sym) {
          emitOp(MULTI[k][1], src[i + sym.length]);
          i += sym.length;
          matched = true;
          break;
        }
      }
      if (matched) continue;

      // Single-character operators / keywords.
      if (c === "\\") { emitOp("lambda", src[i + 1]); lambdaPending += 1; i += 1; continue; }
      if (c === "/") { emitOp("divided-by", src[i + 1]); i += 1; continue; }
      if (c === "=") { emitOp(isCase ? "returns" : "be", src[i + 1]); i += 1; continue; }
      if (c === "-") {
        // Unary negate when nothing meaningful precedes, or an opener / operator
        // (symbolic or a just-emitted word operator) precedes; else subtraction.
        var neg = lastSig === "\n" || lastSig === "" || lastSig === "w" ||
          "([{,".indexOf(lastSig) !== -1 || "+-*/%<>=&|!".indexOf(lastSig) !== -1;
        emitOp(neg ? "negate" : "minus", src[i + 1]);
        i += 1;
        continue;
      }
      if (c === "+") { emitOp("plus", src[i + 1]); i += 1; continue; }
      if (c === "*") { emitOp("times", src[i + 1]); i += 1; continue; }
      if (c === "%") { emitOp("modulo", src[i + 1]); i += 1; continue; }
      if (c === ">") { emitOp("is-greater", src[i + 1]); i += 1; continue; }
      if (c === "<") { emitOp("is-less", src[i + 1]); i += 1; continue; }
      if (c === "!") { emitOp("not", src[i + 1]); i += 1; continue; }

      // Everything else (identifiers, numbers, punctuation) copied as-is.
      out += c;
      if (c.trim()) lastSig = c;
      i += 1;
    }
    return out;
  }

  // --- Syntax highlighting for the written form ---------------------------

  var KEYWORDS = {
    def: 1, do: 1, end: 1, "if": 1, then: 1, "else": 1, loop: 1, "for": 1, "in": 1,
    "case": 1, is: 1, when: 1, "return": 1, returns: 1, "such-that": 1,
    lambda: 1, be: 1, "import": 1, adt: 1, iterative: 1, "true": 1, "false": 1,
    input: 1, foreign: 1, print: 1
  };
  var WORDOPS = {
    plus: 1, minus: 1, negate: 1, times: 1, "divided-by": 1, modulo: 1,
    and: 1, or: 1, not: 1, concat: 1, union: 1, intersect: 1, difference: 1,
    "is-greater": 1, "is-less": 1, "is-greater-or-equal": 1,
    "is-less-or-equal": 1, equals: 1, "not-equals": 1, "typeof": 1,
    "length": 1, "and-do": 1
  };

  function esc(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  // Highlight written-form Bern using the same span classes as the symbolic
  // examples, so both forms look identical.
  function highlight(src) {
    var out = "";
    var re = /(\/\*[\s\S]*?\*\/|--[^\n]*|\/\/[^\n]*)|("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')|(\d+\.\d+|\d+)|([A-Za-z_][A-Za-z0-9_-]*)|(>=|<=|==|!=|&&|\|\||<\/>|<>|<\||\|>|::|:>|\)\||:=|->|[-+*/%<>=!\\])/g;
    var last = 0, m;
    while ((m = re.exec(src))) {
      out += esc(src.slice(last, m.index));
      if (m[1]) {
        out += '<span class="comment">' + esc(m[1]) + "</span>";
      } else if (m[2]) {
        out += '<span class="string">' + esc(m[2]) + "</span>";
      } else if (m[3]) {
        out += '<span class="number">' + esc(m[3]) + "</span>";
      } else if (m[4]) {
        var w = m[4], cls = null;
        if (KEYWORDS[w]) cls = "keyword";
        else if (WORDOPS[w]) cls = "operator";
        else if (/^[A-Z]/.test(w)) cls = "type";
        else if (/^\s*\(/.test(src.slice(m.index + w.length))) cls = "function";
        out += cls ? '<span class="' + cls + '">' + esc(w) + "</span>" : esc(w);
      } else {
        // Symbolic operators (+, -, ->, )|, etc.) so the symbolic form is
        // coloured just like the written form.
        out += '<span class="operator">' + esc(m[5]) + "</span>";
      }
      last = re.lastIndex;
    }
    out += esc(src.slice(last));
    return out;
  }

  function injectCSS() {
    var css =
      ".form-toggle-wrap{position:relative;}" +
      ".form-toggle{position:absolute;top:6px;right:6px;z-index:2;padding:2px 10px;" +
      "font-size:12px;font-family:inherit;line-height:1.4;cursor:pointer;" +
      "color:#5e35b1;background:#ede7f6;border:1px solid #d1c4e9;border-radius:6px;}" +
      ".form-toggle:hover{background:#d1c4e9;}" +
      // Highlight colors (match style.css) so the written form is coloured on
      // every page, including ones whose plain examples carry no spans.
      ".keyword{color:#d73a49;font-weight:500;}.function{color:#6f42c1;}" +
      ".string{color:#22863a;}.number{color:#005cc5;}" +
      ".comment{color:#6a737d;font-style:italic;}.operator{color:#d73a49;}" +
      ".type{color:#005cc5;font-weight:500;}";
    var s = document.createElement("style");
    s.textContent = css;
    document.head.appendChild(s);
  }

  function collect() {
    var items = [];
    document.querySelectorAll("pre.simple-code, div.simple-code").forEach(function (el) {
      items.push({ block: el, code: el });
    });
    document.querySelectorAll(".code-example").forEach(function (el) {
      var code = el.querySelector("code") || el.querySelector(".code-content");
      if (code) items.push({ block: el, code: code });
    });
    return items;
  }

  // Examples already written in word form (e.g. the Word Operators section)
  // should not be re-transformed.
  var ALREADY_WRITTEN = /\b(such-that|is-greater-or-equal|is-less-or-equal|is-greater|is-less|not-equals|divided-by|typeof|length|and-do)\b/;

  function setup(item) {
    var code = item.code;
    var symbols = code.textContent;
    if (ALREADY_WRITTEN.test(symbols)) return;
    var written = toWritten(symbols);
    if (written === symbols) return; // nothing to toggle (no operators)

    // Colour the symbolic (normal) form too, unless the example already carries
    // manual highlighting spans, so it matches the written form's highlighting.
    var original;
    if (code.querySelector("span")) {
      original = code.innerHTML;
    } else {
      original = highlight(symbols);
      code.innerHTML = original;
    }

    var showingWords = false;
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "form-toggle";
    btn.textContent = "Show written form";
    btn.addEventListener("click", function () {
      showingWords = !showingWords;
      if (showingWords) {
        code.innerHTML = highlight(written);
        btn.textContent = "Show symbols";
      } else {
        code.innerHTML = original;
        btn.textContent = "Show written form";
      }
    });
    // Place the button beside the code itself (top-right corner of the block)
    // rather than above it, by wrapping the block in a relative container.
    var block = item.block;
    var wrap = document.createElement("div");
    wrap.className = "form-toggle-wrap";
    block.parentNode.insertBefore(wrap, block);
    wrap.appendChild(block);
    wrap.appendChild(btn);
  }

  document.addEventListener("DOMContentLoaded", function () {
    injectCSS();
    collect().forEach(setup);
  });
})();
