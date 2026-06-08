const vscode = require("vscode");
const fs = require("fs");
const path = require("path");

const KEYWORDS = [
  "def",
  "return",
  "returns",
  "such-that",
  "retorna",
  "tal-que",
  "lambda",
  "be",
  "recebe",
  "when",
  "if",
  "then",
  "else",
  "loop",
  "for",
  "in",
  "do",
  "end",
  "case",
  "is",
  "import",
  "as",
  "adt",
  "iterative",
  "fmap",
  "foreign",
  "input",
  "keys",
  "read_file",
  "write_file",
  "get_host_machine",
  "get_current_dir",
  "true",
  "false",
];

// Written-word synonyms for the symbolic operators.
const WORD_OPERATORS = [
  "plus",
  "minus",
  "negate",
  "times",
  "divided-by",
  "modulo",
  "and",
  "or",
  "not",
  "concat",
  "union",
  "intersect",
  "difference",
  "is-greater",
  "is-less",
  "is-greater-or-equal",
  "is-less-or-equal",
  "equals",
  "not-equals",
  "typeof",
  "length",
  "and-do",
  // Brazilian Portuguese forms (no accents).
  "mais",
  "menos",
  "negativo",
  "vezes",
  "dividido-por",
  "resto",
  "e",
  "ou",
  "nao",
  "concatenar",
  "uniao",
  "intersecao",
  "diferenca",
  "maior-que",
  "menor-que",
  "maior-ou-igual",
  "menor-ou-igual",
  "igual",
  "diferente",
  "tipo-de",
  "tamanho",
  "e-faca",
];

// File-level pragmas, written as `{--! name !--}` magic comments.
const PRAGMAS = [
  "impure-lists",
  "impure-sets",
  "strict-types",
  "strict-arithmetic",
  "immutable",
  "no-eval",
  "show-types",
  "safe-index",
  "no-undefined",
  "start-on-one",
  "main",
  "strict-imports",
  "no-curry",
  "abort-on-error",
  "partial",
  "no-written-operators",
];

// ---------------------------------------------------------------------------
// Hover documentation. Each map explains exactly what a token does, so hovering
// shows a real description instead of a bare "Bern keyword".
// ---------------------------------------------------------------------------

const KEYWORD_DOCS = {
  def: "Defines a function. Repeating `def name(...)` with different argument patterns creates clauses that are tried top-to-bottom. Body is `-> expr` or `do ... end` (with `return`).",
  return: "Yields a value out of a `do ... end` function block.",
  returns: "Written form of `->`. Separates a parameter list from the body expression of a function or lambda.",
  "such-that": "Written form of `->`, typically after `lambda`: `lambda x such-that x + 1`.",
  retorna: "Forma escrita de `->` (igual a `returns`). Separa os parâmetros do corpo da função.",
  "tal-que": "Forma escrita de `->` (igual a `such-that`), usada após `lambda`: `lambda x tal-que x + 1`.",
  lambda: "Anonymous function. `lambda x such-that x + 1` is the same as `\\x -> x + 1`.",
  be: "Written form of `=` (assignment): `total be 10`.",
  recebe: "Forma escrita de `=` (atribuição): `total recebe 10`.",
  when: "Guard. A `def` clause, `case` branch, or lambda matches only when its patterns match **and** the guard expression is true.",
  if: "Conditional: `if cond then ... else ... end`. Chain with `else if`. Each branch is an expression that yields a value.",
  then: "Marks the start of the true-branch of an `if`.",
  else: "The fallback branch of an `if`; use `else if` to chain more conditions.",
  loop: "Loop, three forms: `loop N do` (repeat N times), `loop cond do` (while true), and `loop x : xs do` (iterate, with an optional index: `loop x, i : xs`).",
  for: "Legacy alias of `loop` with the same three forms. `loop` is the preferred spelling.",
  in: "Written form of `:` in a loop header: `loop x in xs do ...`.",
  do: "Opens a block body (for loops or block-bodied functions), closed by `end`.",
  end: "Closes a block opened by `if`, `do`, `loop`/`for`, or `case`.",
  case: "Pattern-match expression: `case value is Pattern = result | _ = result end`. Branches may carry `when` guards.",
  is: "In a `case`, separates the value being matched from its branches. Also the word synonym of `==` (equality) in expressions.",
  import: "Loads a module: `import core`. Add `as alias` to namespace it: `import math as m`, then `m:sqrt(2)`.",
  as: "Gives an imported module a namespace alias: `import x as y`.",
  adt: "Declares an Algebraic Data Type: `adt Maybe = Just Int | None`. Add `iterative` for recursive types.",
  iterative: "Modifier on `adt` for recursive (self-referencing) types: `adt iterative List = Cons Int List | Nil`.",
  fmap: "`fmap(collection, fn)` - applies `fn` to each element of a list/set, or to the values inside an ADT, preserving structure.",
  foreign: 'Declares a C function through the FFI: `foreign name("lib.so", "int") -> "void"`.',
  input: 'Reads a line from the user: `name = input("prompt: ")`.',
  keys: "`keys(obj)` - returns an object's keys as a list, in insertion order.",
  read_file: "`read_file(path)` - reads a file's contents as a string.",
  write_file: "`write_file(path, content)` - writes a string to a file.",
  get_host_machine: "`get_host_machine()` - returns the host OS/system string (handy for picking FFI library paths).",
  get_current_dir: "`get_current_dir()` - returns the current working directory.",
  true: "Boolean literal `true`.",
  false: "Boolean literal `false`.",
};

// Operator word forms, English and Brazilian Portuguese, sharing one entry.
const OPERATOR_GROUPS = [
  { words: ["plus", "mais"], sym: "+", doc: "Addition. With strings/chars it concatenates; a number mixed with text is coerced to text (unless `strict-types`)." },
  { words: ["minus", "menos"], sym: "-", doc: "Subtraction." },
  { words: ["times", "vezes"], sym: "*", doc: "Multiplication." },
  { words: ["divided-by", "dividido-por"], sym: "/", doc: "Division. By zero gives `NaN`, or an error under `strict-arithmetic`." },
  { words: ["modulo", "resto"], sym: "%", doc: "Modulo: the remainder of integer division." },
  { words: ["negate", "negativo"], sym: "-", doc: "Unary numeric negation (prefix)." },
  { words: ["not", "nao"], sym: "!", doc: "Logical NOT (prefix)." },
  { words: ["and", "e"], sym: "&&", doc: "Logical AND. Short-circuits: the right side runs only if the left is true." },
  { words: ["or", "ou"], sym: "||", doc: "Logical OR. Short-circuits: the right side runs only if the left is false." },
  { words: ["equals", "igual"], sym: "==", doc: "Equality test. (`is` is also a synonym for `==`.)" },
  { words: ["not-equals", "diferente"], sym: "!=", doc: "Inequality test." },
  { words: ["is-greater", "maior-que"], sym: ">", doc: "Greater-than comparison." },
  { words: ["is-less", "menor-que"], sym: "<", doc: "Less-than comparison." },
  { words: ["is-greater-or-equal", "maior-ou-igual"], sym: ">=", doc: "Greater-than-or-equal comparison." },
  { words: ["is-less-or-equal", "menor-ou-igual"], sym: "<=", doc: "Less-than-or-equal comparison." },
  { words: ["concat", "concatenar"], sym: "<>", doc: "Concatenates two lists (or strings)." },
  { words: ["union", "uniao"], sym: "<|", doc: "Set union." },
  { words: ["intersect", "intersecao"], sym: "|>", doc: "Set intersection." },
  { words: ["difference", "diferenca"], sym: "</>", doc: "Set difference: elements in the left operand not in the right." },
  { words: ["typeof", "tipo-de"], sym: "::", doc: "Prefix: the type name of a value, as a string." },
  { words: ["length", "tamanho"], sym: ":>", doc: "Prefix: the length/size of a list, set, string, or object." },
  { words: ["and-do", "e-faca"], sym: ")|", doc: "Pipe: threads the left value in as the **first** argument of the right call. `x )| f(a)` is `f(x, a)`." },
];

const OPERATOR_DOCS = {};
for (const group of OPERATOR_GROUPS) {
  for (const word of group.words) {
    OPERATOR_DOCS[word] = { sym: group.sym, doc: group.doc };
  }
}

const PRAGMA_DOCS = {
  "impure-lists": "Lists may hold mixed element types (otherwise a list is homogeneous).",
  "impure-sets": "Set operations keep duplicates instead of deduplicating.",
  "strict-types": "Forbid implicit string/number coercion; such mixes become errors.",
  "strict-arithmetic": "Division/modulo by zero is an error instead of `NaN`.",
  immutable: "Reassigning an existing variable is an error; each name is bound once.",
  "no-eval": "Bare top-level expressions no longer auto-print (`print()` still works).",
  "show-types": "Auto-printed values are shown with their type appended.",
  "safe-index": "Out-of-bounds indexing returns `undefined` instead of crashing.",
  "no-undefined": "Reading an unbound variable is an error.",
  "start-on-one": "Indexing starts at 1 instead of 0.",
  main: "After the file loads, automatically call and print `main()`.",
  "strict-imports": "Reserved: intended to require qualifying imported names with their module namespace.",
  "no-curry": "Disable automatic currying of partially-applied functions.",
  "abort-on-error": "Runtime errors crash execution instead of producing inspectable error values.",
  partial: "Allow non-exhaustive functions (skip the exhaustiveness check).",
  "no-written-operators": "Switch off the written-word operators, freeing names like `length`, `plus`, or `mais` for use as variables.",
};

const BUILTIN_DOCS = {
  map: "`map(xs, fn)` - apply `fn` to every element, returning a new list.",
  filter: "`filter(xs, pred)` - keep only elements for which `pred` is true.",
  foldl: "`foldl(fn, acc, xs)` - left fold: combine elements left-to-right into a single value.",
  foldr: "`foldr(fn, acc, xs)` - right fold: combine elements right-to-left.",
  sum: "`sum(xs)` - add up the numbers in a list.",
  product: "`product(xs)` - multiply the numbers in a list.",
  reverse: "`reverse(xs)` - reverse a list or set.",
  take: "`take(n, xs)` - the first `n` elements.",
  drop: "`drop(n, xs)` - all but the first `n` elements.",
  head: "`head(xs)` - the first element.",
  tail: "`tail(xs)` - every element except the first.",
  isEmpty: "`isEmpty(xs)` - true if the collection has no elements.",
  any: "`any(xs, pred)` - true if `pred` holds for at least one element.",
  all: "`all(xs, pred)` - true if `pred` holds for every element.",
  find: "`find(xs, pred)` - the first element matching `pred`.",
  zip: "`zip(xs, ys)` - pair up elements of two lists.",
  zipWith: "`zipWith(fn, xs, ys)` - combine two lists element-wise with `fn`.",
  range: "`range(a, b)` - the list of integers from `a` to `b`.",
  min: "`min(a, b)` - the smaller of two values.",
  max: "`max(a, b)` - the larger of two values.",
  print: "`print(value)` - print a value explicitly (bare expressions also auto-print).",
  typeOf: "`typeOf(value)` - the value's type name as a string (same as the `::` operator).",
  length: "`length(value)` - the length/size of a list, set, string, or object (same as the `:>` operator).",
  to_int: "`to_int(value)` - parse a string/char into an integer.",
  index_of: "`index_of(xs, item)` - the position of `item`, or -1 if absent.",
  keys: "`keys(obj)` - an object's keys as a list, in insertion order.",
  json_parse: "`json_parse(text)` - parse a JSON string into a Bern value.",
  json_stringify: "`json_stringify(value)` - serialize a Bern value to JSON text.",
  is_null: "`is_null(value)` - true if the value is `undefined`/null.",
  error: '`error(message)` - build an error value: `error("boom")`.',
  is_error: "`is_error(value)` - true if the value is an error (from errors-as-values).",
  fmap: "`fmap(collection, fn)` - map `fn` over a list/set or the values inside an ADT.",
  compose: "`compose(f, g)` - function composition: `compose(f, g)(x)` is `f(g(x))`.",
  id: "`id(x)` - the identity function; returns its argument unchanged.",
  const: "`const(x)` - returns a function that always yields `x`.",
  flip: "`flip(f)` - swaps the first two arguments of `f`.",
  append: "`append(xs, x)` - add `x` to the end of a list.",
  int_to_double: "`int_to_double(n)` - convert an integer to a double.",
};

const BUILTIN_FUNCTIONS = [
  "map",
  "filter",
  "foldl",
  "foldr",
  "sum",
  "product",
  "reverse",
  "take",
  "drop",
  "head",
  "tail",
  "isEmpty",
  "any",
  "all",
  "find",
  "zip",
  "zipWith",
  "range",
  "min",
  "max",
  "print",
  "typeOf",
  "length",
  "to_int",
  "index_of",
  "keys",
  "json_parse",
  "json_stringify",
  "is_null",
  "error",
  "is_error",
  "fmap",
  "compose",
  "id",
  "const",
  "flip",
  "append",
  "int_to_double",
];

const SNIPPETS = [
  {
    label: "import ... as ...",
    insertText: "import ${1:module/path} as ${2:alias}",
    detail: "Import alias",
  },
  {
    label: "loop ... do (repeat)",
    insertText: "loop ${1:3} do\n\t$0\nend",
    detail: "Repeat loop",
  },
  {
    label: "loop ... : ... do (loop-in)",
    insertText: "loop ${1:item} : ${2:collection} do\n\t$0\nend",
    detail: "Loop-in iteration",
  },
  {
    label: "case-is",
    insertText: "case ${1:value} is ${2:Pattern} = ${3:expr} | _ = ${4:expr} end",
    detail: "Case expression",
  },
  {
    label: "adt iterative",
    insertText: "adt iterative ${1:Type} = ${2:Ctor} ${3:FieldType}",
    detail: "Iterative ADT",
  },
  {
    label: "def ... when (guard)",
    insertText: "def ${1:name}(${2:n}) when ${3:cond} -> ${4:expr}",
    detail: "Function clause with a when guard",
  },
  {
    label: "lambda such-that",
    insertText: "lambda ${1:x} such-that ${2:expr}",
    detail: "Lambda (written form)",
  },
  {
    label: "lambda tal-que (pt-br)",
    insertText: "lambda ${1:x} tal-que ${2:expr}",
    detail: "Lambda (forma escrita em portugues)",
  },
  {
    label: "def ... retorna (pt-br)",
    insertText: "def ${1:nome}(${2:n}) retorna ${3:expr}",
    detail: "Funcao com 'retorna' (-> em portugues)",
  },
  {
    label: "pragma {--! ... !--}",
    insertText:
      "{--! ${1|impure-lists,impure-sets,strict-types,strict-arithmetic,immutable,no-eval,show-types,safe-index,no-undefined,start-on-one,main,strict-imports,no-curry,abort-on-error,partial,no-written-operators|} !--}",
    detail: "File-level pragma directive",
  },
  {
    label: "pipe )|",
    insertText: "${1:value} )| ${2:func}",
    detail: "Pipe: pass value as the first argument",
  },
  {
    label: "json_parse",
    insertText: "json_parse(${1:text})",
    detail: "Parse JSON text into a Bern value",
  },
  {
    label: "json_stringify",
    insertText: "json_stringify(${1:value})",
    detail: "Serialize a Bern value to JSON",
  },
];

const moduleIndexCache = new Map();

function activate(context) {
  const diagnostics = vscode.languages.createDiagnosticCollection("bern");
  context.subscriptions.push(diagnostics);

  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      "bern",
      {
        provideCompletionItems(document, position) {
          return provideCompletionItems(document, position);
        },
      },
      ":"
    )
  );

  context.subscriptions.push(
    vscode.languages.registerHoverProvider("bern", {
      provideHover(document, position) {
        // Match hyphenated words too (e.g. `maior-que`, `such-that`).
        const range = document.getWordRangeAtPosition(
          position,
          /[A-Za-z_][A-Za-z0-9_-]*/
        );
        if (!range) {
          return undefined;
        }
        const word = document.getText(range);
        const line = document.lineAt(position.line).text;
        const hover = buildHover(word, line);
        return hover ? new vscode.Hover(hover) : undefined;
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider("bern", {
      provideDefinition(document, position) {
        return provideDefinition(document, position);
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerReferenceProvider("bern", {
      provideReferences(document, position, options) {
        return provideReferences(document, position, options);
      },
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDocumentSymbolProvider("bern", {
      provideDocumentSymbols(document) {
        return provideDocumentSymbols(document);
      },
    })
  );

  const refreshDiagnostics = (document) => {
    if (document.languageId !== "bern") {
      return;
    }
    updateDiagnostics(document, diagnostics);
  };

  if (vscode.window.activeTextEditor) {
    refreshDiagnostics(vscode.window.activeTextEditor.document);
  }

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(refreshDiagnostics)
  );
  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) =>
      refreshDiagnostics(event.document)
    )
  );
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument(refreshDiagnostics)
  );
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument((document) =>
      diagnostics.delete(document.uri)
    )
  );
}

function deactivate() {}

// Build a rich Markdown hover for a token, given the line it sits on (used to
// tell a pragma name like `main` apart from an ordinary identifier).
function buildHover(word, line) {
  const md = (title, body) => {
    const s = new vscode.MarkdownString();
    s.appendMarkdown(`**${title}**\n\n${body}`);
    return s;
  };

  const inPragma = /\{--!/.test(line) && /!--\}/.test(line);
  if (inPragma && PRAGMA_DOCS[word]) {
    return md(`Pragma - \`{--! ${word} !--}\``, PRAGMA_DOCS[word]);
  }

  if (KEYWORD_DOCS[word]) {
    return md("Keyword", KEYWORD_DOCS[word]);
  }

  if (OPERATOR_DOCS[word]) {
    const op = OPERATOR_DOCS[word];
    return md(`Operator - written form of \`${op.sym}\``, op.doc);
  }

  if (BUILTIN_DOCS[word]) {
    return md("Core function", BUILTIN_DOCS[word]);
  }

  if (PRAGMA_DOCS[word]) {
    return md(`Pragma - \`{--! ${word} !--}\``, PRAGMA_DOCS[word]);
  }

  // Fallbacks so anything still in the lists gets at least a label.
  if (KEYWORDS.includes(word)) {
    return md("Keyword", `\`${word}\``);
  }
  if (WORD_OPERATORS.includes(word)) {
    return md("Operator (written form)", `\`${word}\``);
  }
  return undefined;
}

function provideCompletionItems(document, position) {
  const context = buildDocumentContext(document);
  const linePrefix = document
    .lineAt(position.line)
    .text.slice(0, position.character);
  const qualifiedMatch = linePrefix.match(
    /([A-Za-z_][A-Za-z0-9_\/]*)\:([A-Za-z_][A-Za-z0-9_]*)?$/
  );

  if (qualifiedMatch) {
    const qualifier = qualifiedMatch[1];
    const moduleIndex = context.importedIndicesByQualifier.get(qualifier);
    if (!moduleIndex) {
      return [];
    }
    return moduleIndex.symbols.map((symbol) => {
      const item = new vscode.CompletionItem(
        symbol,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Imported from ${qualifier}`;
      return item;
    });
  }

  // Inside an open `{--! ... ` pragma directive: offer the known pragma names.
  if (/\{--!\s*[a-z-]*$/.test(linePrefix)) {
    return PRAGMAS.map((name) => {
      const item = new vscode.CompletionItem(
        name,
        vscode.CompletionItemKind.Constant
      );
      item.detail = "Pragma";
      if (PRAGMA_DOCS[name]) {
        item.documentation = new vscode.MarkdownString(PRAGMA_DOCS[name]);
      }
      return item;
    });
  }

  const byLabel = new Map();
  const push = (item) => {
    if (!byLabel.has(item.label)) {
      byLabel.set(item.label, item);
    }
  };

  for (const keyword of KEYWORDS) {
    const item = new vscode.CompletionItem(
      keyword,
      vscode.CompletionItemKind.Keyword
    );
    item.detail = "Bern keyword";
    if (KEYWORD_DOCS[keyword]) {
      item.documentation = new vscode.MarkdownString(KEYWORD_DOCS[keyword]);
    }
    push(item);
  }

  for (const op of WORD_OPERATORS) {
    const item = new vscode.CompletionItem(
      op,
      vscode.CompletionItemKind.Operator
    );
    const opDoc = OPERATOR_DOCS[op];
    item.detail = opDoc
      ? `Operator - written form of ${opDoc.sym}`
      : "Operator (written form)";
    if (opDoc) {
      item.documentation = new vscode.MarkdownString(opDoc.doc);
    }
    push(item);
  }

  for (const builtin of BUILTIN_FUNCTIONS) {
    const item = new vscode.CompletionItem(
      builtin,
      vscode.CompletionItemKind.Function
    );
    item.detail = "Core function";
    if (BUILTIN_DOCS[builtin]) {
      item.documentation = new vscode.MarkdownString(BUILTIN_DOCS[builtin]);
    }
    push(item);
  }

  for (const snippet of SNIPPETS) {
    const item = new vscode.CompletionItem(
      snippet.label,
      vscode.CompletionItemKind.Snippet
    );
    item.insertText = new vscode.SnippetString(snippet.insertText);
    item.detail = snippet.detail;
    push(item);
  }

  for (const symbol of context.localIndex.symbols) {
    const item = new vscode.CompletionItem(
      symbol,
      vscode.CompletionItemKind.Variable
    );
    item.detail = "Local symbol";
    push(item);
  }

  for (const [symbol, owners] of context.symbolOwners.entries()) {
    if (owners.size === 1) {
      const owner = Array.from(owners)[0];
      const item = new vscode.CompletionItem(
        symbol,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Imported from ${owner}`;
      push(item);
      continue;
    }
    for (const owner of owners) {
      const item = new vscode.CompletionItem(
        `${owner}:${symbol}`,
        vscode.CompletionItemKind.Function
      );
      item.detail = `Ambiguous symbol '${symbol}'`;
      push(item);
    }
  }

  return Array.from(byLabel.values());
}

function provideDefinition(document, position) {
  const token = getSymbolTokenAtPosition(document, position);
  if (!token) {
    return undefined;
  }

  const context = buildDocumentContext(document);
  if (token.qualifier) {
    const moduleIndex = context.importedIndicesByQualifier.get(token.qualifier);
    if (!moduleIndex) {
      return undefined;
    }
    const defs = moduleIndex.definitions.get(token.symbol) || [];
    return defs.map((entry) => entry.location);
  }

  const localDefs = context.localIndex.definitions.get(token.symbol) || [];
  if (localDefs.length > 0) {
    return localDefs.map((entry) => entry.location);
  }

  const owners = context.symbolOwners.get(token.symbol);
  if (!owners || owners.size === 0) {
    return undefined;
  }

  const locations = [];
  for (const owner of owners) {
    const moduleIndex = context.importedIndicesByQualifier.get(owner);
    if (!moduleIndex) {
      continue;
    }
    const defs = moduleIndex.definitions.get(token.symbol) || [];
    for (const def of defs) {
      locations.push(def.location);
    }
  }

  return locations.length > 0 ? locations : undefined;
}

function provideReferences(document, position, options) {
  const token = getSymbolTokenAtPosition(document, position);
  if (!token) {
    return [];
  }

  if (token.qualifier) {
    return findQualifiedReferences(document, token.qualifier, token.symbol);
  }

  const references = findUnqualifiedReferences(document, token.symbol);
  if (options.includeDeclaration) {
    return references;
  }

  const localIndex = extractModuleIndex(document.getText(), document.uri);
  const declarations = localIndex.definitions.get(token.symbol) || [];
  const declarationKeys = new Set(
    declarations.map((entry) => locationKey(entry.location))
  );

  return references.filter(
    (location) => !declarationKeys.has(locationKey(location))
  );
}

function provideDocumentSymbols(document) {
  const localIndex = extractModuleIndex(document.getText(), document.uri);
  const symbols = [];

  for (const [name, defs] of localIndex.definitions.entries()) {
    if (defs.length === 0) {
      continue;
    }
    const first = defs[0];
    const symbol = new vscode.DocumentSymbol(
      name,
      first.kind,
      toVscodeSymbolKind(first.kind),
      first.location.range,
      first.location.range
    );
    symbols.push(symbol);
  }

  symbols.sort((a, b) => a.range.start.line - b.range.start.line);
  return symbols;
}

function updateDiagnostics(document, diagnostics) {
  const source = document.getText();
  const context = buildDocumentContext(document);
  const allDiagnostics = [];

  const ambiguousCalls = /\b([A-Za-z_][A-Za-z0-9_]*)\s*\(/g;
  let callMatch = ambiguousCalls.exec(source);
  while (callMatch) {
    const name = callMatch[1];
    const owners = context.symbolOwners.get(name);
    const index = callMatch.index;
    const before = source.slice(Math.max(0, index - 20), index);

    if (owners && owners.size > 1 && !/\:\s*$/.test(before) && !isDefCall(source, index)) {
      const ownerList = Array.from(owners).map((owner) => `${owner}:${name}(...)`);
      const start = document.positionAt(index);
      const end = document.positionAt(index + name.length);
      const range = new vscode.Range(start, end);
      allDiagnostics.push(
        new vscode.Diagnostic(
          range,
          `Ambiguous imported symbol '${name}'. Use ${ownerList.join(" or ")}.`,
          vscode.DiagnosticSeverity.Warning
        )
      );
    }

    callMatch = ambiguousCalls.exec(source);
  }

  diagnostics.set(document.uri, allDiagnostics);
}

function isDefCall(source, index) {
  const lineStart = source.lastIndexOf("\n", index - 1) + 1;
  const prefix = source.slice(lineStart, index);
  return /^\s*def\s+$/.test(prefix);
}

function buildDocumentContext(document) {
  const source = document.getText();
  const imports = parseImports(source);
  const localIndex = extractModuleIndex(source, document.uri);
  const symbolOwners = new Map();
  const importedIndicesByQualifier = new Map();
  const importsByQualifier = new Map();

  for (const imp of imports) {
    const modulePath = resolveModulePath(imp.moduleName, document);
    if (!modulePath) {
      continue;
    }

    const moduleIndex = readModuleIndex(modulePath);
    importedIndicesByQualifier.set(imp.qualifier, moduleIndex);
    importsByQualifier.set(imp.qualifier, imp.moduleName);

    for (const symbol of moduleIndex.symbols) {
      if (!symbolOwners.has(symbol)) {
        symbolOwners.set(symbol, new Set());
      }
      symbolOwners.get(symbol).add(imp.qualifier);
    }
  }

  return {
    localIndex,
    importsByQualifier,
    importedIndicesByQualifier,
    symbolOwners,
  };
}

function parseImports(source) {
  const imports = [];
  const importRegex =
    /^\s*import\s+([A-Za-z_][A-Za-z0-9_\/]*)(?:\s+as\s+([A-Za-z_][A-Za-z0-9_]*))?/gm;
  let match = importRegex.exec(source);
  while (match) {
    imports.push({
      moduleName: match[1],
      qualifier: match[2] || match[1],
    });
    match = importRegex.exec(source);
  }
  return imports;
}

function extractModuleIndex(source, uri) {
  const definitions = new Map();
  const symbols = new Set();
  const lineOffsets = buildLineOffsets(source);

  const addDefinition = (name, kind, startOffset, endOffset) => {
    if (!name || KEYWORDS.includes(name)) {
      return;
    }
    symbols.add(name);
    const location = createLocation(uri, lineOffsets, startOffset, endOffset);
    if (!definitions.has(name)) {
      definitions.set(name, []);
    }
    definitions.get(name).push({ location, kind });
  };

  const addFromRegex = (regex, kind) => {
    let match = regex.exec(source);
    while (match) {
      const name = match[1];
      const relStart = match[0].indexOf(name);
      if (relStart >= 0) {
        const start = match.index + relStart;
        addDefinition(name, kind, start, start + name.length);
      }
      match = regex.exec(source);
    }
  };

  addFromRegex(/^\s*def\s+([A-Za-z_][A-Za-z0-9_]*)\b/gm, "function");
  addFromRegex(/^\s*foreign\s+([A-Za-z_][A-Za-z0-9_]*)\b/gm, "foreign");
  addFromRegex(/^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?::=|=)/gm, "variable");
  // `be` / `recebe` are the written forms of `=`, e.g. `total be 10`.
  addFromRegex(/^\s*([A-Za-z_][A-Za-z0-9_]*)\s+be\b/gm, "variable");
  addFromRegex(/^\s*([A-Za-z_][A-Za-z0-9_]*)\s+recebe\b/gm, "variable");

  const adtRegex =
    /^\s*adt(?:\s+(?:iterable|iterative))?\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$/gm;
  let adtMatch = adtRegex.exec(source);
  while (adtMatch) {
    const typeName = adtMatch[1];
    const rhs = adtMatch[2];
    const full = adtMatch[0];
    const relTypeStart = full.indexOf(typeName);
    if (relTypeStart >= 0) {
      const typeStart = adtMatch.index + relTypeStart;
      addDefinition(typeName, "type", typeStart, typeStart + typeName.length);
    }

    const rhsAbsStart = adtMatch.index + full.indexOf(rhs);
    const parts = rhs.split("|");
    let searchFrom = 0;
    for (const part of parts) {
      const partIndex = rhs.indexOf(part, searchFrom);
      if (partIndex === -1) {
        continue;
      }
      searchFrom = partIndex + part.length;
      const trimmedLeft = part.match(/^\s*/);
      const trimLen = trimmedLeft ? trimmedLeft[0].length : 0;
      const ctorPart = part.slice(trimLen);
      const ctorMatch = ctorPart.match(/^([A-Z][A-Za-z0-9_]*)/);
      if (!ctorMatch) {
        continue;
      }
      const ctorName = ctorMatch[1];
      const ctorStart = rhsAbsStart + partIndex + trimLen;
      addDefinition(
        ctorName,
        "constructor",
        ctorStart,
        ctorStart + ctorName.length
      );
    }

    adtMatch = adtRegex.exec(source);
  }

  return {
    symbols: Array.from(symbols.values()),
    definitions,
  };
}

function resolveModulePath(moduleName, document) {
  const folder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!folder) {
    return null;
  }

  const root = folder.uri.fsPath;
  const localFile = path.join(root, `${moduleName}.brn`);
  const libFile = path.join(root, "lib", `${moduleName}.brn`);
  const relativeFile = path.join(path.dirname(document.uri.fsPath), `${moduleName}.brn`);

  const candidates = [localFile, libFile, relativeFile];
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }
  return null;
}

function readModuleIndex(modulePath) {
  try {
    const stat = fs.statSync(modulePath);
    const cacheHit = moduleIndexCache.get(modulePath);
    if (cacheHit && cacheHit.mtimeMs === stat.mtimeMs) {
      return cacheHit.index;
    }

    const text = fs.readFileSync(modulePath, "utf8");
    const uri = vscode.Uri.file(modulePath);
    const index = extractModuleIndex(text, uri);
    moduleIndexCache.set(modulePath, { mtimeMs: stat.mtimeMs, index });
    return index;
  } catch {
    return { symbols: [], definitions: new Map() };
  }
}

function getSymbolTokenAtPosition(document, position) {
  const range = document.getWordRangeAtPosition(
    position,
    /[A-Za-z_][A-Za-z0-9_]*/
  );
  if (!range) {
    return null;
  }

  const symbol = document.getText(range);
  const lineText = document.lineAt(position.line).text;
  const prefix = lineText.slice(0, range.start.character);
  const qualifierMatch = prefix.match(/([A-Za-z_][A-Za-z0-9_\/]*)\s*:\s*$/);

  return {
    symbol,
    qualifier: qualifierMatch ? qualifierMatch[1] : null,
    range,
  };
}

function findQualifiedReferences(document, qualifier, symbol) {
  const source = document.getText();
  const lineOffsets = buildLineOffsets(source);
  const regex = new RegExp(
    `\\b${escapeRegExp(qualifier)}\\s*:\\s*${escapeRegExp(symbol)}\\b`,
    "g"
  );
  const refs = [];

  let match = regex.exec(source);
  while (match) {
    const symbolOffset = match.index + match[0].lastIndexOf(symbol);
    refs.push(
      createLocation(
        document.uri,
        lineOffsets,
        symbolOffset,
        symbolOffset + symbol.length
      )
    );
    match = regex.exec(source);
  }
  return refs;
}

function findUnqualifiedReferences(document, symbol) {
  const source = document.getText();
  const lineOffsets = buildLineOffsets(source);
  const regex = new RegExp(`\\b${escapeRegExp(symbol)}\\b`, "g");
  const refs = [];

  let match = regex.exec(source);
  while (match) {
    const before = source.slice(Math.max(0, match.index - 20), match.index);
    if (!/\:\s*$/.test(before)) {
      refs.push(
        createLocation(
          document.uri,
          lineOffsets,
          match.index,
          match.index + symbol.length
        )
      );
    }
    match = regex.exec(source);
  }
  return refs;
}

function buildLineOffsets(source) {
  const offsets = [0];
  for (let i = 0; i < source.length; i += 1) {
    if (source[i] === "\n") {
      offsets.push(i + 1);
    }
  }
  return offsets;
}

function offsetToPosition(offset, lineOffsets) {
  let low = 0;
  let high = lineOffsets.length - 1;
  while (low <= high) {
    const mid = Math.floor((low + high) / 2);
    if (lineOffsets[mid] <= offset) {
      if (mid === lineOffsets.length - 1 || lineOffsets[mid + 1] > offset) {
        return new vscode.Position(mid, offset - lineOffsets[mid]);
      }
      low = mid + 1;
    } else {
      high = mid - 1;
    }
  }
  return new vscode.Position(0, offset);
}

function createLocation(uri, lineOffsets, startOffset, endOffset) {
  const start = offsetToPosition(startOffset, lineOffsets);
  const end = offsetToPosition(endOffset, lineOffsets);
  return new vscode.Location(uri, new vscode.Range(start, end));
}

function toVscodeSymbolKind(kind) {
  if (kind === "function" || kind === "foreign") {
    return vscode.SymbolKind.Function;
  }
  if (kind === "variable") {
    return vscode.SymbolKind.Variable;
  }
  if (kind === "type") {
    return vscode.SymbolKind.Class;
  }
  if (kind === "constructor") {
    return vscode.SymbolKind.Constructor;
  }
  return vscode.SymbolKind.Variable;
}

function locationKey(location) {
  return `${location.uri.toString()}:${location.range.start.line}:${location.range.start.character}`;
}

function escapeRegExp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

module.exports = {
  activate,
  deactivate,
};
