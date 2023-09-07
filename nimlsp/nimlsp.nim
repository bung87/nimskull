import std/[algorithm, hashes, os, osproc, sets,
            streams, strformat, strutils, tables, uri]

import nimlsppkg/[baseprotocol, logger, suggestlib, utfmapping]
include nimlsppkg/[messages, messageenums]


const
  version = block:
    var version = "0.0.0"
    let nimbleFile = staticRead(currentSourcePath().parentDir / "nimlsp.nimble")
    for line in nimbleFile.splitLines:
      let keyval = line.split('=')
      if keyval.len == 2:
        if keyval[0].strip == "version":
          version = keyval[1].strip(chars = Whitespace + {'"'})
          break
    version
  # This is used to explicitly set the default source path
  explicitSourcePath {.strdefine.} = getCurrentCompilerExe().parentDir.parentDir

type
  UriParseError* = object of Defect
    uri: string

var nimpath = explicitSourcePath

infoLog("Version: ", version)
infoLog("explicitSourcePath: ", explicitSourcePath)
for i in 1..paramCount():
  infoLog("Argument ", i, ": ", paramStr(i))

var
  gotShutdown = false
  initialized = false
  projectFiles = initTable[string, tuple[nimsuggest: NimSuggest, openFiles: OrderedSet[string]]]()
  openFiles = initTable[string, tuple[projectFile: string, fingerTable: seq[seq[tuple[u16pos, offset: int]]]]]()

template fileuri(p: untyped): string =
  p["textDocument"]["uri"].getStr

template filePath(p: untyped): string =
  p.fileuri[7..^1]

template filestash(p: untyped): string =
  storage / (hash(p.fileuri).toHex & ".nim" )

template rawLine(p: untyped): int =
  p["position"]["line"].getInt

template rawChar(p: untyped): int =
  p["position"]["character"].getInt

template col(openFiles: typeof openFiles; p: untyped): int =
  openFiles[p.fileuri].fingerTable[p.rawLine].utf16to8(p.rawChar)

template textDocumentRequest(message: typed; kind: typed; name, body: untyped): untyped =
  if message.hasKey("params"):
    let p = message["params"]
    var name = kind(p)
    if p.isValid(kind, allowExtra = true):
      body
    else:
      debugLog("Unable to parse data as ", kind)

template textDocumentNotification(message: typed; kind: typed; name, body: untyped): untyped =
  if message.hasKey("params"):
    var p = message["params"]
    var name = kind(p)
    if p.isValid(kind, allowExtra = false):
      if "languageId" notin name["textDocument"] or name["textDocument"]["languageId"].getStr == "nim":
        body
      else:
        debugLog("Unable to parse data as ", kind)

proc pathToUri(path: string): string =
  # This is a modified copy of encodeUrl in the uri module. This doesn't encode
  # the / character, meaning a full file path can be passed in without breaking
  # it.
  result = newStringOfCap(path.len + path.len shr 2) # assume 12% non-alnum-chars
  when defined(windows):
    result.add '/'
  for c in path:
    case c
    # https://tools.ietf.org/html/rfc3986#section-2.3
    of 'a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', '~', '/': result.add c
    of '\\':
      when defined(windows):
        result.add '/'
      else:
        result.add '%'
        result.add toHex(ord(c), 2)
    else:
      result.add '%'
      result.add toHex(ord(c), 2)

proc uriToPath(uri: string): string =
  ## Convert an RFC 8089 file URI to a native, platform-specific, absolute path.
  #let startIdx = when defined(windows): 8 else: 7
  #normalizedPath(uri[startIdx..^1])
  let parsed = uri.parseUri
  if parsed.scheme != "file":
    var e = newException(UriParseError, &"Invalid scheme: {parsed.scheme}, only \"file\" is supported")
    e.uri = uri
    raise e
  if parsed.hostname != "":
    var e = newException(UriParseError, &"Invalid hostname: {parsed.hostname}, only empty hostname is supported")
    e.uri = uri
    raise e
  return normalizedPath(
    when defined(windows):
      parsed.path[1..^1]
    else:
      parsed.path).decodeUrl

proc parseId(node: JsonNode): int =
  if node.kind == JString:
    parseInt(node.getStr)
  elif node.kind == JInt:
    node.getInt
  else:
    raise newException(MalformedFrame, "Invalid id node: " & repr(node))

proc respond(outs: Stream, request: JsonNode, data: JsonNode) =
  let resp = create(ResponseMessage, "2.0", parseId(request["id"]), some(data), none(ResponseError)).JsonNode
  outs.sendJson resp

proc error(outs: Stream, request: JsonNode, errorCode: ErrorCode, message: string, data: JsonNode) =
  let resp = create(ResponseMessage, "2.0", parseId(request["id"]), none(JsonNode), some(create(ResponseError, ord(errorCode), message, data))).JsonNode
  outs.sendJson resp

proc notify(outs: Stream, notification: string, data: JsonNode) =
  let resp = create(NotificationMessage, "2.0", notification, some(data)).JsonNode
  outs.sendJson resp

type Certainty = enum
  None,
  Folder,
  Cfg,
  Nimble

proc getProjectFile(fileUri: string): string =
  let file = fileUri.decodeUrl
  result = file
  let (dir, _, _) = result.splitFile()
  var
    path = dir
    certainty = None
  while not path.isRootDir:
    let
      (dir, fname, ext) = path.splitFile()
      current = fname & ext
    if fileExists(path / current.addFileExt(".nim")) and certainty <= Folder:
      result = path / current.addFileExt(".nim")
      certainty = Folder
    if fileExists(path / current.addFileExt(".nim")) and
      (fileExists(path / current.addFileExt(".nim.cfg")) or
      fileExists(path / current.addFileExt(".nims"))) and certainty <= Cfg:
      result = path / current.addFileExt(".nim")
      certainty = Cfg
    if certainty <= Nimble:
      for nimble in walkFiles(path / "*.nimble"):
        let info = execProcess("nimble dump " & nimble)
        var sourceDir, name: string
        for line in info.splitLines:
          if line.startsWith("srcDir"):
            sourceDir = path / line[(1 + line.find '"')..^2]
          if line.startsWith("name"):
            name = line[(1 + line.find '"')..^2]
        let projectFile = sourceDir / (name & ".nim")
        if sourceDir.len != 0 and name.len != 0 and
            file.isRelativeTo(sourceDir) and fileExists(projectFile):
          result = projectFile
          certainty = Nimble
    path = dir

template getNimsuggest(fileuri: string): Nimsuggest =
  projectFiles[openFiles[fileuri].projectFile].nimsuggest

if paramCount() == 1:
  case paramStr(1):
    of "--help":
      echo "Usage: nimlsp [OPTION | PATH]\n"
      echo "--help, shows this message"
      echo "--version, shows only the version"
      echo "PATH, path to the Nim source directory, defaults to \"", nimpath, "\""
      quit 0
    of "--version":
      echo "nimlsp v", version
      when defined(debugLogging): echo "Compiled with debug logging"
      when defined(debugCommunication): echo "Compiled with communication logging"
      quit 0
    else: nimpath = expandFilename(paramStr(1))
if not fileExists(nimpath / "config/nim.cfg"):
  stderr.write &"""Unable to find "config/nim.cfg" in "{nimpath
  }". Supply the Nim project folder by adding it as an argument.
"""
  quit 1

proc checkVersion(outs: Stream) =
  let
    nimoutputTuple =
      execCmdEx("nim --version", options = {osproc.poEvalCommand, osproc.poUsePath})
  if nimoutputTuple.exitcode == 0:
    let
      nimoutput = nimoutputTuple.output
      versionStart = "Nim Compiler Version ".len
      version = nimoutput[versionStart..<nimoutput.find(" ", versionStart)]
      #hashStart = nimoutput.find("git hash") + 10
      #hash = nimoutput[hashStart..nimoutput.find("\n", hashStart)]
    if version != NimVersion:
      outs.notify("window/showMessage", create(ShowMessageParams, MessageType.Warning.int, message = "Current Nim version does not match the one NimLSP is built against " & version & " != " & NimVersion).JsonNode)

proc main(ins: Stream, outs: Stream) =
  checkVersion(outs)
  var message: JsonNode
  var frame: string
  while true:
    try:
      debugLog "Trying to read message"
      frame = ins.readFrame
      debugLog "Got message"
      message = frame.parseJson
      if isValid(message, RequestMessage):
        debugLog "Got valid Request message of type ", message["method"].getStr
        if not initialized and message["method"].getStr != "initialize":
          outs.error(message, ServerNotInitialized, "Unable to accept requests before being initialized", newJNull())
          continue
        case message["method"].getStr:
          of "shutdown":
            debugLog "Got shutdown request, answering"
            let resp = newJNull()
            outs.respond(message, resp)
            gotShutdown = true
          of "initialize":
            debugLog "Got initialize request, answering"
            initialized = true
            let resp = create(InitializeResult, create(ServerCapabilities,
              textDocumentSync = some(create(TextDocumentSyncOptions,
                openClose = some(true),
                change = some(TextDocumentSyncKind.Full.int),
                willSave = some(false),
                willSaveWaitUntil = some(false),
                save = some(create(SaveOptions, some(true)))
              )), # ?: TextDocumentSyncOptions or int or float
              hoverProvider = some(true), # ?: bool
              completionProvider = some(create(CompletionOptions,
                resolveProvider = some(false),
                triggerCharacters = some(@["."])
              )), # ?: CompletionOptions
              signatureHelpProvider = some(create(SignatureHelpOptions,
                triggerCharacters = some(@["(", ","])
              )), # ?: SignatureHelpOptions
              definitionProvider = some(true), #?: bool
              typeDefinitionProvider = none(bool), #?: bool or TextDocumentAndStaticRegistrationOptions
              implementationProvider = none(bool), #?: bool or TextDocumentAndStaticRegistrationOptions
              referencesProvider = some(true), #?: bool
              documentHighlightProvider = none(bool), #?: bool
              documentSymbolProvider = some(true), #?: bool
              workspaceSymbolProvider = none(bool), #?: bool
              codeActionProvider = none(bool), #?: bool
              codeLensProvider = none(CodeLensOptions), #?: CodeLensOptions
              documentFormattingProvider = none(bool), #?: bool
              documentRangeFormattingProvider = none(bool), #?: bool
              documentOnTypeFormattingProvider = none(DocumentOnTypeFormattingOptions), #?: DocumentOnTypeFormattingOptions
              renameProvider = some(true), #?: bool
              documentLinkProvider = none(DocumentLinkOptions), #?: DocumentLinkOptions
              colorProvider = none(bool), #?: bool or ColorProviderOptions or TextDocumentAndStaticRegistrationOptions
              executeCommandProvider = none(ExecuteCommandOptions), #?: ExecuteCommandOptions
              workspace = none(WorkspaceCapability), #?: WorkspaceCapability
              experimental = none(JsonNode) #?: any
            )).JsonNode
            outs.respond(message,resp)
          of "textDocument/completion":
            textDocumentRequest(message, CompletionParams, req):
              debugLog "Running equivalent of: sug ", req.filePath, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let suggestions = getNimsuggest(req.fileuri).sug(req.filePath, dirtyfile = req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: ",
                suggestions[0 ..< min(suggestions.len, 10)],
                if suggestions.len > 10: &" and {suggestions.len-10} more" else: ""
              var
                completionItems = newJarray()
                seenLabels: CountTable[string]
                addedSuggestions: HashSet[string]
              for suggestion in suggestions:
                seenLabels.inc suggestion.collapseByIdentifier
              for i in 0..suggestions.high:
                let
                  suggestion = suggestions[i]
                  collapsed = suggestion.collapseByIdentifier
                if not addedSuggestions.contains collapsed:
                  addedSuggestions.incl collapsed
                  let
                    seenTimes = seenLabels[collapsed]
                    detail =
                      if seenTimes == 1: some(nimSymDetails(suggestion))
                      else: some(&"[{seenTimes} overloads]")
                  completionItems.add create(CompletionItem,
                    label = suggestion.qualifiedPath[^1].strip(chars = {'`'}),
                    kind = some(nimSymToLSPKind(suggestion).int),
                    detail = detail,
                    documentation = some(suggestion.doc),
                    deprecated = none(bool),
                    preselect = none(bool),
                    sortText = some(fmt"{i:04}"),
                    filterText = none(string),
                    insertText = none(string),
                    insertTextFormat = none(int),
                    textEdit = none(TextEdit),
                    additionalTextEdits = none(seq[TextEdit]),
                    commitCharacters = none(seq[string]),
                    command = none(Command),
                    data = none(JsonNode)
                  ).JsonNode
              outs.respond(message, completionItems)
          of "textDocument/hover":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog "Running equivalent of: def ", req.filePath, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let suggestions = getNimsuggest(req.fileuri).def(req.filePath, dirtyfile = req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: ",
                suggestions[0 ..< min(suggestions.len, 10)],
                if suggestions.len > 10: &" and {suggestions.len-10} more" else: ""
              var resp: JsonNode
              if suggestions.len == 0:
                resp = newJNull()
              else:
                var label = suggestions[0].qualifiedPath.join(".")
                if suggestions[0].forth != "":
                  label &= ": "
                  label &= suggestions[0].forth
                let
                  rangeopt =
                    some(create(Range,
                      create(Position, req.rawLine, req.rawChar),
                      create(Position, req.rawLine, req.rawChar + suggestions[0].qualifiedPath[^1].len)
                    ))
                  markedString = create(MarkedStringOption, "nim", label)
                if suggestions[0].doc != "":
                  resp = create(Hover,
                    @[
                      markedString,
                      create(MarkedStringOption, "", suggestions[0].doc),
                    ],
                    rangeopt
                  ).JsonNode
                else:
                  resp = create(Hover, markedString, rangeopt).JsonNode;
                outs.respond(message, resp)
          of "textDocument/references":
            textDocumentRequest(message, ReferenceParams, req):
              debugLog "Running equivalent of: use ", req.fileuri, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let suggestions = getNimsuggest(req.fileuri).use(req.filePath, dirtyfile = req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: ",
                suggestions[0 ..< min(suggestions.len, 10)],
                if suggestions.len > 10: &" and {suggestions.len-10} more" else: ""
              var response = newJarray()
              for suggestion in suggestions:
                if suggestion.section == ideUse or req["context"]["includeDeclaration"].getBool:
                  response.add create(Location,
                    "file://" & pathToUri(suggestion.filepath),
                    create(Range,
                      create(Position, suggestion.line-1, suggestion.column),
                      create(Position, suggestion.line-1, suggestion.column + suggestion.qualifiedPath[^1].len)
                    )
                  ).JsonNode
              if response.len == 0:
                outs.respond(message, newJNull())
              else:
                outs.respond(message, response)
          of "textDocument/rename":
            textDocumentRequest(message, RenameParams, req):
              debugLog "Running equivalent of: use ", req.fileuri, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let suggestions = getNimsuggest(req.fileuri).use(req.filePath, dirtyfile = req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: ",
                suggestions[0..<min(suggestions.len, 10)],
                if suggestions.len > 10: &" and {suggestions.len-10} more" else: ""
              var resp: JsonNode
              if suggestions.len == 0:
                resp = newJNull()
              else:
                var textEdits = newJObject()
                for suggestion in suggestions:
                  let uri = "file://" & pathToUri(suggestion.filepath)
                  if uri notin textEdits:
                    textEdits[uri] = newJArray()
                  textEdits[uri].add create(TextEdit, create(Range,
                      create(Position, suggestion.line-1, suggestion.column),
                      create(Position, suggestion.line-1, suggestion.column + suggestion.qualifiedPath[^1].len)
                    ),
                    req["newName"].getStr
                  ).JsonNode
                resp = create(WorkspaceEdit,
                  some(textEdits),
                  none(seq[TextDocumentEdit])
                ).JsonNode
                outs.respond(message, resp)
          of "textDocument/definition":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog "Running equivalent of: def ", req.fileuri, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let declarations = getNimsuggest(req.fileuri).def(req.filePath, dirtyfile = req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: ",
                declarations[0..<min(declarations.len, 10)],
                if declarations.len > 10: &" and {declarations.len-10} more" else: ""
              var resp: JsonNode
              if declarations.len == 0:
                resp = newJNull()
              else:
                resp = newJarray()
                for declaration in declarations:
                  resp.add create(Location,
                    "file://" & pathToUri(declaration.filepath),
                    create(Range,
                      create(Position, declaration.line-1, declaration.column),
                      create(Position, declaration.line-1, declaration.column + declaration.qualifiedPath[^1].len)
                    )
                  ).JsonNode
              outs.respond(message, resp)
          of "textDocument/documentSymbol":
            textDocumentRequest(message, DocumentSymbolParams, req):
              debugLog "Running equivalent of: outline ", req.fileuri,
                        " ", req.filestash
              let syms = getNimsuggest(req.fileuri).outline(
                req.fileuri,
                dirtyfile = req.filestash
              )
              debugLog "Found outlines: ", syms[0..<min(syms.len, 10)],
                        if syms.len > 10: &" and {syms.len-10} more" else: ""
              var resp: JsonNode
              if syms.len == 0:
                resp = newJNull()
              else:
                resp = newJarray()
                for sym in syms.sortedByIt((it.line,it.column,it.quality)):
                  if sym.qualifiedPath.len != 2:
                    continue
                  resp.add create(
                    SymbolInformation,
                    sym.name[],
                    nimSymToLSPKind(sym.symKind).int,
                    some(false),
                    create(Location,
                    "file://" & pathToUri(sym.filepath),
                      create(Range,
                        create(Position, sym.line-1, sym.column),
                        create(Position, sym.line-1, sym.column + sym.qualifiedPath[^1].len)
                      )
                    ),
                    none(string)
                  ).JsonNode
              outs.respond(message, resp)
          of "textDocument/signatureHelp":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog "Running equivalent of: con ", req.filePath, " ", req.filestash, "(",
                req.rawLine + 1, ":",
                openFiles.col(req), ")"
              let suggestions = getNimsuggest(req.fileuri).con(req.filePath, dirtyfile = req.filestash, req.rawLine + 1, req.rawChar)
              var signatures = newSeq[SignatureInformation]()
              for suggestion in suggestions:
                var label = suggestion.qualifiedPath.join(".")
                if suggestion.forth != "":
                  label &= ": "
                  label &= suggestion.forth
                signatures.add create(SignatureInformation,
                  label = label,
                  documentation = some(suggestion.doc),
                  parameters = none(seq[ParameterInformation])
                )
              let resp = create(SignatureHelp,
                signatures = signatures,
                activeSignature = some(0),
                activeParameter = some(0)
              ).JsonNode
              outs.respond(message, resp)
          else:
            let msg = "Unknown request method: " & message["method"].getStr
            debugLog msg
            outs.error(message, MethodNotFound, msg, newJObject())
        continue
      elif isValid(message, NotificationMessage):
        debugLog "Got valid Notification message of type ", message["method"].getStr
        if not initialized and message["method"].getStr != "exit":
          continue
        case message["method"].getStr:
          of "exit":
            debugLog "Exiting"
            if gotShutdown:
              quit 0
            else:
              quit 1
          of "initialized":
            discard
          of "textDocument/didOpen":
            textDocumentNotification(message, DidOpenTextDocumentParams, req):
              let
                file = open(req.filestash, fmWrite)
                projectFile = getProjectFile(uriToPath(req.fileuri))
              debugLog "New document opened for URI: ", req.fileuri, " saving to ", req.filestash
              openFiles[req.fileuri] = (
                projectFile: projectFile,
                fingerTable: @[]
              )

              if projectFile notin projectFiles:
                debugLog "Initialising project with ", nimpath, " ", projectFile
                projectFiles[projectFile] = (nimsuggest: initNimsuggest(projectFile, nimpath), openFiles: initOrderedSet[string]())
              projectFiles[projectFile].openFiles.incl(req.fileuri)

              for line in req["textDocument"]["text"].getStr.splitLines:
                openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                file.writeLine line
              file.close()
          of "textDocument/didChange":
            textDocumentNotification(message, DidChangeTextDocumentParams, req):
              let file = open(req.filestash, fmWrite)
              debugLog "Got document change for URI: ", req.fileuri, " saving to ", req.filestash
              openFiles[req.fileuri].fingerTable = @[]
              for line in req["contentChanges"][0]["text"].getStr.splitLines:
                openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                file.writeLine line
              file.close()

              # Notify nimsuggest about a file modification.
              discard getNimsuggest(req.fileuri).mod(req.filePath, dirtyfile = req.filestash)
          of "textDocument/didClose":
            textDocumentNotification(message, DidCloseTextDocumentParams, req):
              let projectFile = getProjectFile(uriToPath(req.fileuri))
              debugLog "Got document close for URI: ", req.fileuri, " copied to ", req.filestash
              removeFile(req.filestash)
              projectFiles[projectFile].openFiles.excl(req.fileuri)
              if projectFiles[projectFile].openFiles.len == 0:
                debugLog "Trying to stop nimsuggest"
                debugLog "Stopped nimsuggest with code: ",
                          getNimsuggest(req.fileuri).stopNimsuggest()
              openFiles.del(req.fileuri)
          of "textDocument/didSave":
            textDocumentNotification(message, DidSaveTextDocumentParams, req):
              if req["text"].isSome:
                let file = open(req.filestash, fmWrite)
                debugLog "Got document save for URI: ", req.fileuri, " saving to ", req.filestash
                openFiles[req.fileuri].fingerTable = @[]
                for line in req["text"].unsafeGet.getStr.splitLines:
                  openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                  file.writeLine line
                file.close()
              debugLog "fileuri: ", req.fileuri, ", project file: ", openFiles[req.fileuri].projectFile, ", dirtyfile: ", req.filestash
              let diagnostics = getNimsuggest(req.fileuri).chk(req.filePath, dirtyfile = req.filestash)
              debugLog "Got diagnostics: ",
                diagnostics[0..<min(diagnostics.len, 10)],
                if diagnostics.len > 10: &" and {diagnostics.len-10} more" else: ""
              var response: seq[Diagnostic]
              for diagnostic in diagnostics:
                if diagnostic.line == 0:
                  continue

                if diagnostic.filePath != req.filePath:
                  continue
                # Try to guess the size of the identifier
                let
                  message = diagnostic.doc
                  endcolumn = diagnostic.column + message.rfind('\'') - message.find('\'') - 1
                response.add create(Diagnostic,
                  create(Range,
                    create(Position, diagnostic.line-1, diagnostic.column),
                    create(Position, diagnostic.line-1, max(diagnostic.column, endcolumn))
                  ),
                  some(case diagnostic.forth:
                    of "Error": DiagnosticSeverity.Error.int
                    of "Hint": DiagnosticSeverity.Hint.int
                    of "Warning": DiagnosticSeverity.Warning.int
                    else: DiagnosticSeverity.Error.int),
                  none(int),
                  some("nimsuggest chk"),
                  message,
                  none(seq[DiagnosticRelatedInformation])
                )

              # Invoke chk on all open files.
              let projectFile = openFiles[req.fileuri].projectFile
              for f in projectFiles[projectFile].openFiles.items:
                let diagnostics = getNimsuggest(f).chk(req.filePath, dirtyfile = req.filestash)
                debugLog "Got diagnostics: ",
                  diagnostics[0 ..< min(diagnostics.len, 10)],
                  if diagnostics.len > 10: &" and {diagnostics.len-10} more" else: ""

                var response: seq[Diagnostic]
                for diagnostic in diagnostics:
                  if diagnostic.line == 0:
                    continue

                  if diagnostic.filePath != uriToPath(f):
                    continue
                  # Try to guess the size of the identifier
                  let
                    message = diagnostic.doc
                    endcolumn = diagnostic.column + message.rfind('\'') - message.find('\'') - 1

                  response.add create(
                    Diagnostic,
                    create(Range,
                      create(Position, diagnostic.line-1, diagnostic.column),
                      create(Position, diagnostic.line-1, max(diagnostic.column, endcolumn))
                    ),
                    some(case diagnostic.forth:
                      of "Error": DiagnosticSeverity.Error.int
                      of "Hint": DiagnosticSeverity.Hint.int
                      of "Warning": DiagnosticSeverity.Warning.int
                      else: DiagnosticSeverity.Error.int),
                    none(int),
                    some("nimsuggest chk"),
                    message,
                    none(seq[DiagnosticRelatedInformation])
                  )
                let resp = create(PublishDiagnosticsParams, f, response).JsonNode
                outs.notify("textDocument/publishDiagnostics", resp)
              let resp = create(PublishDiagnosticsParams,
                req.fileuri,
                response).JsonNode
              outs.notify("textDocument/publishDiagnostics", resp)
          else:
            let msg = "Unknown notification method: " & message["method"].getStr
            warnLog msg
            outs.error(message, MethodNotFound, msg, newJObject())
        continue
      else:
        let msg = "Invalid message: " & frame
        debugLog msg
        outs.error(message, InvalidRequest, msg, newJObject())
    except MalformedFrame as e:
      warnLog "Got Invalid message id: ", e.msg
      continue
    except UriParseError as e:
      warnLog "Got exception parsing URI: ", e.msg
      continue
    except IOError as e:
      errorLog "Got IOError: ", e.msg
      break
    except CatchableError as e:
      warnLog "Got exception: ", e.msg
      continue

var
  ins = newFileStream(stdin)
  outs = newFileStream(stdout)
main(ins, outs)
