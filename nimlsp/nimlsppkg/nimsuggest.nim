when not defined(nimcore):
  {.error: "nimcore MUST be defined for Nim's core tooling".}

import std/[os, net]
import std/options as stdOptions
# import strformat
import
  compiler/ast/[
    idents,
    lineinfos,
    ast,
    syntaxes,
    parser,
    ast_parsed_types,
    ast_types
  ],
  compiler/modules/[
    modules,
    modulegraphs
  ],
  compiler/front/[
    options,
    optionsprocessor,
    # commands,
    msgs,
    cmdlinehelper,
    cli_reporter
  ],
  compiler/utils/[
    # prefixmatches,
    pathutils
  ],
  compiler/sem/[
    sem,
    passes,
    passaux,
  ]

from compiler/ast/reports import Report,
  category,
  kind,
  location

from compiler/front/main import customizeForBackend

from compiler/tools/suggest import isTracked, listUsages, suggestSym, `$`

export Suggest
export IdeCmd
export AbsoluteFile
type
  CachedMsgs = seq[Report]
  NimSuggest* = ref object
    graph: ModuleGraph
    idle: int
    cachedMsgs: CachedMsgs

proc defaultStructuredReportHook(conf: ConfigRef, report: Report): TErrorHandling =
  discard

proc initNimSuggest*(project: string, nimPath: string = ""): NimSuggest =
  var retval: ModuleGraph
  proc mockCommand(graph: ModuleGraph) =
    retval = graph
    let conf = graph.config
    clearPasses(graph)
    registerPass graph, verbosePass
    registerPass graph, semPass
    conf.setCmd cmdIdeTools
    # main module checked in compileProject
    # wantMainModule(conf)

    # if not fileExists(conf.projectFull):
    #   quit "cannot find file: " & conf.projectFull.string

    add(conf.searchPaths, conf.libpath)

    conf.setErrorMaxHighMaybe
    # do not print errors, but log them
    conf.writelnHook = proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
      stderr.writeLine msg
    conf.structuredReportHook = defaultStructuredReportHook

    # compile the project before showing any input so that we already
    # can answer questions right away:
    compileProject(graph)


  proc mockCmdLine(pass: TCmdLinePass, argv: openArray[string];
        conf: ConfigRef) =
    conf.writeHook = proc(conf: ConfigRef, s: string, flags: MsgFlags) = stderr.write s
    
    let a = unixToNativePath(project)
    if dirExists(a) and not fileExists(a.addFileExt("nim")):
      conf.projectName = findProjectNimFile(conf, a)
      # don't make it worse, report the error the old way:
      if conf.projectName.len == 0: conf.projectName = a
    else:
      conf.projectName = a
  let
    cache = newIdentCache()
    conf = newConfigRef(cli_reporter.reportHook)
    self = NimProg(
      suggestMode: true,
      processCmdLine: mockCmdLine
    )
  conf.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  self.initDefinesProg(conf, "nimsuggest")

  self.processCmdLineAndProjectPath(conf, [])

  # Find Nim's prefix dir.
  if nimPath == "":
    let binaryPath = findExe("nim")
    if binaryPath == "":
      raise newException(IOError,
          "Cannot find Nim standard library: Nim compiler not in PATH")
    conf.prefixDir = AbsoluteDir binaryPath.splitPath().head.parentDir()
    if not dirExists(conf.prefixDir / RelativeDir"lib"):
      conf.prefixDir = AbsoluteDir""
  else:
    conf.prefixDir = AbsoluteDir nimPath

  var graph = newModuleGraph(cache, conf)
  graph.onMarkUsed = proc (g: ModuleGraph; info: TLineInfo; s: PSym; usageSym: var PSym; isDecl: bool) =
    suggestSym(g, info, s, usageSym, isDecl)
  graph.onSymImport = graph.onMarkUsed # same callback
  if self.loadConfigsAndProcessCmdLine(cache, conf, graph, []):
    customizeForBackend(graph, conf, backendC)
    mockCommand(graph)

  retval.doStopCompile = proc (): bool = false
  return NimSuggest(graph: retval, idle: 0, cachedMsgs: @[])

proc findNode(n: PNode; trackPos: TLineInfo): PSym =
  if n.kind == nkSym:
    if isTracked(n.info, trackPos, n.sym.name.s.len): return n.sym
  else:
    for i in 0 ..< safeLen(n):
      let res = findNode(n[i], trackPos)
      if res != nil: return res

proc symFromInfo(graph: ModuleGraph; trackPos: TLineInfo; moduleIdx: FileIndex): PSym =
  let m = graph.getModule(moduleIdx)
  if m != nil and m.ast != nil:
    result = findNode(m.ast, trackPos)

proc parsedNodeToSugget(n: ParsedNode; moduleName: string): Suggest =
  if n.kind in {pnkError, pnkEmpty}: return
  new(result)
  let token = getToken(n)
  if token.ident != nil:
    result.name = addr token.ident.s
    result.qualifiedPath = @[moduleName, token.ident.s]
  result.line = token.line.int
  result.column = token.col.int
  var symkind: TSymKind = skUnknown
  case n.kind
    of pnkConstSection: symkind = skConst
    of pnkLetSection: symkind = skLet
    of pnkVarSection: symkind = skVar
    of pnkProcDef: symkind = skProc
    of pnkFuncDef: symkind = skFunc
    of pnkMethodDef: symkind = skMethod
    of pnkConverterDef: symkind = skConverter
    of pnkIteratorDef: symkind = skIterator
    of pnkMacroDef: symkind = skMacro
    of pnkTemplateDef: symkind = skTemplate
    of pnkTypeDef: symkind = skType
    else: discard
  result.symkind = byte symkind

  
proc executeNoHooks(cmd: IdeCmd, file, dirtyfile: AbsoluteFile, line, col: int,
             graph: ModuleGraph) =
  let conf = graph.config
  conf.ideCmd = cmd

  var isKnownFile = true
  let dirtyIdx = fileInfoIdx(conf, file, isKnownFile)

  if not dirtyfile.isEmpty: msgs.setDirtyFile(conf, dirtyIdx, dirtyfile)
  else: msgs.setDirtyFile(conf, dirtyIdx, AbsoluteFile"")

  conf.m.trackPos = newLineInfo(dirtyIdx, line, col)
  conf.m.trackPosAttached = false
  conf.errorCounter = 0
  var moduleIdx: FileIndex
  var needCompile = true
  if conf.ideCmd in {ideUse, ideDus} and
      dirtyfile.isEmpty:
    needCompile = false
  if conf.ideCmd == ideOutline:
    needCompile = false
    var parser: Parser
    var sug: Suggest
    var parsedNode: ParsedNode
    let m = splitFile(file.string)
    if setupParser(parser, dirtyIdx, graph.cache, conf):
      while true:
        parsedNode = parser.parseTopLevelStmt()
        if parsedNode.kind == pnkEmpty:
          break
        sug = parsedNodeToSugget(parsedNode, m.name)
        if sug != nil:
          sug.filepath = file.string
          conf.suggestionResultHook(sug)
      closeParser(parser)

  if needCompile:
    if not isKnownFile:
      moduleIdx = dirtyIdx
      stderr.writeLine "Compile unknown module: " & toFullPath(conf, moduleIdx).string
      discard graph.compileModule(moduleIdx, {})
    else:
      moduleIdx = graph.parentModule(dirtyIdx)
      stderr.writeLine "Compile known module: " & toFullPath(conf, moduleIdx).string
      graph.markDirty dirtyIdx
      graph.markClientsDirty dirtyIdx
      # partially recompiling the project means that that VM and JIT state
      # would become stale, which we prevent by discarding all of it:
      graph.vm = nil
      if conf.ideCmd != ideMod:
        discard graph.compileModule(moduleIdx, {})
  if conf.ideCmd in {ideUse, ideDus}:
    let u = graph.symFromInfo(conf.m.trackPos, moduleIdx)
    if u != nil:
      listUsages(graph, u)
    else:
      stderr.writeLine "found no symbol at position: " & (conf $ conf.m.trackPos)

proc runCmd*(nimsuggest: NimSuggest, cmd: IdeCmd, file,
      dirtyfile: AbsoluteFile, line, col: int): seq[Suggest] =
  var retval: seq[Suggest] = @[]
  let conf = nimsuggest.graph.config
  conf.ideCmd = cmd
  conf.suggestionResultHook = proc (s: Suggest) =
    retval.add(s)
  
  if conf.ideCmd == ideKnown:
    retval.add(Suggest(section: ideKnown, quality: ord(fileInfoKnown(conf, file))))
  elif conf.ideCmd == ideProject:
    retval.add(Suggest(section: ideProject,
        filePath: string conf.projectFull))
  else:
    # if conf.ideCmd == ideChk:
    #   for cm in nimsuggest.cachedMsgs: errorHook(conf, cm.info, cm.msg, cm.sev)
    if conf.ideCmd == ideChk:
      conf.structuredReportHook = proc (conf: ConfigRef, report: Report): TErrorHandling =
        let loc = report.location()
        if stdOptions.isSome(loc):
          let info = loc.get()
          retval.add(Suggest(section: ideChk, filePath: toFullPath(conf,
                  info),
            line: toLinenumber(info), column: toColumn(info), 
            forth: $severity(conf, report)))
        return doNothing
    else:
      conf.structuredReportHook = defaultStructuredReportHook
    executeNoHooks(conf.ideCmd, file, dirtyfile, line, col,
            nimsuggest.graph)
  return retval
