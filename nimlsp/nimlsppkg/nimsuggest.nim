when not defined(nimcore):
  {.error: "nimcore MUST be defined for Nim's core tooling".}

import std/[os, net]
import std/options as stdOptions
# import strformat
import
  compiler/ast/[
    idents,
    lineinfos,
    ast
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

# from compiler/front/main import customizeForBackend

from compiler/tools/suggest import isTracked, listUsages, `$`

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
    wantMainModule(conf)

    if not fileExists(conf.projectFull):
      quit "cannot find file: " & conf.projectFull.string

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
    conf.suggestVersion = 0
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
  if self.loadConfigsAndProcessCmdLine(cache, conf, graph, []):
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

proc symFromInfo(graph: ModuleGraph; trackPos: TLineInfo): PSym =
  let m = graph.getModule(trackPos.fileIndex)
  if m != nil and m.ast != nil:
    result = findNode(m.ast, trackPos)

proc executeNoHooks(cmd: IdeCmd, file, dirtyfile: AbsoluteFile, line, col: int,
             graph: ModuleGraph) =
  let conf = graph.config
  conf.ideCmd = cmd
  if cmd == ideUse and conf.suggestVersion != 0:
    graph.resetAllModules()
  var isKnownFile = true
  let dirtyIdx = fileInfoIdx(conf, file, isKnownFile)

  if not dirtyfile.isEmpty: msgs.setDirtyFile(conf, dirtyIdx, dirtyfile)
  else: msgs.setDirtyFile(conf, dirtyIdx, AbsoluteFile"")

  conf.m.trackPos = newLineInfo(dirtyIdx, line, col)
  conf.m.trackPosAttached = false
  conf.errorCounter = 0
  if conf.suggestVersion == 1:
    graph.usageSym = nil
  if not isKnownFile:
    graph.compileProject(dirtyIdx)
  if conf.suggestVersion == 0 and conf.ideCmd in {ideUse, ideDus} and
      dirtyfile.isEmpty:
    discard "no need to recompile anything"
  else:
    let modIdx = graph.parentModule(dirtyIdx)
    graph.markDirty dirtyIdx
    graph.markClientsDirty dirtyIdx
    if conf.ideCmd != ideMod:
      if isKnownFile:
        graph.compileProject(modIdx)
  if conf.ideCmd in {ideUse, ideDus}:
    let u = if conf.suggestVersion != 1: graph.symFromInfo(conf.m.trackPos) else: graph.usageSym
    if u != nil:
      listUsages(graph, u)
    else:
      stderr.writeLine "found no symbol at this position " & (conf $ conf.m.trackPos)

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
