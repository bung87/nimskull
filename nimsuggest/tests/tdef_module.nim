import std/[strformat, strutils]
import std/[strformat as sformat]
import
  compiler/ast/[
    ast
  ]

when isMainModule:
  import os, nimsuggest/nimsuggest
  const PkgDir = currentSourcePath.parentDir.parentDir
  var graph = initNimSuggest(currentSourcePath, nimPath = PkgDir.parentDir)
  echo graph.runCmd(ideDef, currentSourcePath.AbsoluteFile, currentSourcePath.AbsoluteFile, 1, 28)
  # echo graph.runCmd(ideDef, currentSourcePath.AbsoluteFile, "".AbsoluteFile, 2, 28)
  # echo graph.runCmd(ideDef, currentSourcePath.AbsoluteFile, "".AbsoluteFile, 5, 6)