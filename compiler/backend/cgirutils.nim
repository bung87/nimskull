## Non-essential code-generator IR related routines.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_types,
    typesrenderer
  ],
  compiler/backend/[
    cgir
  ],
  compiler/utils/[
    idioms
  ]

from compiler/ast/ast import id

proc treeRepr*(n: CgNode): string =
  ## Renders the tree representation of `n` to text.
  proc treeRepr(n: CgNode, indent: int, result: var string) {.nimcall.} =
    result.add $n.kind
    result.add " "
    if n.typ != nil:
      result.add "typ: "
      result.add $n.typ
      result.add " "

    case n.kind
    of cnkIntLit:
      result.add "intVal: "
      result.add $n.intVal
    of cnkUIntLit:
      result.add "uintVal: "
      result.add $cast[BiggestUInt](n.intVal)
    of cnkFloatLit:
      result.add "floatVal: "
      result.add $n.floatVal
    of cnkStrLit:
      result.add "strVal: \""
      result.add n.strVal
      result.add "\""
    of cnkPragmaStmt:
      result.add "pragma: "
      result.add $n.pragma
    of cnkSym:
      result.add "sym: "
      result.add n.sym.name.s
      result.add " id: "
      result.add $n.sym.itemId
    of cnkLocal:
      result.add "local: "
      result.add $n.local.int
    of cnkMagic:
      result.add "magic: "
      result.add $n.magic
    of cnkEmpty, cnkInvalid, cnkType, cnkAstLit, cnkNilLit, cnkReturnStmt:
      discard
    of cnkWithOperand:
      result.add "\n"
      result.add repeat("  ", indent)
      treeRepr(n.operand, indent+1, result)
    of cnkWithItems:
      result.add "\n"
      for i in 0..<n.len:
        if i > 0:
          result.add "\n"
        result.add repeat("  ", indent)
        result.add $i
        result.add ": "
        treeRepr(n[i], indent+1, result)

  treeRepr(n, 0, result)

type
  Symbol = object
    ## The common denomitator for representing all kinds of symbols in a way
    ## that makes them comparable, at least within a procedure.
    kind: uint8 ## the name-space
    id: int     ## the ID within the name-space

  RenderCtx = object
    syms: seq[tuple[name: PIdent, sym: Symbol]]
      ## remembers the already-rendered symbols. Used to provide unique names.

proc disambiguate(c: var RenderCtx, name: PIdent, s: Symbol): int =
  ## Computes and returns a number to append to the symbol name in order to
  ## make it unique in the output. This way, multiple different symbols sharing
  ## the same name can be disambiguated.
  result = 0
  for it in c.syms.items:
    if it.sym == s:
      return
    elif it.name.id == name.id: # same name?
      inc result

  c.syms.add (name, s) # remember the symbol

proc renderSymbol(c: var RenderCtx, name: PIdent, s: Symbol, flags: TSymFlags,
                  res: var string) =
  ## Appends the textual representation for the given symbol to `res`. `name`
  ## may be 'nil', in which case only the '_cursor' postfix, if required, is
  ## rendered.
  if name != nil:
    res.add name.s
    let postfix = disambiguate(c, name, s)
    if postfix > 0:
      res.add "_" & $postfix

  # the rendered code is currently used for the ``--expandArc`` debug feature,
  # so we also highlight cursor locations
  if sfCursor in flags:
    res.add "_cursor"

proc render(c: var RenderCtx, body: Body, ind: int, n: CgNode,
            res: var string) =
  template add(s: var string, n: CgNode) =
    render(c, body, ind, n, res)

  template indent(extra = 1) =
    if res.len > 0 and res[^1] == ' ':
      # remove trailing space
      res.setLen(res.len - 1)
    res.add "\n"
    res.add repeat("  ", ind + extra)

  template newLine() =
    indent(0)

  template renderList(n: CgNode, sep: untyped; start: int = 0; fin: int = 0) =
    ## Renders the items in the slice ``start..<n.len - fin``
    for i in start..<n.len-fin:
      if i > start:
        sep
      res.add n[i]

  template renderList(n: CgNode, sep: string; start: int = 0; fin: int = 0) =
    ## Renders the items in the slice ``start..<n.len - fin``
    renderList(n, res.add(sep), start, fin)

  case n.kind
  of cnkIntLit:
    res.addInt n.intVal
  of cnkUIntLit:
    res.addInt cast[BiggestUInt](n.intVal)
  of cnkFloatLit:
    res.add $n.floatVal
  of cnkStrLit:
    res.add '"'
    res.add n.strVal
    res.add '"'
  of cnkNilLit:
    res.add "nil"
  of cnkAstLit:
    res.add "<ast>"
  of cnkSym:
    let s = n.sym
    if s.magic == mNone:
      renderSymbol(c, s.name, Symbol(kind: 0, id: n.sym.id), s.flags, res)
    else:
      # magics are never cursors nor do they need disambiguation
      res.add s.name.s
  of cnkLocal:
    let name = body[n.local].name
    if name.isNil:
      # has no user-provided name. These are usually auxiliary locals, so
      # the "aux" prefix is used
      res.add ":aux_"
      res.addInt n.local.int

    renderSymbol(c, name, Symbol(kind: 1, id: n.local.int),
                 body[n.local].flags, res)
  of cnkMagic:
    # cut off the 'm' prefix and use lower-case for the first character
    var name = substr($n.magic, 1)
    name[0] = toLowerAscii(name[0])
    res.add name
  of cnkType:
    if n.typ.sym != nil:
      res.add $n.typ
    else:
      res.add "[type node]"
  of cnkCheckedFieldAccess:
    res.add n[0]
  of cnkHiddenAddr, cnkDerefView, cnkHiddenConv:
    res.add n.operand
  of cnkAddr:
    res.add "addr "
    res.add n.operand
  of cnkDeref:
    res.add n.operand
    res.add "[]"
  of cnkFieldAccess:
    res.add n[0]
    res.add '.'
    res.add n[1]
  of cnkBracketAccess:
    res.add n[0]
    res.add '['
    res.add n[1]
    res.add ']'
  of cnkRange:
    res.add n[0]
    res.add ".."
    res.add n[1]
  of cnkCast:
    res.add "cast["
    res.add $n.typ
    res.add "]("
    res.add n.operand
    res.add ")"
  of cnkStringToCString:
    res.add "cstring("
    res.add n.operand
    res.add ')'
  of cnkCStringToString:
    res.add "string("
    res.add n.operand
    res.add ')'
  of cnkConv:
    if n.typ.sym != nil:
      res.add $n.typ
    else:
      res.add "[type node]"
    res.add '('
    res.add n.operand
    res.add ')'
  of cnkObjUpConv, cnkObjDownConv:
    res.add $n.typ
    res.add "("
    res.add n.operand
    res.add ")"
  of cnkBinding:
    res.add n[0]
    res.add ": "
    res.add n[1]
  of cnkCall:
    res.add n[0]
    res.add '('
    let ind = ind + 1
    renderList(n, ", ", 1)
    res.add ')'

  of cnkObjConstr:
    res.add $n.typ
    res.add '('
    renderList(n, ", ", 1)
    res.add ')'
  of cnkTupleConstr, cnkClosureConstr:
    res.add '('
    renderList(n, ", ")
    res.add ')'
  of cnkArrayConstr:
    res.add '['
    renderList(n, ", ")
    res.add ']'
  of cnkSetConstr:
    res.add '{'
    renderList(n, ", ")
    res.add '}'

  of cnkAsgn, cnkFastAsgn:
    res.add n[0]
    res.add " = "
    let ind = ind + 1
    res.add n[1]
  of cnkDef:
    res.add "var "
    res.add n[0]
    if n[1].kind != cnkEmpty:
      res.add " = "
      let ind = ind + 1
      res.add n[1]
  of cnkPragmaStmt:
    res.add "{."
    let name = substr($n.pragma, 1) # cut off the 'w' prefix
    res.add name
    res.add ".}"
  of cnkReturnStmt:
    res.add "return"
  of cnkVoidStmt:
    res.add "discard "
    res.add n[0]
  of cnkBreakStmt:
    res.add "break "
    res.add n[0]
  of cnkRaiseStmt:
    if n[0].kind == cnkEmpty:
      res.add "raise"
    else:
      res.add "raise "
      res.add n[0]
  of cnkAsmStmt:
    res.add "asm "
    let ind = ind + 1
    renderList(n, ", ")
    res.add ""
  of cnkEmitStmt:
    res.add "{.emit: "
    let ind = ind + 1
    renderList(n, ", ")
    res.add ".}"
  of cnkRepeatStmt:
    res.add "while true:"
    indent()
    render(c, body, ind + 1, n[0], res)
  of cnkBlockStmt:
    if n[0].kind == cnkEmpty:
      res.add "block:"
    else:
      res.add "block "
      res.add n[0]
      res.add ":"
    indent()
    render(c, body, ind + 1, n[1], res)
  of cnkIfStmt:
    res.add "if "
    res.add n[0]
    res.add ':'
    indent()
    render(c, body, ind + 1, n[1], res)
  of cnkCaseStmt:
    res.add "case "
    res.add n[0]
    for i in 1..<n.len:
      newLine()
      if n[i].len > 1:
        res.add "of "
        renderList(n[i], ", ", 1, 1)
      else:
        res.add "else"

      res.add ":"
      indent()
      render(c, body, ind + 1, n[i][^1], res)
  of cnkTryStmt:
    res.add "try:"
    indent()
    render(c, body, ind + 1, n[0], res)
    for i in 1..<n.len:
      case n[i].kind
      of cnkExcept:
        newLine()
        res.add "except:"
        indent()
        render(c, body, ind + 1, n[i][^1], res)
      of cnkFinally:
        newLine()
        res.add "finally:"
        indent()
        render(c, body, ind + 1, n[i][0], res)
      else:
        unreachable()
  of cnkStmtListExpr:
    newLine()
    renderList(n, newLine())
  of cnkStmtList:
    renderList(n, newLine())
  of cnkEmpty:
    discard
  of cnkInvalid, cnkExcept, cnkFinally, cnkBranch:
    unreachable(n.kind)

proc render*(body: Body): string =
  ## Renders `body` to human-readable code that tries to emulate the shape
  ## of the high-level language. The output is meant for debugging and tracing
  ## and is not guaranteed to have a stable format.
  var c = RenderCtx()
  render(c, body, 0, body.code, result)