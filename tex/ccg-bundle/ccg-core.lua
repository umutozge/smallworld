--[[
  ccg-core.lua  --  CCG derivation engine for LuaLaTeX  (v8)

  :SYN is a pre-encoded quoted string, e.g. "S[+,VR]" or "(S[b]\\NP)/NP".
  In the .sexpr file a backward slash in a category is written as four
  characters \\\\ (s-expr quoting). syn_to_tex() normalises that to one
  real backslash operator before rendering.

  flush() uses tex.sprint so no end-of-line tokens appear inside tabular
  cells (which would cause "Extra }" errors with tex.print).
--]]

local M = {}

-- ── output buffer ────────────────────────────────────────────────────────

local _buf      = {}
local _out_file = nil

local function put(s)
  _buf[#_buf + 1] = s
end

local function flush()
  if _out_file then
    for _, line in ipairs(_buf) do
      _out_file:write(line .. "\n")
    end
  else
    tex.sprint(table.concat(_buf, " "))
  end
  _buf = {}
end

-- ── utilities ────────────────────────────────────────────────────────────

local function trim(s)
  return s:match("^%s*(.-)%s*$")
end

local function starts_with(s, p)
  return s:sub(1, #p) == p
end

-- ── s-expression tokeniser ───────────────────────────────────────────────

local function tokenise(text)
  text = text:gsub(";[^\n]*", "")
  local toks = {}
  local i = 1
  local n = #text
  while i <= n do
    local c = text:sub(i, i)
    if c:match("%s") then
      i = i + 1
    elseif text:sub(i, i + 2):upper() == "#S(" then
      toks[#toks + 1] = "#S("
      i = i + 3
    elseif c == "(" or c == ")" then
      toks[#toks + 1] = c
      i = i + 1
    elseif c == '"' then
      local j = text:find('"', i + 1, true) or n
      toks[#toks + 1] = text:sub(i, j)
      i = j + 1
    else
      local tok = text:match("^([^%s()\"]+)", i)
      if tok then
        if tok:sub(1, 1) == "#" then tok = tok:upper() end
        toks[#toks + 1] = tok
        i = i + #tok
      else
        i = i + 1
      end
    end
  end
  return toks
end

-- ── s-expression reader ──────────────────────────────────────────────────

local Reader = {}
Reader.__index = Reader

function Reader.new(toks)
  return setmetatable({ t = toks, i = 1 }, Reader)
end

function Reader:peek()
  return self.t[self.i]
end

function Reader:pop()
  local v = self.t[self.i]
  if not v then error("ccg-core: unexpected end of input") end
  self.i = self.i + 1
  return v
end

function Reader:read()
  local t = self:peek()
  if not t then error("ccg-core: unexpected end of input") end

  if t == "(" then
    self:pop()
    local lst = {}
    while true do
      local p = self:peek()
      if not p then error("ccg-core: unclosed (") end
      if p == ")" then self:pop(); break end
      lst[#lst + 1] = self:read()
    end
    return lst

  elseif t == "#S(" then
    self:pop()
    local name = self:read()
    local d = { ["#S"] = name }
    while true do
      local p = self:peek()
      if not p then error("ccg-core: unclosed #S(") end
      if p == ")" then self:pop(); break end
      local raw = self:pop()
      local key = raw:match("^:?(.*)"):upper()
      d[key] = self:read()
    end
    return d

  else
    return self:pop()
  end
end

local function parse(text)
  local toks = tokenise(text)
  if #toks == 0 then error("ccg-core: empty input") end
  return Reader.new(toks):read()
end

-- ── file reader ──────────────────────────────────────────────────────────

local function read_file(filename)
  local function try(name)
    local f = io.open(name, "r")
    if f then
      local t = f:read("*a")
      f:close()
      return t
    end
  end
  local text = try(filename) or try(filename .. ".sexpr")
  if not text then
    error('ccg-core: cannot open "' .. filename .. '"')
  end
  return text
end

-- ── rule table ───────────────────────────────────────────────────────────

local RULE = {
  ["#:>"]    = "\\cgfa",
  ["#:<"]    = "\\cgba",
  ["#:>B"]   = "\\cgfc",
  ["#:<B"]   = "\\cgbc",
  ["#:>BX"]  = "\\cgfx",
  ["#:<BX"]  = "\\cgbx",
  ["#:>B2"]  = "\\cgfc$^{2}$",
  ["#:<B2"]  = "\\cgbc$^{2}$",
  ["#:>B3"]  = "\\cgfc$^{3}$",
  ["#:<B3"]  = "\\cgbc$^{3}$",
  ["#:B>"]   = "\\cgfc",
  ["#:B<"]   = "\\cgbc",
  ["#:BX>"]  = "\\cgfx",
  ["#:BX<"]  = "\\cgbx",
  ["#:B2>"]  = "\\cgfc$^{2}$",
  ["#:B2<"]  = "\\cgbc$^{2}$",
  ["#:B3>"]  = "\\cgfc$^{3}$",
  ["#:B3<"]  = "\\cgbc$^{3}$",
  ["#:>T"]   = "\\cgftr",
  ["#:<T"]   = "\\cgbtr",
  ["#:T>"]   = "\\cgftr",
  ["#:T<"]   = "\\cgbtr",
  ["#:>S"]   = "\\cgsf",
  ["#:<S"]   = "\\cgsb",
  ["#:>SX"]  = "\\cgsfx",
  ["#:<SX"]  = "\\cgsbx",
  ["#:S>"]   = "\\cgsf",
  ["#:S<"]   = "\\cgsb",
  ["#:SX>"]  = "\\cgsfx",
  ["#:SX<"]  = "\\cgsbx",
  ["#:PHI"]  = "\\cgdcompf",
  ["#:PSI"]  = "\\cgdcomp",
  ["#:CONJ"] = "\\ensuremath{\\Phi}",
}

local function infer_rule(phon)
  if type(phon) ~= "table" or #phon == 0 then return nil end
  return RULE[tostring(phon[1]):upper()]
end

-- ── category string to TeX ───────────────────────────────────────────────
--
-- Input: quoted string from .sexpr, e.g. "S[+,VR]" "(S[+,VR]\\\\S[+,VR])"
--
-- Modality encoding (attached immediately to slash):
--   plain  /   \    ->  \fs   \bs
--   x      /x  \x   ->  \fxs  \bxs
--   .      /.  \.   ->  \fdots\bdots
--   *      /*  \*   ->  \fstars\bstars
--   ^      /^  \^   ->  \fds  \bds
--
-- Atoms uppercased, wrapped in \cgf{}.
-- Features in [...] lowercased, become subscript inside \cgf{}.

-- We identify backslash by its ASCII code 92 to avoid Lua string-escape
-- ambiguity throughout this function.
local BS = string.char(92)   -- one backslash character

local SLASH_MACRO = {
  ["/"]        = "\\fs ",
  [BS]         = "\\bs ",
  ["/" .. "x"] = "\\fxs ",
  [BS  .. "x"] = "\\bxs ",
  ["/" .. "."] = "\\fdots ",
  [BS  .. "."] = "\\bdots ",
  ["/" .. "*"] = "\\fstars ",
  [BS  .. "*"] = "\\bstars ",
  ["/" .. "^"] = "\\fds ",
  [BS  .. "^"] = "\\bds ",
}

local function syn_to_tex(syn)
  if type(syn) ~= "string" then return "\\cgf{?}" end

  -- Strip surrounding double-quotes added by the tokeniser.
  local s = syn:match('^"(.*)"$') or syn

  -- Normalise backslash sequences from s-expr quoting.
  -- Four backslashes in the file (\\\\) represent one category backslash.
  -- Two backslashes (\\) also represent one (some writers use that).
  -- We use BS (char 92) as the canonical single backslash.
  s = s:gsub(BS .. BS .. BS .. BS, BS)  -- \\\\ -> \
  s = s:gsub(BS .. BS,             BS)  -- \\   -> \

  local out = {}
  local i = 1
  local n = #s

  while i <= n do
    local c = s:sub(i, i)

    if c == "(" or c == ")" then
      out[#out + 1] = c
      i = i + 1

    elseif c == "/" or c == BS then
      local mod = ""
      if i + 1 <= n then
        local nx = s:sub(i + 1, i + 1)
        if nx == "x" or nx == "." or nx == "*" or nx == "^" then
          mod = nx
          i = i + 2
        else
          i = i + 1
        end
      else
        i = i + 1
      end
      out[#out + 1] = SLASH_MACRO[c .. mod] or "\\fs "

    elseif c == " " then
      i = i + 1

    elseif c == "[" then
      local j = s:find("]", i, true) or n
      out[#out + 1] = "_{" .. s:sub(i + 1, j):lower() .. "}"
      i = j + 1

    else
      local atom = s:match("^([^%(%)%[%]/" .. BS .. " ]+)", i)
      if not atom or #atom == 0 then atom = c end
      i = i + #atom
      atom = atom:upper()
      if i <= n and s:sub(i, i) == "[" then
        local j = s:find("]", i, true) or n
        local feat = s:sub(i + 1, j-1):lower()
        out[#out + 1] = "\\cgf{" .. atom .. "_{" .. feat .. "}}"
        i = j + 1
      else
        out[#out + 1] = "\\cgf{" .. atom .. "}"
      end
    end
  end

  return table.concat(out)
end

-- ── tree helpers ─────────────────────────────────────────────────────────

local function is_sign(x)
  if type(x) ~= "table" then return false end
  local s = x["#S"]
  return s ~= nil and tostring(s):upper() == "SIGN"
end

local function tget(obj, ...)
  for _, k in ipairs({...}) do
    local v = obj[k] or obj[k:upper()] or obj[k:lower()]
    if v ~= nil then return v end
  end
  return nil
end

local function unwrap(obj)
  while type(obj) == "table" and #obj == 1
      and type(obj[1]) == "table" and not obj[1]["#S"] do
    obj = obj[1]
  end
  if type(obj) == "table" and #obj == 1
      and type(obj[1]) == "table" and is_sign(obj[1]) then
    return obj[1]
  end
  return obj
end

local function build_tree(obj)
  obj = unwrap(obj)

  if is_sign(obj) then
    return {
      phon     = tget(obj, "PHON"),
      syn      = tget(obj, "SYN"),
      sem      = tget(obj, "SEM"),
      rule     = nil,
      children = {},
    }
  end

  if type(obj) == "table" and #obj > 0 and is_sign(obj[1]) then
    local h    = obj[1]
    local kids = {}
    for i = 2, #obj do
      kids[#kids + 1] = build_tree(obj[i])
    end
    return {
      phon     = tget(h, "PHON"),
      syn      = tget(h, "SYN"),
      sem      = tget(h, "SEM"),
      rule     = infer_rule(tget(h, "PHON")),
      children = kids,
    }
  end

  error("ccg-core: cannot build tree from "
        .. type(obj)
        .. (type(obj) == "table" and " len=" .. #obj or ""))
end

local function span_len(node)
  if #node.children == 0 then return 1 end
  local s = 0
  for _, c in ipairs(node.children) do
    s = s + span_len(c)
  end
  return s
end

local function leaves(node)
  if #node.children == 0 then return { node } end
  local out = {}
  for _, c in ipairs(node.children) do
    for _, l in ipairs(leaves(c)) do
      out[#out + 1] = l
    end
  end
  return out
end

-- ── surface form ─────────────────────────────────────────────────────────

local function tex_escape(s)
  s = s:gsub("_", "\\_")
  s = s:gsub("&", "\\&")
  s = s:gsub("#", "\\#")
  return s
end

local function surface(node)
  local function words(x)
    if type(x) == "string" then
      if x:sub(1, 1) == "#" or x:sub(1, 1) == "?" then return {} end
      local w = x:match("^|(.+)|$") or x
      return { w:lower() }
    end
    if type(x) == "table" then
      local out = {}
      for _, v in ipairs(x) do
        for _, w in ipairs(words(v)) do
          out[#out + 1] = w
        end
      end
      return out
    end
    return { tostring(x):lower() }
  end
  local ws = words(node.phon)
  local parts = {}
  for _, w in ipairs(ws) do
    parts[#parts + 1] = tex_escape(w)
  end
  return table.concat(parts, " ")
end

-- ── semantics to TeX ─────────────────────────────────────────────────────

local OPERATORS = { lam = true, forall = true, exists = true }

local LOG_CONST = {
  cond  = true, ["and"] = true, ["or"] = true,
  equal = true, prec   = true,
}

local TEX_SYM = {
  forall  = "\\forall",   exists = "\\exists",
  ["and"] = "\\land",     neg    = "\\neg",
  ["or"]  = "\\lor",      cond   = "\\rightarrow",
  prec    = "\\prec",     equal  = "=",
  lam     = "\\lambda",   tau    = "\\tau",
  fall    = "\\mathrm{fall}", three = "3",
}

local GensymCtx = {}
GensymCtx.__index = GensymCtx

function GensymCtx.new()
  return setmetatable({ tbl = {}, cnt = 0 }, GensymCtx)
end

function GensymCtx:name(raw)
  local k = tostring(raw):upper()
  if not self.tbl[k] then
    self.tbl[k] = self.cnt
    self.cnt = self.cnt + 1
  end
  return "x_{" .. self.tbl[k] .. "}"
end

local function is_gensym(s)
  s = tostring(s)
  return starts_with(s, "#") or starts_with(s, "?")
end

local function sem_parse(form, ctx)
  local function atom(f)
    local s = tostring(f)
    if #s == 1 then return s:lower() end
    if is_gensym(s) then return ctx:name(s) end
    local sl = s:lower()
    return TEX_SYM[sl] or ("\\so{" .. sl .. "}")
  end

  if form == nil then return "" end
  if type(form) ~= "table" then return atom(form) end
  if #form == 0 then return "" end

  local head = tostring(form[1]):lower()

  if OPERATORS[head] then
    local op   = TEX_SYM[head] or head
    local var  = atom(form[2] or "?")
    local body = sem_parse(form[3] or "", ctx)
    return op .. " " .. var .. ".\\," .. body
  end

  if LOG_CONST[head] then
    local op  = TEX_SYM[head] or head
    local lhs = sem_parse(form[2], ctx)
    if form[3] then
      local rhs = sem_parse(form[3], ctx)
      return "(" .. lhs .. " " .. op .. " " .. rhs .. ")"
    end
    return lhs .. " " .. op
  end

  -- function application
  local function apply_arg(f_str, arg_form)
    local arg = sem_parse(arg_form, ctx)
    local needs_par = arg:find(" ")
                      and not (arg:sub(1, 1) == "("
                               or arg:sub(1, 1) == "\\")
    if needs_par then
      return f_str .. "\\,(" .. arg .. ")"
    else
      return f_str .. "\\," .. arg
    end
  end

  local r = sem_parse(form[1], ctx)
  for i = 2, #form do
    r = apply_arg(r, form[i])
  end
  return r
end

local function sem_to_tex(sem)
  return trim(sem_parse(sem, GensymCtx.new()))
end

-- ── layout ───────────────────────────────────────────────────────────────

local function depth_up(node)
  if #node.children == 0 then return 0 end
  local mx = 0
  for _, c in ipairs(node.children) do
    mx = math.max(mx, depth_up(c))
  end
  return 1 + mx
end

local function gather_internals(node, acc)
  acc = acc or {}
  for _, c in ipairs(node.children) do
    gather_internals(c, acc)
  end
  if #node.children > 0 then
    acc[#acc + 1] = { depth_up(node), node }
  end
  return acc
end

local function build_row(n, spans, lpos, fmt)
  local is_start = {}
  local is_body  = {}
  local content  = {}
  for i = 1, n do
    is_start[i] = false
    is_body[i]  = false
  end
  for _, nd in ipairs(spans) do
    local lv = leaves(nd)
    local l  = lpos[lv[1]]
    local w  = span_len(nd)
    content[l]  = fmt(nd, w)
    is_start[l] = true
    for i = l + 1, l + w - 1 do
      is_body[i] = true
    end
  end
  local result = {}
  local i = 1
  while i <= n do
    if is_body[i] then
      i = i + 1
    elseif is_start[i] then
      result[#result + 1] = content[i]
      i = i + 1
    else
      local j = i
      while j <= n and not is_start[j] and not is_body[j] do
        j = j + 1
      end
      result[#result + 1] = "\\mc{" .. (j - i) .. "}{}"
      i = j
    end
  end
  return table.concat(result, " & ")
end

-- ── cgex emitter ─────────────────────────────────────────────────────────

local function emit_cgex(node, with_sem)
  local lv   = leaves(node)
  local n    = #lv
  local lpos = {}
  for i, l in ipairs(lv) do lpos[l] = i end

  put("\\cgex{" .. n .. "}{")

  local words = {}
  for _, l in ipairs(lv) do words[#words + 1] = surface(l) end
  put(table.concat(words, " & ") .. "\\\\")

  put("\\cglines{" .. n .. "}\\\\")

  local cats = {}
  for _, l in ipairs(lv) do cats[#cats + 1] = syn_to_tex(l.syn) end
  put(table.concat(cats, " & ") .. "\\\\")

  if with_sem then
    local sems = {}
    for _, l in ipairs(lv) do
      sems[#sems + 1] = "\\lf{" .. sem_to_tex(l.sem) .. "}"
    end
    put(table.concat(sems, " & ") .. "\\\\")
  end

  local all  = gather_internals(node)
  local by_d = {}
  for _, pair in ipairs(all) do
    local d  = pair[1]
    local nd = pair[2]
    if not by_d[d] then by_d[d] = {} end
    by_d[d][#by_d[d] + 1] = nd
  end
  local depths = {}
  for d in pairs(by_d) do depths[#depths + 1] = d end
  table.sort(depths)

  for _, d in ipairs(depths) do
    local spans = by_d[d]
    table.sort(spans, function(a, b)
      return lpos[leaves(a)[1]] < lpos[leaves(b)[1]]
    end)
    put(build_row(n, spans, lpos, function(nd, w)
      return "\\cgline{" .. w .. "}{" .. (nd.rule or "\\cgfa") .. "}"
    end) .. "\\\\")
    put(build_row(n, spans, lpos, function(nd, w)
      local cat = syn_to_tex(nd.syn)
      if with_sem then
        return "\\cgres{" .. w .. "}{" .. cat
               .. " \\lf{" .. sem_to_tex(nd.sem) .. "}}"
      else
        return "\\cgres{" .. w .. "}{" .. cat .. "}"
      end
    end) .. "\\\\")
  end

  put("}")
  flush()
end

-- ── forest emitter ────────────────────────────────────────────────────────

local function forest_node(node, with_sem, indent)
  indent = indent or 2
  local sp  = string.rep(" ", indent)
  local cat = syn_to_tex(node.syn)
  local parts = { "\\textit{" .. surface(node) .. "}", cat }
  if with_sem then
    parts[#parts + 1] = "\\ensuremath{" .. sem_to_tex(node.sem) .. "}"
  end
  local label = "\\shortstack{" .. table.concat(parts, "\\\\") .. "}"
  if #node.children == 0 then
    put(sp .. "[{" .. label .. "}]")
    return
  end
  local rl = node.rule or ""
  local edge = ""
  if rl ~= "" then
    edge = ", edge label={node[midway,fill=white,font=\\scriptsize]{" .. rl .. "}}"
  end
  put(sp .. "[{" .. label .. "}" .. edge)
  for _, c in ipairs(node.children) do
    forest_node(c, with_sem, indent + 2)
  end
  put(sp .. "]")
end

local function emit_forest(node, with_sem)
  put("\\begin{forest}")
  put("  for tree={parent anchor=south, child anchor=north,")
  put("    align=center, l sep=2.5em, s sep=1em, inner sep=0.5ex}")
  forest_node(node, with_sem)
  put("\\end{forest}")
  flush()
end

-- ── dispatch ─────────────────────────────────────────────────────────────

local function emit(tree, mode, with_sem)
  mode = (mode or "cgex"):lower()
  if mode == "cgex" then
    emit_cgex(tree, with_sem)
  elseif mode == "forest" then
    emit_forest(tree, with_sem)
  else
    emit_cgex(tree, with_sem)
    put("\\bigskip")
    flush()
    emit_forest(tree, with_sem)
  end
end

-- ── error wrapper ─────────────────────────────────────────────────────────

local function safe(fn, ...)
  local ok, err = pcall(fn, ...)
  if not ok then
    local msg = tostring(err):gsub("[{}\\]", "|")
    tex.print("\\fbox{\\textcolor{red}{\\textbf{ccg-core error:} " .. msg .. "}}")
  end
end

local function truthy(v)
  if v == nil or v == true then return true end
  if type(v) == "boolean"  then return v    end
  local s = tostring(v):lower()
  return not (s == "false" or s == "0" or s == "no")
end

-- ── public API ────────────────────────────────────────────────────────────

function M.file(infile, mode, ws_flag)
  safe(function()
    local tree = build_tree(parse(read_file(infile)))
    _out_file = nil
    emit(tree, mode, truthy(ws_flag))
  end)
end

function M.dumpfile(infile, outfile, mode, ws_flag)
  safe(function()
    local tree = build_tree(parse(read_file(infile)))
    local f, ferr = io.open(outfile, "w")
    if not f then
      error("ccg-core: cannot write " .. outfile .. ": " .. tostring(ferr))
    end
    f:write("% Auto-generated by ccg-core.lua from: " .. infile .. "\n")
    f:write("% Edit freely. Delete to regenerate.\n%\n")
    _out_file = f
    emit(tree, mode, truthy(ws_flag))
    _out_file = nil
    f:close()
    tex.sprint("\\input{" .. outfile .. "}")
  end)
end

function M.cgexfile(f)     M.file(f, "cgex",   true)  end
function M.cgexfilens(f)   M.file(f, "cgex",   false) end
function M.forestfile(f)   M.file(f, "forest", true)  end
function M.forestfilens(f) M.file(f, "forest", false) end
function M.bothfile(f)     M.file(f, "both",   true)  end
function M.bothfilens(f)   M.file(f, "both",   false) end

function M.cgexdump(i, o)     M.dumpfile(i, o, "cgex",   true)  end
function M.cgexdumpns(i, o)   M.dumpfile(i, o, "cgex",   false) end
function M.forestdump(i, o)   M.dumpfile(i, o, "forest", true)  end
function M.forestdumpns(i, o) M.dumpfile(i, o, "forest", false) end
function M.bothdump(i, o)     M.dumpfile(i, o, "both",   true)  end
function M.bothdumpns(i, o)   M.dumpfile(i, o, "both",   false) end

return M
