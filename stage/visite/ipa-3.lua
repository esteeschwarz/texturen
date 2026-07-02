-- commands.lua
-- Combined filter: YAML vars injection + IPA font switching.
--
-- Quarto processes metadata before Lua filters run, so metadata
-- arrives as plain Lua tables with {t=..., c=...} fields rather
-- than Pandoc API objects. We walk those tables directly.
--
-- YAML key   -> LaTeX command
-- title      -> \doctitle
-- subtitle   -> \docsubtitle
-- author     -> \docauthor
-- date       -> \docdate
-- cmeta      -> \docmeta   (rich: links become \protect\href)
-- cfoot      -> \docfoot   (rich: links become \protect\href)
-- logoleft   -> \doclogoleft
-- logoright  -> \doclogoright


-- ---------------------------------------------------------------------------
-- IPA helpers
-- ---------------------------------------------------------------------------

local function is_ipa(cp)
  return (cp >= 0x0250 and cp <= 0x02FF)
      or (cp >= 0x1D00 and cp <= 0x1D7F)
end

local function has_ipa(s)
  for _, cp in utf8.codes(s) do
    if is_ipa(cp) then return true end
  end
  return false
end

local function split_segments(s)
  local segments = {}
  local current = ""
  local current_is_ipa = nil
  for _, cp in utf8.codes(s) do
    local char = utf8.char(cp)
    local this_is_ipa = is_ipa(cp)
    if current_is_ipa == nil then current_is_ipa = this_is_ipa end
    if this_is_ipa ~= current_is_ipa then
      table.insert(segments, { text = current, ipa = current_is_ipa })
      current = ""
      current_is_ipa = this_is_ipa
    end
    current = current .. char
  end
  if current ~= "" then
    table.insert(segments, { text = current, ipa = current_is_ipa })
  end
  return segments
end


-- ---------------------------------------------------------------------------
-- AST table walker
-- Quarto flattens metadata into plain {t, c} Lua tables before filters run.
-- We recurse through these directly to produce LaTeX strings.
-- ---------------------------------------------------------------------------

local pandoc = require("pandoc")

local function get_url_dep(meta)
  local para = meta[1]
  local out = {}

  io.stderr:write("\n--- cmeta[1].t=" .. para.t .. " ---\n")
--   io.stderr:write("\n--- cmeta[1].c=" .. para .. " ---\n")

--   if para and para.t == "Para" then
    for _, el in ipairs(para.content) do
    --   if el.t == "Quoted" and el.content and el.content[1].t == "Link" then
        local link1 = el.content[1]
        local link2 = el.content[2]
        local link3 = el.content[3]
        io.stderr:write("\n --- link1: " .. tostring(link1.content[1]) .. " --- \n")
        io.stderr:write("\n --- link2: " .. tostring(link1.content) .. " --- \n")
        io.stderr:write("\n --- link3: " .. tostring(link1.content[3]) .. " --- \n")

        -- if link.target and link.target ~= "" then
          -- Return the URL as a raw LaTeX inline element
        table.insert(out,"\\protect\\href{" .. link1 .."}{nothing}")
        --   return linkc
        --   return pandoc.MetaValue(pandoc.RawInline("latex", "\\url{" .. link.target .. "}"))
    end
    --   end
    -- end
--   end
  return out
end

local pandoc = require("pandoc")

function get_url(meta)
  local para = meta[1]
  local out = {}

  io.stderr:write("\n---GETURL() cmeta[1].t=" .. para.t .. " ---\n")
  if para and para.t == "Para" then
    for _, el in ipairs(para.content) do
          io.stderr:write("\n---GETURL() pairs.el.t=" .. el.t .. " ---\n")

    --   if el.t ~= "Quoted" and el.content and el.content[1].t == "Link" then
      if el.t == "Link" and el.content then
         io.stderr:write("\n---GETURL() pairs.el.content[1]=" .. el.content[1].t .. " ---\n")
         local link = el
        if link.target and link.target ~= "" then
        io.stderr:write("\n--- link.target=" .. link.target .. " ---\n")
        io.stderr:write("\n--- link.text=" .. pandoc.utils.stringify(link.content) .. " ---\n")
          -- Return the URL as a raw LaTeX inline element
        --   return pandoc.MetaValue(pandoc.RawInline("latex", "\\url{" .. link.target .. "}"))
        --   return {url = "\\url{" .. link.target .. "}",text = pandoc.utils.stringify(link.content)}
         return {"\\href{"  .. link.target .. "}{\\textcolor{white}{".. pandoc.utils.stringify(link.content) .. "}}"}
        end
      end
    end
  end
  return nil
end

function get_paper_url(meta)
  local para = meta[1]
  local out = {}

  io.stderr:write("\n---GETURL() cmeta[1].t=" .. para.t .. " ---\n")
  if para and para.t == "Para" then
    for _, el in ipairs(para.content) do
          io.stderr:write("\n---GETURL() pairs.el.t=" .. el.t .. " ---\n")

    --   if el.t ~= "Quoted" and el.content and el.content[1].t == "Link" then
      if el.t == "Link" and el.content then
        
         io.stderr:write("\n---GETURL() pairs.el.content[1]=" .. el.content[1].t .. " ---\n")
         local link = el
        if link.target and link.target ~= "" then
          if link.content == "general" then 
          link.content = link.target
        end
        io.stderr:write("\n--- link.target=" .. link.target .. " ---\n")
        io.stderr:write("\n--- link.text=" .. pandoc.utils.stringify(link.content) .. " ---\n")
          -- Return the URL as a raw LaTeX inline element
          -- return pandoc.MetaValue(pandoc.RawInline("latex", "\\url{" .. link.target .. "}"))
        --   return {url = "\\url{" .. link.target .. "}",text = pandoc.utils.stringify(link.content)}
        --  return {"\\href{"  .. link.target .. "}{{".. link.target .. "}}"}
        return {"\\url{" .. link.target .. "}"}
        end
      end
    end
  end
  return nil
end

local function walk_inlines_dep(inlines)
  local out = {}
  for i = 1, #inlines do
    local el = inlines[i]
    if el == nil then break end
    local t = el.t
    if t == "Str" then
      table.insert(out, el.c)
    elseif t == "Space" then
      table.insert(out, " ")
    elseif t == "SoftBreak" or t == "LineBreak" then
      table.insert(out, " \\newline ")
    elseif t == "Quoted" then
      table.insert(out, table.concat(walk_inlines(el.c[2])))
    elseif t == "Link" then
      local content = el.c[2]
      local target  = el.c[3]
      local url     = type(target) == "table" and target[1] or tostring(target)
      local text    = table.concat(walk_inlines(content))
      url = url:gsub("%%", "\\%%")
      table.insert(out, "\\protect\\href{" .. url .. "}{" .. text .. "}")
    elseif t == "Emph" then
      table.insert(out, "\\emph{" .. table.concat(walk_inlines(el.c)) .. "}")
    elseif t == "Strong" then
      table.insert(out, "\\textbf{" .. table.concat(walk_inlines(el.c)) .. "}")
    elseif t == "Code" then
      table.insert(out, "\\texttt{" .. el.c[2] .. "}")
    elseif t == "RawInline" and el.c[1] == "latex" then
      table.insert(out, el.c[2])
    end
  end
  return out
end

local function walk_blocks(blocks)
  local parts = {}
  for i = 1, #blocks do
    local block = blocks[i]
    if block == nil then break end
    local t = block.t
    if t == "Para" or t == "Plain" then
      table.insert(parts, table.concat(walk_inlines(block.c)))
    end
  end
  return parts
end
local function walk_inlines_dep(inlines)
  local out = {}
  for _, el in ipairs(inlines) do
    local t = el.t
    if t == "Str" then
      table.insert(out, el.c)
    elseif t == "Space" then
      table.insert(out, " ")
    elseif t == "SoftBreak" or t == "LineBreak" then
      table.insert(out, " \\newline ")
    elseif t == "Link" then
      -- Pandoc Link AST: c = { attr, [inlines], {url, title} }
      local content = el.c[2]
      local target  = el.c[3]
      local url     = type(target) == "table" and target[1] or tostring(target)
      local text    = table.concat(walk_inlines(content))
      url = url:gsub("%%", "\\%%")
      table.insert(out, "\\protect\\href{" .. url .. "}{" .. text .. "}")
    elseif t == "Emph" then
      table.insert(out, "\\emph{" .. table.concat(walk_inlines(el.c)) .. "}")
    elseif t == "Strong" then
      table.insert(out, "\\textbf{" .. table.concat(walk_inlines(el.c)) .. "}")
    elseif t == "Code" then
      table.insert(out, "\\texttt{" .. el.c[2] .. "}")
    elseif t == "RawInline" and el.c[1] == "latex" then
      table.insert(out, el.c[2])
    end
  end
  return out
end

local function walk_blocks_dep(blocks)
  local parts = {}
  for _, block in ipairs(blocks) do
    local t = block.t
    if t == "Para" or t == "Plain" then
      table.insert(parts, table.concat(walk_inlines(block.c)))
    end
  end
  return parts
end


-- ---------------------------------------------------------------------------
-- Injection helpers
-- ---------------------------------------------------------------------------

local function is_inline_list(value)
  -- a flat list of inlines has no .t on the list itself, but elements have .t
  -- and the first element is an inline node (Str, Link, Space etc.)
  if type(value) ~= "table" then return false end
  local first = value[1]
  if type(first) ~= "table" then return false end
  local t = first.t
  return t == "Str" or t == "Link" or t == "Space" or t == "Emph"
      or t == "Strong" or t == "Code" or t == "RawInline"
end



local function inject_rich(includes, cmd, value)
  io.stderr:write("cmeta type=" .. type(value) .. "\n")

-- if it's a table, print the first element's t field:
if type(value) == "table" then
  local first = value[1]
  if type(first) == "table" then
    io.stderr:write("cmeta[1].t=" .. tostring(first.t) .. "\n")
    io.stderr:write("cmeta[1].c type=" .. type(first.c) .. "\n")
  else
    io.stderr:write("cmeta[1] is " .. type(first) .. "=" .. tostring(first) .. "\n")
  end
end
  if value == nil or type(value) ~= "table" then return end
includes:insert(pandoc.RawBlock("latex",
    "% DEBUG " .. cmd .. " type=" .. tostring(value.t)))
  io.stderr:write("META KEY: " .. tostring(value))

  local latex
  if is_inline_list(value) then
    -- flat list of inlines (e.g. params: quoted single-line value)
    -- latex = table.concat(walk_inlines(value))
    latex = table.concat(get_url(value))

else
    -- block list (e.g. | block scalar -> Para nodes)
    local parts = get_url(value)
    latex = table.concat(parts, " \\newline ")
  end

  latex = latex:gsub("^%s+", ""):gsub("%s+$", "")
  if latex ~= "" then
    includes:insert(pandoc.RawBlock("latex",
      "\\newcommand{\\" .. cmd .. "}{" .. latex .. "}"))
  end
end

local function inject_meta(includes, cmd, value)
  if value == nil then return end
  value = value[1]
  includes:insert(pandoc.RawBlock("latex",
    "% DEBUG " .. cmd .. " type=" .. tostring(value.t)))
  io.stderr:write("META KEY: " .. tostring(value.t) .. "\n")
  io.stderr:write("META KEY: " .. tostring(value) .. "\n")

  -- MetaBlocks: walk each block, convert inlines to LaTeX
  local blocks
  if value.t == "MetaBlocks" then
    blocks = value
  elseif value.t == "Para" then
    blocks = value
  elseif value.t == "MetaInlines" then
    blocks = pandoc.Blocks({ pandoc.Para(value) })
  else
    -- fallback to plain stringify
    inject(includes, cmd, value)
    return
  end

  -- Convert blocks to LaTeX via Pandoc writer
-- local doc = pandoc.Pandoc(blocks, PANDOC_STATE.meta)

-- local latex = pandoc.write(
--   doc,
--   "latex",
--   PANDOC_STATE.writer_options
-- )

-- latex = latex:gsub("\\href", "\\protect\\href")
-- latex = latex:gsub("\\url",  "\\protect\\url")

-- latex = latex:gsub("^%s*\\par%s*", "")
--              :gsub("%s*\\par%s*$", "")
--              :gsub("^%s+", "")
--              :gsub("%s+$", "")
--                             :gsub("anarkkiv", "ada")

  local doc = pandoc.Pandoc(blocks)
  local latex = pandoc.write(doc, "latex")
  latex = latex:gsub("\\href", "\\protect\\href")
  -- Strip surrounding \par / newlines Pandoc adds around blocks
  latex = latex:gsub("^%s*\\par%s*", "")
               :gsub("%s*\\par%s*$", "")
               :gsub("^%s+", "")
               :gsub("%s+$", "")
                                           :gsub("anarkkiv", "ada")

              --  :gsub("\\href(%b{})(%b{})", "\\mbox{\\href%1%2}")

  if latex ~= "" then
    includes:insert(pandoc.RawBlock("latex",
      "\\newcommand{\\" .. cmd .. "}{" .. latex .. "}"))
  end
end

local function inject_plain(includes, cmd, value)
  if value == nil or type(value) ~= "table" then return end
    io.stderr:write("PLAIN():META VALUE: " .. tostring(value) .. "\n")

  -- for plain fields, walk_blocks then stringify without link markup
  local str = pandoc.utils.stringify(value)
  if str == "" then
    local parts = walk_blocks(value)
    str = table.concat(parts, " ")
  end
  str = str:gsub("^%s+", ""):gsub("%s+$", "")
  if str ~= "" then
    includes:insert(pandoc.RawBlock("latex",
      "\\newcommand{\\" .. cmd .. "}{" .. str .. "}"))
  end
end


-- ---------------------------------------------------------------------------
-- Filter entry points
-- ---------------------------------------------------------------------------

function Meta(m)
  local includes = m["header-includes"] or pandoc.List()
  if m.title then
  inject_plain(includes, "doctitle",    m.title)
  end
  if m.subtitle then

  inject_plain(includes, "docsubtitle", m.subtitle)
  end
  if m.paperlink then

  inject_plain(includes, "docpaperlink", get_paper_url(m.paperlink))
  end
    if m.date then
  inject_plain(includes, "docdate",     m.date)
    end
      if m.cmeta then

  inject_meta( includes, "docmeta",     m.cmeta)
      end
        if m.cfoot then

  inject_plain( includes, "docfoot",   get_url(m.cfoot))
        end
          if m.cfootli then

  inject_plain( includes, "docfootli",   get_url(m.cfootli))
        end
          if m.logoleft then

  inject_plain(includes, "doclogoleft", m.logoleft)
          end
            if m.logoright then

  inject_plain(includes, "doclogoright",m.logoright)
            end
  if m.author then
    local ok, str = pcall(pandoc.utils.stringify, m.author)
    if ok and str ~= "" then
      includes:insert(pandoc.RawBlock("latex",
        "\\newcommand{\\docauthor}{" .. str .. "}"))
    end
  end

  m["header-includes"] = includes
  return m
end


function Str(el)
  if not (FORMAT:match("latex") or FORMAT:match("pdf")) then return el end
  if not has_ipa(el.text) then return el end
  if FORMAT:match("md") then return el end
  local segments = split_segments(el.text)
  local inlines  = pandoc.List()
  for _, seg in ipairs(segments) do
    if seg.ipa then
      inlines:insert(pandoc.RawInline("latex",
        "{\\ipafont " .. seg.text .. "}"))
    else
      inlines:insert(pandoc.Str(seg.text))
    end
  end
  if #inlines == 1 then return inlines[1] end
  return inlines
end