local function fix_table_footer(text)
return text:gsub("\begin{tabular}[t]{lcccc}", function(prefix, content, suffix)
  if content:find("textbackslash{}num") then
  io.stderr:write("Fixing table footer: " .. content .. "\n")
  return prefix .. "* p < 0.1, ** p < 0.05, *** p < 0.01" .. suffix
  else
    return prefix .. content .. suffix
  end
  end)
end

function RawBlock(el)
if el.format:match("tex") or el.format:match("latex") then
local modified = fix_table_footer(el.text)
if modified ~= el.text then
io.stderr:write("Modified RawBlock text.\n")
el.text = modified
return el
end
end
end
