if !has('python')
    echo "Error: Required vim compiled with +python"
    finish
endif

function! Email(name)

python << EOF

import vim

vim.command("new")
#vim.current.window.cursor = (1, 0) # move to top left
py_name = vim.eval("a:name")
vim.current.buffer[0] = "Hallo %s," % py_name
vim.current.buffer.append("")
vim.current.buffer.append("")
vim.current.buffer.append("")
vim.current.buffer.append("Gruß")
vim.current.buffer.append("")
vim.current.buffer.append("Rosto")

vim.current.window.cursor = (3, 0) # move to top left
vim.command("startinsert")

EOF

endfunction

