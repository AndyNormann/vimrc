local vim = vim
local o = vim.opt
local g = vim.g
local map = vim.keymap.set
local pack = vim.pack

g.mapleader = " "
g.localleader = " "

-- vim.cmd.colorscheme("retrobox")
--
-- vim.api.nvim_set_hl(0, 'Normal', {guibg=nil, ctermbg=nil})
-- vim.api.nvim_set_hl(0, 'NormalFloat', {guibg=nil, ctermbg=nil})
-- vim.api.nvim_set_hl(0, 'SignColumn', {guibg=nil, ctermbg=nil})

vim.api.nvim_create_autocmd("FileType", {
	pattern = { "cpp", "lua", "c", "make", "ts", "tsx", "typescript", "typescriptreact", "zig", "go", "odin" },
	callback = function()
		vim.treesitter.start()
	end,
})

pack.add({
	{ src = "https://github.com/echasnovski/mini.nvim" },
	-- { src = "https://github.com/neovim/nvim-lspconfig" },
})

vim.schedule(function()
	-- require("mini.pick").setup()
	-- require("mini.files").setup()
	-- require("mini.pairs").setup()
	require("mini.surround").setup()
	-- vim.lsp.enable({ "lua_ls", "clangd" })
end)

-- Options
o.laststatus = 0
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.undofile = true
o.mouse = "a"
o.backup = false
o.breakindent = true
o.ruler = false
o.signcolumn = "yes"
o.writebackup = false
o.shell = "fish"
o.number = false
o.winborder = "rounded"
o.pumborder = "rounded"
o.virtualedit = "block"
o.formatoptions = "qjl1"
o.updatetime = 60
o.timeoutlen = 300
o.cursorline = false
o.ignorecase = true
o.incsearch = true
o.infercase = true
o.smartcase = true
o.smartindent = true
o.completeopt = "noselect,menuone"
o.clipboard = "unnamedplus"
o.showmode = false

o.smoothscroll = true
o.shortmess = "IWATFsScnxC"
o.shortmess:append({ W = true, I = true, c = true, C = true })
o.termguicolors = true
o.pumheight = 15
o.conceallevel = 0
o.foldmethod = "expr"
o.foldexpr = "nvim_treesitter#foldexpr()"
o.foldlevel = 100
g.markdown_folding = 1

o.fillchars = "eob: "

vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")

map({ "n", "v" }, "j", "gj")
map({ "n", "v" }, "k", "gk")

map("n", ";", ":")
map("n", ":", ";")
-- map("n", "<Tab>", "za")

map({ "n", "v" }, "{", "15gk", { silent = true })
map({ "n", "v" }, "}", "15gj", { silent = true })
map("t", "<esc>", "<C-\\><C-n>", { silent = true })
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
map("i", "<C-CR>", "<C-y>", { silent = true })
-- map("n", "-", "<cmd>lua MiniFiles.open()<cr>")
