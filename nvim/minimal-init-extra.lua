local vim = vim
local o = vim.opt
local g = vim.g
-- local pack = vim.pack

g.mapleader = " "
g.localleader = " "

-- vim.api.nvim_create_autocmd("FileType", {
-- 	pattern = { "cpp", "lua", "c", "make", "ts", "tsx", "typescript", "typescriptreact", "zig", "go", "odin" },
-- 	callback = function()
-- 		vim.treesitter.start()
-- 	end,
-- })

-- pack.add({
-- { src = "https://github.com/echasnovski/mini.nvim" },
-- { src = "https://github.com/catppuccin/nvim" },
-- { src = "https://github.com/neovim/nvim-lspconfig" },
-- })

-- vim.cmd.colorscheme("catppuccin-mocha")

-- vim.schedule(function()
-- 	vim.lsp.enable({ "clangd", "lua-ls" })
-- 	vim.lsp.semantic_tokens.enable(false)
-- 	vim.diagnostic.config({
-- 		underline = true,
-- 		virtual_text = {
-- 			current_line = true,
-- 			severity = vim.diagnostic.severity.WARN,
-- 			spacing = 2,
-- 		},
-- 	})
-- end)
local map = vim.keymap.set

map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map("n", ";", ":")
map("n", ":", ";")
map({ "n", "v" }, "j", "gj")
map({ "n", "v" }, "k", "gk")
map("n", "gd", vim.lsp.buf.definition, { silent = true, buffer = true })
map({ "n", "v" }, "{", "<C-u>", { silent = true })
map({ "n", "v" }, "}", "<C-d>", { silent = true })

-- vim.keymap.set("i", "<C-k>", vim.lsp.buf.signature_help, { silent = true, buffer = true })

-- o.complete = "o,.,w,b,u"
-- o.completeopt = "fuzzy,menuone,noselect,popup" -- add 'popup' for docs (sometimes)

-- Options
o.laststatus = 0
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.undofile = true
o.mouse = "a"
o.wrap = true
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
o.shortmess = "IWATFsScnxC"
o.termguicolors = true
