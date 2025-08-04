vim.loader.enable()
local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local pack = vim.pack

vim.g.mapleader = " "
vim.g.localleader = " "

map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")

map("n", ";", ":")
map("n", ":", ";")

map({ "n", "v" }, "{", "20gk", { silent = true })
map({ "n", "v" }, "}", "20gj", { silent = true })
map("t", "<esc>", "<C-\\><C-n>")

-- Options
opt.laststatus = 0
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.signcolumn = "yes"
opt.clipboard = "unnamedplus"
opt.completeopt = "fuzzy,noselect,menuone,popup"
opt.swapfile = false
opt.shell = "fish"
opt.winborder = "bold"
opt.complete = ".,o"

pack.add({
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/catppuccin/nvim" },
	{ src = "https://github.com/mattn/emmet-vim" },
	{ src = "https://github.com/rachartier/tiny-inline-diagnostic.nvim" },
	{ src = "https://github.com/ibhagwan/fzf-lua" },
	{ src = "https://github.com/davidosomething/format-ts-errors.nvim" },
	{ src = "https://github.com/stevearc/oil.nvim" },
	{ src = "https://github.com/ray-x/lsp_signature.nvim" },
	{ src = "https://github.com/dmtrKovalenko/fff.nvim" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	{ src = "https://github.com/echasnovski/mini.surround" },
	{ src = "https://github.com/echasnovski/mini.pairs" },
})

vim.cmd.colorscheme("catppuccin-mocha")
require("nvim-treesitter.configs").setup({
	highlight = { enable = true },
})

vim.schedule(function()
	require("fzf-lua").setup({})

	require("mason").setup()
	require("mason-lspconfig").setup()

	require("conform").setup({
		formatters_by_ft = {
			lua = { "stylua" },
			css = { "prettierd" },
			scss = { "prettierd" },
			typescript = { "prettierd" },
			typescriptreact = { "prettierd" },
			javascript = { "prettierd" },
			svelte = { "prettierd" },
			go = { "gofumpt" },
		},
		format_on_save = {
			timeout_ms = 150,
		},
	})
	require("lsp_signature").setup()
	require("oil").setup({
		default_file_explorer = true,
		view_options = {
			show_hidden = true,
		},
	})

	require("tiny-inline-diagnostic").setup()
	require("mini.surround").setup()
	require("mini.pairs").setup()
end)

-- Restore cursor to file position in previous editing session
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.cmd('normal! g`"zz')
		end
	end,
})

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args, bufnr)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		client.server_capabilities.semanticTokensProvider = nil
		vim.lsp.completion.enable(true, client.id, bufnr, {
			autotrigger = true,
		})
	end,
})

-- Keymaps
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>")
map("n", "<leader>e", "<cmd>FzfLua files<cr>")
map("n", "<leader>f", "<cmd>FzfLua live_grep<cr>")
map("n", "<leader>b", "<cmd>FzfLua buffers<cr>")
map("n", "<leader>r", "<cmd>FzfLua resume<cr>")
map("n", "-", "<cmd>Oil<cr>", { silent = true })
