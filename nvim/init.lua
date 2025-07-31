local vim = vim
vim.loader.enable()
local opt = vim.opt
local map = vim.keymap.set
local pack = vim.pack

pack.add({
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/catppuccin/nvim" },
	{ src = "https://github.com/mattn/emmet-vim" },
	{ src = "https://github.com/rachartier/tiny-inline-diagnostic.nvim" },
	{ src = "https://github.com/nvim-telescope/telescope.nvim" },
	{ src = "https://github.com/nvim-lua/plenary.nvim" },
	{ src = "https://github.com/nvim-telescope/telescope-fzy-native.nvim" },
	{ src = "https://github.com/davidosomething/format-ts-errors.nvim" },
	{ src = "https://github.com/stevearc/oil.nvim" },
	{ src = "https://github.com/saghen/blink.cmp", version = "v1.3.1" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
	{ src = "https://github.com/echasnovski/mini.basics" },
	{ src = "https://github.com/echasnovski/mini.surround" },
	{ src = "https://github.com/echasnovski/mini.pairs" },
})

vim.cmd.colorscheme("catppuccin-mocha")

-- Options
require("mini.basics").setup()
opt.laststatus = 0
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.clipboard = "unnamedplus"
opt.swapfile = false
opt.shell = "fish"
opt.winborder = "bold"

require("nvim-treesitter.configs").setup({
	ensure_installed = { "lua", "vimdoc", "tsx", "c", "typescript" },
	highlight = {
		enable = true,
		use_languagetree = true,
		additional_vim_regex_highlighting = true,
	},
	indent = { enable = true },
})

vim.schedule(function()
	local telescope = require("telescope")
	telescope.setup({
		defaults = {
			border = false,
		},
	})
	telescope.load_extension("fzy_native")

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
	require("blink-cmp").setup({
		completion = {
			accept = { auto_brackets = { enabled = true } },
			list = { selection = { preselect = false, auto_insert = true } },
			documentation = {
				auto_show = true,
				auto_show_delay_ms = 250,
				treesitter_highlighting = true,
			},
		},
		keymap = {
			["<C-CR>"] = { "accept", "fallback" },
			["<C-p>"] = { "select_prev", "fallback" },
			["<C-n>"] = { "select_next", "fallback" },
			["<C-up>"] = { "scroll_documentation_up", "fallback" },
			["<C-down>"] = { "scroll_documentation_down", "fallback" },
		},

		signature = { enabled = true },
		cmdline = { enabled = false },
		sources = {
			default = { "lsp", "path", "buffer" },
			providers = {
				lsp = {
					min_keyword_length = 0,
					score_offset = 9,
				},
				path = {
					score_offset = 10,
					min_keyword_length = 0,
				},
				buffer = {
					min_keyword_length = 2,
					score_offset = -5,
					max_items = 5,
				},
			},
		},
	})
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
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		client.server_capabilities.semanticTokensProvider = nil
	end,
})

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")

map("n", ";", ":")
map("n", ":", ";")

map({ "n", "v" }, "{", "20gk", { silent = true })
map({ "n", "v" }, "}", "20gj", { silent = true })
map("t", "<esc>", "<C-\\><C-n>")

map("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>")
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>")
map("n", "<leader>e", "<cmd>lua require('telescope.builtin').find_files()<cr>")
map("n", "<leader>f", "<cmd>lua require('telescope.builtin').live_grep()<cr>")
map("n", "<leader>b", "<cmd>lua require('telescope.builtin').buffers()<cr>")
map("n", "<leader>r", "<cmd>lua require('telescope.builtin').resume()<cr>")
map("n", "-", "<cmd>Oil<cr>", { silent = true })
