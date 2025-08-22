vim.loader.enable()
local opt = vim.opt
local g = vim.g
local map = vim.keymap.set
local pack = vim.pack

g.mapleader = " "
g.localleader = " "

pack.add({
	{ src = "https://github.com/everviolet/nvim", name = "evergarden" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
})

require("nvim-treesitter.configs").setup({
	highlight = { enable = true },
})

require("evergarden").setup({
	theme = "winter",
	accent = "red",
	editor = {
		transparent_background = true,
	},
	color_overrides = {
		-- cherry = "#F7A182",
		base = "#000000",
		mantle = "#000000",
		crust = "#000000",

		-- base = "#12171a",
		-- mantle = "#12171a",
		-- crust = "#12171a",
		cherry = "#fddce3",
		pink = "#fddce3",
	},
})

vim.cmd.colorscheme("evergarden")
vim.cmd([[highlight Visual gui=reverse]])

pack.add({
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/nvim-lua/plenary.nvim" },
	{ src = "https://github.com/saghen/blink.cmp", version = "v1.3.1" },
	{ src = "https://github.com/rachartier/tiny-inline-diagnostic.nvim" },
	{ src = "https://github.com/nvim-telescope/telescope.nvim" },
	{ src = "https://github.com/nvim-telescope/telescope-fzy-native.nvim" },
	{ src = "https://github.com/davidosomething/format-ts-errors.nvim" },
	{ src = "https://github.com/stevearc/oil.nvim" },
	{ src = "https://github.com/echasnovski/mini.surround" },
	-- { src = "https://github.com/echasnovski/mini.pairs" },
	{ src = "https://github.com/L3MON4D3/LuaSnip" },
})

vim.schedule(function()
	require("telescope").setup({
		defaults = {
			border = true,
			borderchars = { "━", "┃", "━", "┃", "┏", "┓", "┛", "┗" },
		},
	})
	require("telescope").load_extension("fzy_native")

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
	require("oil").setup({
		default_file_explorer = true,
		view_options = {
			show_hidden = true,
		},
	})
	local ls = require("luasnip")
	ls.config.setup({})
	require("luasnip.loaders.from_vscode").lazy_load({ paths = { "./snippets" } })

	map("i", "<Tab>", function()
		if ls.expandable() then
			vim.schedule(function()
				vim.snippet.jump(1)
				ls.expand()
			end)
			return ""
		else
			return "\t"
		end
	end, { silent = true, expr = true })

	require("tiny-inline-diagnostic").setup({
		preset = "modern",
	})
	require("mini.surround").setup()
	-- require("mini.pairs").setup()
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
		if client == nil then
			return
		end
		client.server_capabilities.semanticTokensProvider = nil
		vim.lsp.document_color.enable()
		-- vim.lsp.completion.enable(true, client.id, 0, {
		-- 	autotrigger = true,
		-- })
	end,
})

-- Options
opt.laststatus = 1
opt.cmdheight = 1
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.signcolumn = "yes"
opt.clipboard = "unnamedplus"
opt.completeopt = "noselect,menuone,popup,nosort,fuzzy"
opt.swapfile = false
opt.shell = "fish"
opt.winborder = "bold"
opt.complete = ".,o"
opt.undofile = true
opt.pumheight = 8
opt.pumblend = 0
opt.showmode = false
opt.syntax = "off"
opt.cursorline = false
opt.shortmess = "aAcFInosStWx"

vim.diagnostic.config({ virtual_text = false })

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")

map({ "n", "v" }, "j", "gj")
map({ "n", "v" }, "k", "gk")

map("n", ";", ":")
map("n", ":", ";")
map("n", "<Tab>", "za")

map("i", "<c-cr>", "<c-y>")

map({ "n", "v" }, "{", "20gk", { silent = true })
map({ "n", "v" }, "}", "20gj", { silent = true })
map("t", "<esc>", "<C-\\><C-n>", { silent = true })
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
map("n", "<leader>e", "<cmd>Telescope find_files<cr>", { silent = true })
map("n", "<leader>f", "<cmd>Telescope live_grep<cr>", { silent = true })
map("n", "<leader>b", "<cmd>Telescope buffers<cr>", { silent = true })
map("n", "<leader>r", "<cmd>Telescope resume<cr>", { silent = true })
map("n", "-", "<cmd>Oil<cr>", { silent = true })
