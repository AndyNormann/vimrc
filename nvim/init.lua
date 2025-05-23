vim.loader.enable()
local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local g = vim.g
local o = vim.o

local path_package = vim.fn.stdpath("data") .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
	vim.cmd('echo "Installing `mini.nvim`" | redraw')
	local clone_cmd = {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/echasnovski/mini.nvim",
		mini_path,
	}
	vim.fn.system(clone_cmd)
	vim.cmd("packadd mini.nvim | helptags ALL")
	vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

local deps = require("mini.deps")
deps.setup({ path = { package = path_package } })

local add = deps.add
local now = deps.now
local later = deps.later

add({
	source = "neovim/nvim-lspconfig",
	depends = { "mason-org/mason.nvim", "mason-org/mason-lspconfig.nvim", "stevearc/conform.nvim" },
})
add({ source = "catppuccin/nvim" })
add({ source = "rachartier/tiny-inline-diagnostic.nvim" })
add({
	source = "nvim-telescope/telescope.nvim",
	depends = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope-fzy-native.nvim" },
})
add({ source = "davidosomething/format-ts-errors.nvim" })
add({ source = "stevearc/oil.nvim" })
add({ source = "smoka7/hop.nvim" })
add({ source = "garymjr/nvim-snippets" })
add({
	source = "saghen/blink.cmp",
	checkout = "1.3.1",
})
add({
	source = "nvim-treesitter/nvim-treesitter",
	checkout = "master",
	monitor = "main",
	hooks = {
		post_checkout = function()
			vim.cmd("TSUpdate")
		end,
	},
})

now(function()
	vim.cmd.colorscheme("catppuccin-mocha")
	require("mini.basics").setup()
	-- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	-- vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })
	-- vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })

	require("mason").setup()
	require("mason-lspconfig").setup()

	require("nvim-treesitter.configs").setup({
		ensure_installed = { "lua", "vimdoc", "tsx", "c", "typescript" },
		highlight = {
			enable = true,
			use_languagetree = true,
			additional_vim_regex_highlighting = true,
		},
		indent = { enable = true },
	})

	local telescope = require("telescope")
	telescope.setup({
		defaults = {
			border = false,
		},
	})
	telescope.load_extension("fzy_native")
end)

later(function()
	require("conform").setup({
		formatters_by_ft = {
			lua = { "stylua" },
			css = { "prettierd" },
			scss = { "prettierd" },
			typescript = { "prettierd" },
			typescriptreact = { "prettierd" },
			javascript = { "prettierd" },
			svelte = { "prettierd" },
		},
		format_on_save = {
			timeout_ms = 150,
		},
	})

	require("hop").setup({})

	require("snippets").setup({
		create_autocommand = true,
		create_cmp_source = false,
		friendly_snippets = false,
		search_paths = {
			vim.fn.stdpath("config") .. "/snippets",
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

	require("tiny-inline-diagnostic").setup()
	require("oil").setup({
		default_file_explorer = true,
		view_options = {
			show_hidden = true,
		},
	})

	require("mini.surround").setup()
	require("mini.icons").setup()
	require("mini.notify").setup()
	require("mini.pairs").setup()
end)

g.mapleader = " "
g.maplocalleader = " "

-- Options
opt.laststatus = 0
opt.expandtab = true
opt.smarttab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.wrap = true
opt.clipboard = "unnamedplus"
opt.cursorline = false
opt.completeopt = "noselect,menuone,longest,popup,fuzzy"
o.winborder = "bold"

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

_G.cr_action = function()
	if vim.fn.complete_info()["selected"] ~= -1 then
		return "\25"
	end
	return "\r"
end

vim.keymap.set("i", "<CR>", "v:lua.cr_action()", { expr = true })

map({ "n" }, "<leader>e", "<cmd>lua require('telescope.builtin').find_files()<cr>")
map({ "n" }, "<leader>f", "<cmd>lua require('telescope.builtin').live_grep()<cr>")
map({ "n" }, "<leader>b", "<cmd>lua require('telescope.builtin').buffers()<cr>")
map({ "n" }, "<leader>r", "<cmd>lua require('telescope.builtin').resume()<cr>")
map({ "n" }, "S", "<cmd>HopWord<cr>")
map({ "i" }, "<Tab>", function()
	vim.cmd('echo "hello from tab handler"')
	if vim.snippet.active() then
		vim.cmd('echo "we went it"')
		vim.schedule(function()
			vim.snippet.jump(1)
		end)
		return
	end
	vim.cmd('echo "no we didnt"')
	return "<Tab>"
end, { silent = true, expr = true })
map({ "n" }, "-", "<cmd>Oil<cr>", { silent = true })
