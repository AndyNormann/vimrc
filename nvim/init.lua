vim.loader.enable()
local o = vim.opt
local g = vim.g
local map = vim.keymap.set
local pack = vim.pack

g.mapleader = " "
g.localleader = " "

pack.add({
	{ src = "https://github.com/cpea2506/one_monokai.nvim" },
	{ src = "https://github.com/folke/snacks.nvim" },
	{ src = "https://github.com/esmuellert/codediff.nvim" },
	{ src = "https://github.com/MunifTanjim/nui.nvim" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
	{ src = "https://github.com/echasnovski/mini.nvim" },
	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/mason-org/mason.nvim" },
	{ src = "https://github.com/mason-org/mason-lspconfig.nvim" },
	{ src = "https://github.com/stevearc/oil.nvim" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/L3MON4D3/LuaSnip" },
	{ src = "https://github.com/stevearc/overseer.nvim" },
	{ src = "https://github.com/Saghen/blink.cmp", version = "v1.7.0" },
	{ src = "https://github.com/hedyhli/outline.nvim" },
	{ src = "https://github.com/folke/sidekick.nvim" },
	{ src = "https://github.com/folke/lazydev.nvim" },
	{ src = "https://github.com/j-hui/fidget.nvim" },
	{ src = "https://github.com/mattn/emmet-vim" },
}, {
	confirm = false,
})

require("mini.deps").now(function()
	vim.api.nvim_create_autocmd("FileType", {
		pattern = {
			"cpp",
			"lua",
			"c",
			"make",
			"ts",
			"tsx",
			"typescript",
			"typescriptreact",
			"zig",
			"go",
			"odin",
			"rust",
		},
		callback = function()
			vim.treesitter.start()
		end,
	})

	require("one_monokai").setup({
		transparent = true,
		colors = {
			fg = "#dcdfe4",
			white = "#dcdfe4",
		},
	})

	vim.cmd("highlight Visual gui=reverse cterm=reverse")

	require("snacks").setup({
		bigfile = { enabled = true },
		explorer = { enabled = true },
		input = { enabled = true },
		picker = { enabled = true },
		notifier = { enabled = true },
		quickfile = { enabled = true },
	})

	require("mini.pick").setup({
		mappings = {
			choose_marked = "<C-CR>",
		},
	})

	require("mini.misc").setup_restore_cursor()

	-- Options
	require("mini.basics").setup({})
	o.laststatus = 0
	o.cmdheight = 1
	o.tabstop = 2
	o.shiftwidth = 2
	o.autochdir = false
	o.wrap = true
	o.number = false
	o.pumheight = 8
	o.cursorline = false
	o.updatetime = 20
	o.timeoutlen = 200
	o.scrolloff = 999
	o.clipboard = "unnamedplus"
	o.winborder = "rounded"
	o.pumborder = "rounded"
	o.shortmess = "IWATFsScnxC"

	g.loaded_2html_plugin = 1
	g.loaded_gzip = 1
	g.loaded_man = 1
	g.loaded_tarPlugin = 1
	g.loaded_zipPlugin = 1
	g.loaded_remote_plugins = 1
	g.loaded_editorconfig = 1
	g.loaded_netrw = 1
	g.loaded_netrwPlugin = 1
	g.loaded_tutor = 1

	map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
	map({ "n", "v" }, "0", "^")

	map({ "n", "v" }, "j", "gj")
	map({ "n", "v" }, "k", "gk")

	map("n", ";", ":")
	map("n", ":", ";")

	map({ "n", "v" }, "{", "<C-u>", { silent = true })
	map({ "n", "v" }, "}", "<C-d>", { silent = true })
	map("t", "<esc>", "<C-\\><C-n>", { silent = true })
	map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
	vim.keymap.set(
		"n",
		"gf",
		"<cmd>lua vim.lsp.buf.code_action({ filter = function(action) return action.isPreferred end, apply = true })<cr>",
		{ silent = true, noremap = true }
	)
	map("i", "<C-l>", "<cmd>lua vim.lsp.inline_completion.get()<cr>", { noremap = true, silent = true })

	map("n", "-", "<cmd>Oil<cr>", { silent = true })

	map("n", "<leader>e", "<cmd>lua Snacks.picker.files()<cr>")
	map("n", "<leader>f", "<cmd>lua Snacks.picker.grep()<cr>")
	map("n", "<leader>b", "<cmd>lua Snacks.picker.buffers()<cr>")
	map("n", "<leader>r", "<cmd>lua Snacks.picker.resume()<cr>")
	map("n", "<leader>h", "<cmd>lua Snacks.picker.help()<cr>")
end)

require("mini.deps").later(function()
	require("fidget").setup({
		progress = {
			display = {
				render_limit = 4,
				done_ttl = 1,
				progress_icon = { pattern = "bouncing_bar", period = 4 },
			},
		},
	})

	vim.g.copilot_nes_debounce = 500
	vim.g.sidekick_nes = true
	vim.lsp.enable("copilot")

	require("vim._extui").enable({
		enable = true,
		msg = {
			target = "msg",
			timeout = 4000,
		},
	})

	o.statusline = "%f %m"

	require("sidekick").setup({
		cli = {
			win = {
				layout = "float",
			},
		},
	})

	map("n", "grp", require("sidekick").nes_jump_or_apply, { silent = true })
	map("n", "<leader>c", function()
		require("sidekick.cli").toggle("claude")
	end, { silent = true })

	require("mason").setup()
	require("mason-lspconfig").setup()

	require("outline").setup({})

	require("conform").setup({
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "never",
		},
		formatters_by_ft = {
			typescript = { "prettierd", "prettier", stop_after_first = true },
			typescriptreact = { "prettierd", "prettier", stop_after_first = true },
			sass = { "prettierd", "prettier", stop_after_first = true },
			css = { "prettierd", "prettier", stop_after_first = true },
			scss = { "prettierd", "prettier", stop_after_first = true },
			go = { "gofumpt" },
			lua = { "stylua" },
			rust = { "rustfmt" },
		},
	})

	require("overseer").setup({
		templates = { "builtin" },
	})

	vim.api.nvim_create_user_command("OverseerRunLast", function()
		local overseer = require("overseer")
		local tasks = overseer.list_tasks({ recent_first = true })
		if vim.tbl_isempty(tasks) then
			vim.notify("No tasks found", vim.log.levels.WARN)
		else
			overseer.run_action(tasks[1], "restart")
		end
	end, {})
	map("n", "<leader>m", "<cmd>OverseerRunLast<cr>", { silent = true })
	map("n", "<leader>n", "<cmd>OverseerRun<cr>", { silent = true })
	map("n", "<leader>t", "<cmd>OverseerToggle<cr>", { silent = true })

	local ls = require("luasnip")
	ls.config.setup({})
	require("luasnip.loaders.from_vscode").lazy_load({ paths = { "./snippets" } })

	map("i", "<Tab>", function()
		if ls.expand_or_jumpable() then
			vim.schedule(function()
				ls.expand_or_jump()
			end)
			return ""
		else
			return "\t"
		end
	end, { silent = true, expr = true })

	require("oil").setup({
		default_file_explorer = true,
		view_options = {
			show_hidden = true,
		},
	})
	require("mini.surround").setup()
	require("mini.pairs").setup()
	require("mini.extra").setup()
	require("mini.cmdline").setup()

	require("lazydev").setup()
	require("blink.cmp").setup({
		completion = {
			documentation = { auto_show = true },
			list = { selection = { preselect = false, auto_insert = true } },
		},
		sources = {
			default = { "lazydev", "lsp", "path", "buffer" },
			providers = {
				lazydev = { name = "LazyDev", module = "lazydev.integrations.blink", score_offset = 100 },
			},
		},
		cmdline = { enabled = false },
		signature = { enabled = true, window = { border = "bold" } },
		fuzzy = { implementation = "prefer_rust_with_warning" },
		keymap = { ["<C-CR>"] = { "select_and_accept", "fallback" } },
	})

	vim.lsp.document_color.enable()
	vim.lsp.inline_completion.enable(true)
	vim.lsp.semantic_tokens.enable(false)
	vim.diagnostic.config({
		underline = true,
		virtual_text = {
			current_line = true,
			severity = vim.diagnostic.severity.WARN,
			spacing = 2,
		},
	})
end)
