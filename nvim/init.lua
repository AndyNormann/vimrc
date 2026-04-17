vim.loader.enable()
local o = vim.opt
local g = vim.g
local map = vim.keymap.set
local pack = vim.pack

g.mapleader = " "
g.localleader = " "

---@param x string
---@return string
local gh = function(x)
	return "https://github.com/" .. x
end

pack.add({
	{ src = gh("catppuccin/nvim"), name = "catppuccin" },
	{ src = gh("nvim-mini/mini.nvim") },
	{ src = gh("saghen/blink.pairs"), version = "v0.4.1" },
	{ src = gh("SmiteshP/nvim-navic") },
	{ src = gh("nvim-treesitter/nvim-treesitter"), version = "main" },
	{ src = gh("neovim/nvim-lspconfig") },
	{ src = gh("mason-org/mason.nvim") },
	{ src = gh("mason-org/mason-lspconfig.nvim") },
	{ src = gh("stevearc/oil.nvim") },
	{ src = gh("stevearc/conform.nvim") },
}, {
	confirm = false,
})

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
		"svelte",
	},
	callback = function()
		vim.treesitter.start()
	end,
})

require("catppuccin").setup({
	flavour = "mocha",
	transparent_background = true,
	term_colors = true,
})
vim.cmd.colorscheme("catppuccin")

vim.cmd("highlight Visual gui=reverse cterm=reverse")

-- Options
o.undofile = true
o.splitkeep = "screen"
o.splitright = true
o.backup = false
o.writebackup = false
o.signcolumn = "yes"
o.fillchars = "eob: "
o.ignorecase = true
o.incsearch = true
o.infercase = true
o.smartcase = true
o.smartindent = true
o.mouse = "a"
o.showmode = false
o.ruler = false
o.laststatus = 0
o.cmdheight = 1
o.expandtab = true
o.tabstop = 2
o.shiftwidth = 2
o.autochdir = false
o.wrap = true
o.shell = "fish"
o.number = false
o.pumheight = 8
o.cursorline = false
o.updatetime = 20
o.timeoutlen = 300
o.scrolloff = 10
o.clipboard = "unnamedplus"
o.winborder = "bold"
o.pumborder = "bold"
o.shortmess = "IWATFcnxC"
o.completeopt = "menu,menuone,noselect"
o.complete = ".,b,o"
o.autocomplete = true
o.foldlevel = 99
o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
o.foldmethod = "expr"
o.indentexpr = "v:lua.vim.treesitter.indentexpr()"

g.neovide_hide_mouse_when_typing = true
g.neovide_cursor_animation_length = 0
g.neovide_scroll_animation_length = 0.05
g.neovide_show_border = false
g.neovide_window_blurred = false
vim.g.neovide_floating_shadow = false

require("mini.misc").setup_restore_cursor()

map("n", "<leader>e", "<cmd>lua MiniPick.builtin.files()<cr>")
map("n", "<leader>f", "<cmd>lua MiniPick.builtin.grep_live()<cr>")
map("n", "<leader>b", "<cmd>lua MiniPick.builtin.buffers()<cr>")
map("n", "<leader>r", "<cmd>lua MiniPick.builtin.resume()<cr>")
map("n", "<leader>h", "<cmd>lua MiniPick.builtin.help()<cr>")

require("mini.pick").setup({
	window = {
		config = {
			width = math.floor(0.718 * vim.o.columns),
		},
	},
	mappings = {
		choose_marked = "<C-cr>",
	},
})

vim.schedule(function()
	map("n", "-", "<cmd>Oil<cr>", { silent = true })
	map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
	map({ "n", "v" }, "0", "^")

	map({ "n", "v" }, "j", "gj")
	map({ "n", "v" }, "k", "gk")

	map("n", ";", ":")
	map("n", ":", ";")

	map({ "n", "v" }, "{", "<C-u>", { silent = true })
	map({ "n", "v" }, "}", "<C-d>", { silent = true })
	map("t", "<esc>", "<C-\\><C-n>", { silent = true })

	map("i", "<C-k>", vim.lsp.buf.signature_help, { desc = "LSP Signature Help" })
	map("n", "gd", "<cmd>lua vim.lsp.buf.implementation()<cr>", { silent = true })
	vim.keymap.set(
		"n",
		"gf",
		"<cmd>lua vim.lsp.buf.code_action({ filter = function(action) return action.isPreferred end, apply = true })<cr>",
		{ silent = true, noremap = true }
	)
	map("i", "<C-l>", "<cmd>lua vim.lsp.inline_completion.get()<cr>", { noremap = true, silent = true })
	map("i", "<C-CR>", "<C-y>", { noremap = true, silent = true })

	vim.api.nvim_create_user_command("Breadcrumbs", function()
		local navic = require("nvim-navic")
		print(navic.get_location())
	end, {})
	vim.api.nvim_create_autocmd("LspProgress", {
		callback = function(ev)
			local value = ev.data.params.value or {}
			local msg = value.message or "done"

			-- rust analyszer in particular has really long LSP messages so truncate them
			if #msg > 40 then
				msg = msg:sub(1, 37) .. "..."
			end

			-- :h LspProgress
			vim.api.nvim_echo({ { msg } }, false, {
				id = "lsp",
				kind = "progress",
				title = value.title,
				source = value.kind,
				status = value.kind ~= "end" and "running" or "success",
				percent = value.percentage,
			})
		end,
	})

	vim.lsp.enable("copilot")
	require("mini.extra").setup()
	require("mini.surround").setup({})

	require("mason").setup()
	require("mason-lspconfig").setup()
	require("oil").setup({
		default_file_explorer = true,
		show_hidden = true,
	})

	require("conform").setup({
		format_on_save = {
			timeout_ms = 500,
			lsp_format = "fallback",
		},
		formatters_by_ft = {
			typescript = { "prettierd", "prettier", stop_after_first = true },
			typescriptreact = { "prettierd", "prettier", stop_after_first = true },
			sass = { "prettierd", "prettier", stop_after_first = true },
			css = { "prettierd", "prettier", stop_after_first = true },
			scss = { "prettierd", "prettier", stop_after_first = true },
			lua = { "stylua" },
			go = { "gofmt" },
		},
	})

	require("blink.pairs").setup({})

	local snip = require("mini.snippets")
	local gen_loader = snip.gen_loader
	snip.setup({
		snippets = { gen_loader.from_lang() },
	})

	map("i", "<Tab>", function()
		if snip.expand({ insert = false }) ~= nil then
			vim.schedule(function()
				snip.expand()
				snip.session.stop()
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
	vim.api.nvim_create_user_command("ShowNesting", function()
		local current_node = vim.treesitter.get_node({})
		local path = {}

		while current_node do
			print(current_node:type())
			-- We look for nodes that typically represent keys or identifiers
			if current_node:type() == "pair" or current_node:type() == "field" then
				-- This logic varies slightly by language, but usually the first child is the key
				local key_node = current_node:child(0)
				if key_node then
					table.insert(path, 1, vim.treesitter.get_node_text(key_node, 0))
				end
			end
			current_node = current_node:parent()
		end

		if #path > 0 then
			print("Nesting: " .. table.concat(path, " > "))
		else
			print("No nesting found (or not in a recognized object)")
		end
	end, {})

	require("vim._core.ui2").enable({
		enable = true,
		msg = {
			targets = {
				[""] = "msg",
				empty = "cmd",
				bufwrite = "msg",
				confirm = "cmd",
				emsg = "pager",
				echo = "msg",
				echomsg = "msg",
				echoerr = "pager",
				completion = "cmd",
				list_cmd = "pager",
				lua_error = "pager",
				lua_print = "msg",
				progress = "pager",
				rpc_error = "pager",
				quickfix = "msg",
				search_cmd = "cmd",
				search_count = "cmd",
				shell_cmd = "pager",
				shell_err = "pager",
				shell_out = "pager",
				shell_ret = "msg",
				undo = "msg",
				verbose = "pager",
				wildlist = "cmd",
				wmsg = "msg",
				typed_cmd = "cmd",
			},
			cmd = {
				height = 0.5,
			},
			dialog = {
				height = 0.5,
			},
			msg = {
				height = 0.3,
				timeout = 5000,
			},
			pager = {
				height = 0.5,
			},
		},
	})

	vim.lsp.document_color.enable()
	vim.lsp.inline_completion.enable(true)
	vim.lsp.semantic_tokens.enable(false)
	vim.diagnostic.config({
		underline = true,
		update_in_insert = false,
		virtual_text = {
			current_line = true,
			severity = vim.diagnostic.severity.WARN,
			spacing = 2,
		},
	})

	vim.api.nvim_create_autocmd("LspAttach", {
		callback = function(args)
			local client = vim.lsp.get_client_by_id(args.data.client_id)
			if not client then
				return
			end

			if client:supports_method("textDocument/completion") then
				vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
			end
			if client.server_capabilities.documentSymbolProvider then
				require("nvim-navic").attach(client, args.buf)
			end
		end,
	})
end)
