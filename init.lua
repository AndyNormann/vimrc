vim.loader.enable()

local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local g = vim.g
local o = vim.o
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api

g.mapleader = " "
g.maplocalleader = " "

opt.background = "dark"
cmd.colorscheme("retrobox")
api.nvim_set_hl(0, "Normal", { bg = "none" })
api.nvim_set_hl(0, "NormalNC", { bg = "none" })
api.nvim_set_hl(0, "SignColumn", { bg = "none" })

-- Plugins
-- Setup Lazy vim

local lazypath = fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.8",
		event = "VeryLazy",
		enabled = not g.vscode,
		dependencies = { "nvim-lua/plenary.nvim" },
		keys = {
			{ "<leader>e", "<cmd>lua require('telescope.builtin').find_files()<cr>" },
			{ "<leader>f", "<cmd>lua require('telescope.builtin').live_grep()<cr>" },
			{ "<leader>b", "<cmd>lua require('telescope.builtin').buffers()<cr>" },
			{ "<leader>r", "<cmd>lua require('telescope.builtin').resume()<cr>" },
			{ "<leader>s", "<cmd>lua require('telescope.builtin').lsp_document_symbols()<cr>" },
			{ "gd", "<cmd>lua require('telescope.builtin').lsp_definitions()<cr>" },
			{ "gr", "<cmd>lua require('telescope.builtin').lsp_references()<cr>" },
		},
	},
	-- {
	-- 	"voldikss/vim-floaterm",
	-- 	event = "VeryLazy",
	-- 	enabled = not g.vscode,
	-- 	keys = {
	-- 		{ "<Leader>t", "<cmd>FloatermToggle<cr>", { silent = true } },
	-- 		{ "t", "<Leader>t", "<C-\\><C-n><cmd>FloatermToggle<cr>", { silent = true } },
	-- 		{ "<Leader>T", "<cmd>FloatermNew<cr>", { silent = true } },
	-- 		{ "<Leader>n", ":FloatermSend " },
	-- 		{ "<Leader>m", ":FloatermSend<up><cr>" },
	-- 	},
	-- 	config = function()
	-- 		g.floaterm_wintype = "float"
	-- 		g.floaterm_autoclose = 1
	-- 		g.floaterm_height = 0.3
	-- 		g.floaterm_width = 0.4
	-- 		g.floaterm_position = "bottomright"
	-- 		g.floaterm_autoinsert = false
	-- 	end,
	-- },
	{
		"stevearc/oil.nvim",
		event = "VeryLazy",
		opts = {
			default_file_explorer = true,
			view_options = {
				show_hidden = true,
			},
		},
		keys = { { "-", "<cmd>Oil<cr>" } },
	},
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = true,
		version = "*",
		event = "VeryLazy",
		build = ":TSUpdate",
		enabled = not g.vscode,
		config = function()
			local configs = require("nvim-treesitter.configs")
			require("nvim-treesitter.install").compilers = { "clang" }
			configs.setup({
				highlight = {
					enable = true,
					use_languagetree = true,
					additional_vim_regex_highlighting = true,
				},
				indent = { enable = true },
			})
		end,
	},
	{
		"smoka7/hop.nvim",
		event = "VeryLazy",
		opts = {},
		keys = {
			{ "s", "<cmd>HopWord<cr>" },
		},
	},
	{
		"numToStr/Comment.nvim",
		event = "VeryLazy",
		enabled = not g.vscode,
		dependencies = {
			{ "JoosepAlviste/nvim-ts-context-commentstring", opts = {} },
			"nvim-treesitter/nvim-treesitter",
		},
		config = function()
			require("Comment").setup({
				pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
			})
		end,
	},
	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		opts = {},
	},
	{
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		enabled = not g.vscode,
		event = "VeryLazy",
		filetypes = { "typescript", "typescriptreact", "typescript.tsx", "javascript" },
		opts = {
			handlers = {
				["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
			},
			settings = {
				jsx_close_tag = {
					enable = true,
					filetypes = { "javascriptreact", "typescriptreact" },
				},
			},
		},
	},
	{ "SmiteshP/nvim-navic", event = "VeryLazy", enabled = not g.vscode, opts = {} },
	{
		"rachartier/tiny-inline-diagnostic.nvim",
		event = "VeryLazy",
		enabled = not g.vscode,
		config = function()
			require("tiny-inline-diagnostic").setup({
				preset = "modern",
				options = {
					multiline = true,
				},
			})
		end,
	},
	{ "windwp/nvim-autopairs", event = "VeryLazy", opts = {} },
	{
		"saghen/blink.cmp",
		event = "VeryLazy",
		enabled = not g.vscode,
		version = "*",
		opts = {
			completion = {
				accept = { auto_brackets = { enabled = true } },

				documentation = {
					auto_show = true,
					auto_show_delay_ms = 250,
					treesitter_highlighting = true,
					window = { border = "rounded" },
				},
				menu = { border = "rounded" },
			},

			keymap = {
				["<CR>"] = { "accept", "fallback" },
				["<C-CR>"] = { "accept", "fallback" },
				["<C-p>"] = { "select_prev", "fallback" },
				["<C-n>"] = { "select_next", "fallback" },
				["<C-up>"] = { "scroll_documentation_up", "fallback" },
				["<C-down>"] = { "scroll_documentation_down", "fallback" },
			},

			signature = {
				enabled = true,
				window = { border = "rounded" },
			},

			cmdline = { enabled = false },
			sources = {
				default = { "lsp", "path", "buffer" },
				providers = {
					lsp = {
						min_keyword_length = 0,
						score_offset = 97,
					},
					path = {
						score_offset = 98,
						min_keyword_length = 0,
					},
					-- snippets = {
					-- 	min_keyword_length = 2,
					-- 	score_offset = 100,
					-- 	max_items = 1,
					-- },
					buffer = {
						min_keyword_length = 5,
						score_offset = 0,
						max_items = 5,
					},
				},
			},
		},
	},
	{
		"neovim/nvim-lspconfig",
		event = { "VeryLazy" },
		enabled = not g.vscode,
		cmd = { "LspInfo", "LspInstall", "LspUninstall" },
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"nvim-lua/plenary.nvim",
			"stevearc/conform.nvim",
		},
		config = function()
			require("mason").setup({})
			require("mason-lspconfig").setup({})

			local prettier_config = "prettierd"
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					css = { prettier_config },
					scss = { prettier_config },
					typescript = { prettier_config },
					typescriptreact = { prettier_config },
					javascript = { prettier_config },
					svelte = { prettier_config },
				},
				format_on_save = {
					timeout_ms = 150,
				},
			})

			local servers = {
				clangd = {},
				cssls = {},
				html = {},
				jsonls = {},
				lua_ls = {
					settings = {
						Lua = {
							workspace = { checkThirdParty = false },
							telemetry = { enabled = false },
						},
					},
				},
				marksman = {},
				svelte = {},
				ols = {},
				zls = {},
			}

			-- Default handlers for LSP
			local default_handlers = {
				["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
				["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
			}

			-- Disable semantic highlight
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local client = vim.lsp.get_client_by_id(args.data.client_id)
					client.server_capabilities.semanticTokensProvider = nil
				end,
			})

			local capabilities = vim.tbl_deep_extend(
				"force",
				vim.lsp.protocol.make_client_capabilities(),
				require("blink.cmp").get_lsp_capabilities()
				-- require("cmp_nvim_lsp").default_capabilities()
			)
			capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = false
			capabilities.textDocument.foldingRange = { dynamicRegistration = false, lineFoldingOnly = true }

			-- Iterate over our servers and set them up
			for name, config in pairs(servers) do
				require("lspconfig")[name].setup({
					capabilities = capabilities,
					filetypes = config.filetypes,
					handlers = vim.tbl_deep_extend("force", {}, default_handlers, config.handlers or {}),
					settings = config.settings,
					on_attach = function(client, bufnr)
						if client.server_capabilities.documentSymbolProvider then
							require("nvim-navic").attach(client, bufnr)
						end
					end,
				})
			end
		end,
	},
}, {
	defaults = { lazy = true },
	performance = {
		cache = {
			enabled = true,
		},
		rtp = {
			disabled_plugins = {
				"editorconfig",
				"health",
				"spellfile",
				"man",
				"shada",
				"2html_plugin",
				"tohtml",
				"getscript",
				"getscriptPlugin",
				"gzip",
				"logipat",
				"netrw",
				"netrwPlugin",
				"netrwSettings",
				"netrwFileHandlers",
				"tar",
				"tarPlugin",
				"rrhelper",
				"spellfile_plugin",
				"vimball",
				"vimballPlugin",
				"zip",
				"osc52",
				"zipPlugin",
				"tutor",
				"rplugin",
				"syntax",
				"synmenu",
				"optwin",
				"compiler",
				"bugreport",
			},
		},
	},
})

vim.api.nvim_create_autocmd("LspProgress", {
	callback = function(ev)
		local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
		vim.notify(vim.lsp.status(), "info", {
			id = "lsp_progress",
			title = "LSP Progress",
			opts = function(notif)
				notif.icon = ev.data.params.value.kind == "end" and " "
					or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
			end,
		})
	end,
})

-- Options
opt.history = 700
opt.signcolumn = "yes:1"
opt.autoread = true
opt.inccommand = "split"
opt.mouse = "nv"
opt.shortmess = opt.shortmess + "I"
opt.hidden = true
opt.updatetime = 100
opt.wildmenu = true
opt.wildignore = "*.o,*~,*.class"
opt.cmdheight = 1
opt.cmdwinheight = 1
opt.backspace = "eol,start,indent"
opt.whichwrap = opt.whichwrap + "<,>,h,l"
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true
opt.magic = true
opt.timeoutlen = 1000
opt.ttimeoutlen = 0
opt.showmatch = true
opt.mat = 2
opt.number = false
opt.backup = false
opt.writebackup = false
opt.scrolloff = 5
opt.completeopt = "noselect,menuone,longest"
opt.laststatus = 0
opt.smd = false
opt.ru = false
opt.display = "lastline"
opt.termguicolors = true
opt.encoding = "utf8"
opt.expandtab = true
opt.smarttab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.lbr = true
opt.tw = 500
opt.ai = true
opt.si = true
opt.wrap = true
opt.shada = "!,'300,<50,s10,h"
opt.clipboard = "unnamedplus"
opt.undofile = true
opt.foldmethod = "expr"
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldtext = ""
opt.foldenable = true
opt.foldnestmax = 1
opt.foldlevel = 1
opt.foldlevelstart = 1
opt.cursorline = false
opt.winbar = nil

g.markdown_folding = 1
g.skip_ts_context_commentstring_module = true

vim.diagnostic.config({ virtual_text = false })

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
-- map("n", "<Tab>", "za", { silent = true })
map("n", "<Tab>", "za", { silent = true })
map({ "n", "v" }, "0", "^")
map({ "n", "v" }, "j", "gj", { silent = true })
map({ "n", "v" }, "k", "gk", { silent = true })
map({ "n", "v" }, "0", "g0", { silent = true })
map({ "n", "v" }, "$", "g$", { silent = true })

map("n", ";", ":")
map("n", ":", ";")

map({ "n", "v" }, "{", "16gk", { silent = true })
map({ "n", "v" }, "}", "16gj", { silent = true })

map("t", "<esc><esc>", "<C-\\><C-n>")

map({ "n", "v" }, "K", "<cmd>lua vim.lsp.buf.hover()<cr>", { silent = true })
map({ "n", "v" }, "ga", "<cmd>lua vim.lsp.buf.code_action()<cr>", { silent = true })

local group = vim.api.nvim_create_augroup("typescript_autocommands", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
	pattern = { "typescript", "typescriptreact", "ts", "tsx" },
	callback = function(ev)
		map("i", "cl<tab>", "console.log()<left>", { noremap = true, silent = true })
	end,
	group = group,
})

map({ "n" }, "<Leader>z", function()
	if o.winbar == "" then
		o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"
	else
		o.winbar = ""
	end
	print("winbar " .. o.winbar)
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

vim.api.nvim_create_autocmd({ "InsertLeave", "WinEnter" }, {
	callback = function()
		if vim.w.auto_cursorline then
			vim.wo.cursorline = true
			vim.w.auto_cursorline = false
		end
	end,
})

vim.api.nvim_create_autocmd({ "InsertEnter", "WinLeave" }, {
	callback = function()
		if vim.wo.cursorline then
			vim.w.auto_cursorline = true
			vim.wo.cursorline = false
		end
	end,
})
