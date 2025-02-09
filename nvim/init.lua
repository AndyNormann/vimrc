local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local g = vim.g
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api

g.mapleader = " "
g.maplocalleader = " "

g.base46_cache = vim.fn.stdpath("data") .. "/base46_cache/"

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

vim.loader.enable()

require("lazy").setup({
	install = { colorscheme = { "quiet" } },
	{ "philosofonusus/morta.nvim", lazy = false, priority = 1000 },
	-- { "comfysage/evergarden", lazy = false, priority = 1000 },
	{
		"nvchad/ui",
		lazy = false,
		config = function()
			require("nvchad")
		end,
	},

	{
		"nvchad/base46",
		lazy = false,
		build = function()
			require("base46").load_all_highlights()
		end,
	},
	{ "nvchad/volt", lazy = false },
	{ "yorickpeterse/nvim-pqf", event = "VeryLazy", opts = {} },
	{ "mattn/emmet-vim", event = "VeryLazy" },
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		opts = {
			bigfile = { enabled = true },
			dashboard = { enabled = false },
			dim = { enabled = false },
			indent = {
				enabled = false,
				indent = {
					char = "▏",
					only_scope = false,
					only_current = true,
				},
				animate = { enabled = false },
				scope = {
					enabled = true, -- enable highlighting the current scope
					priority = 200,
					char = "▏",
					only_current = true, -- only show scope in the current window
					-- hl = "SnacksIndentScope", ---@type string|string[] hl group for scopes
				},
			},
			input = { enabled = true },
			notifier = { enabled = true },
			notify = { enabled = true },
			quickfile = { enabled = true },
			scroll = { enabled = false },
			picker = { enabled = true },
			statuscolumn = { enabled = false },
			words = { enabled = false },
			zen = { enabled = false },
		},
		keys = {
			{ "<leader>e", "<cmd>lua Snacks.picker.files()<cr>" },
			{ "<leader>f", "<cmd>lua Snacks.picker.grep()<cr>" },
			{ "<leader>b", "<cmd>lua Snacks.picker.buffers()<cr>" },
			{ "<leader>r", "<cmd>lua Snacks.picker.resume()<cr>" },
			{ "<leader>h", "<cmd>lua Snacks.notifier.show_history()<cr>" },
		},
	},
	{
		"voldikss/vim-floaterm",
		event = "VeryLazy",
		enabled = not g.vscode,
		keys = {
			{ "<Leader>t", "<cmd>FloatermToggle<cr>", { silent = true } },
			{ "t", "<Leader>t", "<C-\\><C-n><cmd>FloatermToggle<cr>", { silent = true } },
			{ "<Leader>T", "<cmd>FloatermNew<cr>", { silent = true } },
			{ "<Leader>n", ":FloatermSend " },
			{ "<Leader>m", ":FloatermSend<up><cr>" },
		},
		config = function()
			g.floaterm_wintype = "float"
			g.floaterm_autoclose = 1
			g.floaterm_height = 0.3
			g.floaterm_width = 0.4
			g.floaterm_position = "bottomright"
			g.floaterm_autoinsert = false
		end,
	},
	{
		"stevearc/oil.nvim",
		event = "VeryLazy",
		enabled = not g.vscode,
		dependencies = { { "echasnovski/mini.icons", opts = {} } },
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
		event = "BufRead",
		build = ":TSUpdate",
		enabled = not g.vscode,
		config = function()
			local configs = require("nvim-treesitter.configs")
			require("nvim-treesitter.install").compilers = { "clang" }
			configs.setup({
				ensure_installed = {
					"c",
					"lua",
					"cpp",
					"css",
					"odin",
					"gitignore",
					"json",
					"scss",
					"markdown",
					"markdown_inline",
					"vim",
					"vimdoc",
					"query",
				},
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
		event = "BufRead",
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
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
			"neovim/nvim-lspconfig",
			"hrsh7th/cmp-nvim-lsp",
			"rafamadriz/friendly-snippets",
			"onsails/lspkind.nvim",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			{ "L3MON4D3/LuaSnip", version = "v2.*" },
			"saadparwaiz1/cmp_luasnip",
			"zbirenbaum/copilot-cmp",
		},
		event = "BufRead",
		config = function()
			local cmp = require("cmp")
			local cmp_autopairs = require("nvim-autopairs.completion.cmp")
			require("luasnip.loaders.from_vscode").lazy_load()
			require("copilot_cmp").setup()

			cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
			local my_options = {
				snippet = {
					expand = function(args)
						require("luasnip").lsp_expand(args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				view = {
					entries = {
						name = "custom",
						selection_order = "near_cursor",
						follow_cursor = true,
					},
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-n>"] = cmp.mapping.select_next_item({
						behavior = cmp.ConfirmBehavior.Insert,
					}),
					["<C-p>"] = cmp.mapping.select_prev_item({
						behavior = cmp.ConfirmBehavior.Insert,
					}),
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<C-CR>"] = cmp.mapping.confirm({ select = true }),
					["<Tab>"] = cmp.mapping.confirm({ select = true }),
				}),
				sources = cmp.config.sources({
					{ name = "luasnip", group_index = 6, option = { use_show_condition = true }, priority = 6 },
					{ name = "path", group_index = 6, priority = 6 },
					{ name = "copilot", group_index = 5, max_item_count = 1, priority = 5 },
					{ name = "nvim_lsp", group_index = 4, priority = 4 },
					{ name = "buffer", keyword_length = 3, group_index = 3 },
					{ name = "nvim_lsp_signature_help" },
				}),
				matching = {
					disallow_fuzzy_matching = true,
					disallow_fullfuzzy_matching = true,
					disallow_partial_fuzzy_matching = true,
					disallow_partial_matching = false,
					disallow_prefix_unmatching = true,
				},
				performance = {
					debounce = 0,
					throttle = 0,
				},
				experimental = {
					ghost_text = true,
				},
			}

			local options = vim.tbl_deep_extend("force", my_options, require("nvchad.cmp"))
			cmp.setup(options)
		end,
	},
	-- {
	-- 	"saghen/blink.cmp",
	-- 	event = "VeryLazy",
	-- 	enabled = not g.vscode,
	-- 	dependencies = {
	-- 		"rafamadriz/friendly-snippets",
	-- 		"onsails/lspkind.nvim",
	-- 		{ "giuxtaposition/blink-cmp-copilot" },
	-- 	},
	-- 	version = "*",
	-- 	opts = {
	-- 		appearance = {
	-- 			use_nvim_cmp_as_default = false,
	-- 			nerd_font_variant = "mono",
	-- 			kind_icons = {
	-- 				Copilot = "",
	-- 				Text = "󰉿",
	-- 				Method = "󰊕",
	-- 				Function = "󰊕",
	-- 				Constructor = "󰒓",
	--
	-- 				Field = "󰜢",
	-- 				Variable = "󰆦",
	-- 				Property = "󰖷",
	--
	-- 				Class = "󱡠",
	-- 				Interface = "󱡠",
	-- 				Struct = "󱡠",
	-- 				Module = "󰅩",
	--
	-- 				Unit = "󰪚",
	-- 				Value = "󰦨",
	-- 				Enum = "󰦨",
	-- 				EnumMember = "󰦨",
	--
	-- 				Keyword = "󰻾",
	-- 				Constant = "󰏿",
	--
	-- 				Snippet = "󱄽",
	-- 				Color = "󰏘",
	-- 				File = "󰈔",
	-- 				Reference = "󰬲",
	-- 				Folder = "󰉋",
	-- 				Event = "󱐋",
	-- 				Operator = "󰪚",
	-- 				TypeParameter = "󰬛",
	-- 			},
	-- 		},
	--
	-- 		completion = {
	-- 			accept = { auto_brackets = { enabled = true } },
	--
	-- 			documentation = {
	-- 				auto_show = true,
	-- 				auto_show_delay_ms = 250,
	-- 				treesitter_highlighting = true,
	-- 				window = { border = "rounded" },
	-- 			},
	--
	-- 			ghost_text = {
	-- 				enabled = true,
	-- 			},
	--
	-- 			menu = {
	-- 				border = "rounded",
	--
	-- 				cmdline_position = function()
	-- 					if vim.g.ui_cmdline_pos ~= nil then
	-- 						local pos = vim.g.ui_cmdline_pos -- (1, 0)-indexed
	-- 						return { pos[1] - 1, pos[2] }
	-- 					end
	-- 					local height = (vim.o.cmdheight == 0) and 1 or vim.o.cmdheight
	-- 					return { vim.o.lines - height, 0 }
	-- 				end,
	--
	-- 				draw = {
	-- 					columns = {
	-- 						{ "kind_icon", "label", gap = 1 },
	-- 						{ "kind" },
	-- 					},
	-- 					components = {
	-- 						kind_icon = {
	-- 							text = function(item)
	-- 								local kind = require("lspkind").symbol_map[item.kind] or ""
	-- 								return kind .. " "
	-- 							end,
	-- 							highlight = "CmpItemKind",
	-- 						},
	-- 						label = {
	-- 							text = function(item)
	-- 								return item.label
	-- 							end,
	-- 							highlight = "CmpItemAbbr",
	-- 						},
	-- 						kind = {
	-- 							text = function(item)
	-- 								return item.kind
	-- 							end,
	-- 							highlight = "CmpItemKind",
	-- 						},
	-- 					},
	-- 				},
	-- 			},
	-- 		},
	--
	-- 		keymap = {
	-- 			["<C-e>"] = { "hide", "fallback" },
	-- 			["<Tab>"] = { "accept", "fallback" },
	-- 			["<CR>"] = { "accept", "fallback" },
	-- 			["<C-CR>"] = { "accept", "fallback" },
	-- 			["<C-p>"] = { "select_prev", "fallback" },
	-- 			["<C-n>"] = { "select_next", "fallback" },
	-- 			["<C-up>"] = { "scroll_documentation_up", "fallback" },
	-- 			["<C-down>"] = { "scroll_documentation_down", "fallback" },
	-- 		},
	--
	-- 		signature = {
	-- 			enabled = true,
	-- 			window = { border = "rounded" },
	-- 		},
	--
	-- 		sources = {
	-- 			default = { "lsp", "path", "snippets", "buffer", "copilot" },
	-- 			cmdline = {},
	-- 			providers = {
	-- 				lsp = {
	-- 					min_keyword_length = 0,
	-- 					score_offset = 97,
	-- 				},
	-- 				path = {
	-- 					score_offset = 98,
	-- 					min_keyword_length = 0,
	-- 				},
	-- 				snippets = {
	-- 					min_keyword_length = 2,
	-- 					score_offset = 100,
	-- 					max_items = 1,
	-- 				},
	-- 				buffer = {
	-- 					min_keyword_length = 5,
	-- 					score_offset = 0,
	-- 					max_items = 5,
	-- 				},
	-- 				copilot = {
	-- 					name = "copilot",
	-- 					enabled = true,
	-- 					module = "blink-cmp-copilot",
	-- 					min_keyword_length = 1,
	-- 					score_offset = 99,
	-- 					async = true,
	-- 					transform_items = function(_, items)
	-- 						local CompletionItemKind = require("blink.cmp.types").CompletionItemKind
	-- 						local kind_idx = #CompletionItemKind + 1
	-- 						CompletionItemKind[kind_idx] = "Copilot"
	-- 						for _, item in ipairs(items) do
	-- 							item.kind = kind_idx
	-- 						end
	-- 						return items
	-- 					end,
	-- 				},
	-- 			},
	-- 		},
	-- 	},
	-- },
	{
		"neovim/nvim-lspconfig",
		event = { "BufRead" },
		enabled = not g.vscode,
		cmd = { "LspInfo", "LspInstall", "LspUninstall" },
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"nvim-lua/plenary.nvim",
			"stevearc/conform.nvim",
			{ "zbirenbaum/copilot.lua", cmd = "Copilot" },
		},
		config = function()
			require("mason").setup({})
			require("mason-lspconfig").setup({})
			require("copilot").setup({
				suggestion = { enabled = false },
				panel = { enabled = false },
			})

			map({ "n", "v" }, "K", "<cmd>lua vim.lsp.buf.hover()<cr>", { silent = true })
			map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
			map("n", "gr", "<cmd>lua vim.lsp.buf.references()<cr>", { silent = true })

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
					go = { "gofumpt" },
					templ = { "gofumpt" },
				},
				format_on_save = {
					timeout_ms = 250,
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
				gopls = {},
				templ = {},
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
				-- require("blink.cmp").get_lsp_capabilities()
				require("cmp_nvim_lsp").default_capabilities()
			)
			capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = false

			-- Iterate over our servers and set them up
			for name, config in pairs(servers) do
				require("lspconfig")[name].setup({
					capabilities = capabilities,
					filetypes = config.filetypes,
					handlers = vim.tbl_deep_extend("force", {}, default_handlers, config.handlers or {}),
					settings = config.settings,
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

-- vim.api.nvim_create_autocmd("LspProgress", {
-- 	callback = function(ev)
-- 		local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
-- 		vim.notify(vim.lsp.status(), "info", {
-- 			id = "lsp_progress",
-- 			title = "LSP Progress",
-- 			opts = function(notif)
-- 				notif.icon = ev.data.params.value.kind == "end" and " "
-- 					or spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
-- 			end,
-- 		})
-- 	end,
-- })

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
opt.foldenable = false
opt.foldnestmax = 1
opt.foldlevel = 99
opt.foldlevelstart = 1
opt.foldcolumn = "0"
opt.cursorline = true

g.markdown_folding = 1
g.skip_ts_context_commentstring_module = true

vim.diagnostic.config({ virtual_text = false })

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
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

-- dofile(vim.g.base46_cache .. "defaults")
-- dofile(vim.g.base46_cache .. "statusline")
for _, v in ipairs(vim.fn.readdir(vim.g.base46_cache)) do
	dofile(vim.g.base46_cache .. v)
end

if vim.g.neovide then
	cmd.colorscheme("nvchad")

	vim.o.guifont = "Cascadia Code:h24:b"
	vim.opt.linespace = -10
	vim.g.neovide_padding_top = 0
	vim.g.neovide_padding_bottom = 0
	vim.g.neovide_padding_right = 0
	vim.g.neovide_padding_left = 0
	vim.g.neovide_show_border = false
	vim.g.neovide_position_animation_length = 0
	vim.g.neovide_scroll_animation_length = 0
	vim.g.neovide_cursor_animation_length = 0
	vim.g.neovide_cursor_trail_size = 0
	vim.g.neovide_refresh_rate = 100
	vim.g.neovide_fullscreen = false
end

if not g.vscode and not vim.g.neovide then
	opt.background = "dark"
	-- cmd.colorscheme("morta")
	cmd.colorscheme("nvchad")
	-- cmd.colorscheme("oldworld")

	-- api.nvim_set_hl(0, "Normal", { bg = "none" })
	-- api.nvim_set_hl(0, "NormalNC", { bg = "none" })
	-- api.nvim_set_hl(0, "SignColumn", { bg = "none" })
	-- api.nvim_set_hl(0, "SnacksIndentScope", { fg = "#E67E80" })

	-- api.nvim_set_hl(0, "Visual", { reverse = true })
end
