local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local g = vim.g
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api
local o = vim.o

-- Options
opt.history = 700
opt.signcolumn = "yes:1"
opt.autoread = true
opt.inccommand = "split"
opt.mouse = "a"
opt.shortmess = opt.shortmess + "I"
opt.hidden = true
opt.updatetime = 100
opt.wildmenu = true
opt.wildignore = "*.o,*~,*.class"
opt.cmdheight = 0
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
opt.foldnestmax = 2
opt.foldlevel = 99
opt.foldlevelstart = 1
opt.foldcolumn = "0"
opt.cursorline = false
g.markdown_folding = 1
g.skip_ts_context_commentstring_module = true

opt.background = "dark"
o.background = "dark"

g.mapleader = " "
g.maplocalleader = " "

-- Plugins
-- Setup Lazy vim
local lazypath = fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
opt.rtp:prepend(lazypath)

vim.loader.enable()

require("lazy").setup({
	-- {
	-- 	"notken12/base46-colors",
	-- 	lazy = false,
	-- 	priority = 1000,
	-- 	config = function(plugin)
	-- 		vim.opt.rtp:append(plugin.dir .. "/lua")
	-- 	end,
	-- },
	{ "rebelot/kanagawa.nvim", lazy = false, priority = 1000 },
	{ "catppuccin/nvim", lazy = false, priority = 1000 },

	{ "mattn/emmet-vim", event = "VeryLazy" },
	{
		"voldikss/vim-floaterm",
		event = "VeryLazy",
		keys = {
			{ "<Leader>t", "<cmd>FloatermToggle<cr>", { silent = true } },
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
		"rachartier/tiny-code-action.nvim",
		dependencies = {
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope.nvim" },
		},
		event = "LspAttach",
		keys = {
			{
				"<Leader>ca",
				function()
					require("tiny-code-action").code_action()
				end,
				{ noremap = true, silent = true },
			},
		},
		opts = {},
	},
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = true,
		version = "*",
		event = "BufRead",
		build = ":TSUpdate",
		config = function()
			if g.vscode then
				return
			end

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
					"org",
					"vim",
					"vimdoc",
					"query",
				},
				highlight = {
					enable = true,
					use_languagetree = true,
					additional_vim_regex_highlighting = true,
				},
				fold = {
					fold_one_line_after = true,
				},
				indent = { enable = true },
			})
		end,
	},
	{
		"numToStr/Comment.nvim",
		event = "VeryLazy",
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
		"smoka7/hop.nvim",
		opts = {},
		keys = {
			{ "f", "<cmd>HopWord<cr>" },
		},
	},
	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		opts = {},
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.6",
		event = "VeryLazy",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		keys = {
			{ "<Leader>e", "<cmd>Telescope find_files<cr>" },
			{ "<Leader>f", "<cmd>Telescope live_grep<cr>" },
			{ "<Leader>b", "<cmd>Telescope buffers<cr>" },
			{ "<Leader>r", "<cmd>Telescope resume<cr>" },
		},
		config = function()
			local actions = require("telescope.actions")
			require("telescope").setup({
				pickers = {
					colorscheme = {
						enable_preview = true,
					},
				},
				defaults = {
					mappings = {
						n = {
							["<esc>"] = actions.close,
						},
						i = {
							["<esc>"] = actions.close,
						},
					},
					path_display = { "smart" },
				},
				extensions = {
					fzf = {
						fuzzy = true,
						override_generic_sorter = true,
						override_file_sorter = true,
						case_mode = "smart_case",
					},
				},
			})
			require("telescope").load_extension("fzf")
		end,
	},
	{
		"folke/noice.nvim",
		priority = 1000,
		lazy = true,
		enabled = not g.vscode,
		event = "VeryLazy",
		opts = {
			presets = {
				bottom_search = true,
				command_palette = false,
				lsp_doc_border = true,
			},
			messages = {
				enabled = true,
				view_search = false,
			},
			cmdline = {
				enabled = true,
				view = "cmdline",
			},
		},
	},
	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
		dependencies = {
			"honza/vim-snippets",
		},
		event = "VeryLazy",
		config = function()
			local ls = require("luasnip")
			ls.config.set_config({
				history = true,
				region_check_events = "InsertEnter",
				delete_check_events = "TextChanged,InsertLeave",
			})
			require("luasnip.loaders.from_snipmate").lazy_load()
			vim.keymap.set({ "i", "s" }, "<Tab>", function()
				if ls.expandable() then
					ls.expand()
				else
					local key = api.nvim_replace_termcodes("<Tab>", true, false, true)
					api.nvim_feedkeys(key, "n", false)
				end
			end, { silent = true })
		end,
	},
	{
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		enabled = not g.vscode,
		event = "BufRead",
		filetypes = { "typescript", "typescriptreact", "typescript.tsx", "javascript" },
		opts = {
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
		config = function()
			require("tiny-inline-diagnostic").setup()
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		event = { "LspAttach" },
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"nvim-lua/plenary.nvim",
			"onsails/lspkind.nvim",
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"zbirenbaum/copilot-cmp",
			"windwp/nvim-autopairs",
		},
		config = function()
			local cmp = require("cmp")
			local lspkind = require("lspkind")
			require("copilot_cmp").setup()
			require("nvim-autopairs").setup()
			local cmp_autopairs = require("nvim-autopairs.completion.cmp")

			cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

			cmp.setup({
				completion = {
					completeopt = "menu, menuone, noselect",
				},
				mapping = cmp.mapping.preset.insert({
					["<C-k>"] = cmp.mapping.select_prev_item(), -- previous suggestion
					["<C-j>"] = cmp.mapping.select_next_item(), -- next suggestion
					["<C-u>"] = cmp.mapping.scroll_docs(4), -- scroll up preview
					["<C-d>"] = cmp.mapping.scroll_docs(-4), -- scroll down preview
					["<C-Space>"] = cmp.mapping.complete({}), -- show completion suggestions
					["<C-c>"] = cmp.mapping.abort(), -- close completion window
					["<C-CR>"] = cmp.mapping.confirm({ select = true }), -- select suggestion
					["<CR>"] = cmp.mapping({
						i = function(fallback)
							if cmp.visible() and cmp.get_active_entry() then
								cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
							else
								fallback()
							end
						end,
						s = cmp.mapping.confirm({ select = true }),
						c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
					}),
				}),
				window = {
					completion = cmp.config.window.bordered({
						border = { "┌", "─", "┐", "│", "┘", "─", "└", "│" },
						scrollbar = false,
						winhighlight = "Normal:Normal,FloatBorder:CmpWinBorder,CursorLine:PmenuSel,Search:None",
					}),
					documentation = cmp.config.window.bordered({
						border = { "┌", "─", "┐", "│", "┘", "─", "└", "│" },
						scrollbar = false,
						winhighlight = "Normal:Normal,FloatBorder:CmpWinBorder,CursorLine:PmenuSel,Search:None",
					}),
				},

				sources = cmp.config.sources({
					{ name = "copilot", max_item_count = 1, group_index = 1 },
					{ name = "path", group_index = 1 },
					{ name = "nvim_lsp", group_index = 2 },
					{ name = "buffer", group_index = 3 },
					{ name = "nvim_lsp_signature_help" },
				}),
				formatting = {
					expandable_indicator = true,
					format = lspkind.cmp_format({
						mode = "symbol_text",
						maxwidth = 30,
						ellipsis_char = "...",
						symbol_map = {
							Codeium = "",
						},
					}),
				},
				experimental = {
					ghost_text = false,
				},
			})
		end,
	},
	{
		"neovim/nvim-lspconfig",
		event = { "BufRead" },
		cmd = { "LspInfo", "LspInstall", "LspUninstall" },
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"nvim-lua/plenary.nvim",
			"hrsh7th/cmp-nvim-lsp",
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
					c = { "clangd" },
					cpp = { "clangd" },
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
				java_language_server = {},
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

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local client = vim.lsp.get_client_by_id(args.data.client_id)
					client.server_capabilities.semanticTokensProvider = nil
				end,
			})

			-- nvim-cmp supports additional completion capabilities
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			local default_capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

			-- Iterate over our servers and set them up
			for name, config in pairs(servers) do
				require("lspconfig")[name].setup({
					capabilities = default_capabilities,
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
				"matchparen",
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

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")
map({ "n", "v" }, "j", "gj", { silent = true })
map({ "n", "v" }, "k", "gk", { silent = true })
map({ "n", "v" }, "0", "g0", { silent = true })
map({ "n", "v" }, "$", "g$", { silent = true })
map({ "i" }, "<C-y>", "<Nop>")

map("n", ";", ":")
map("n", ":", ";")

map({ "n", "v" }, "{", "16gk", { silent = true })
map({ "n", "v" }, "}", "16gj", { silent = true })

map("t", "<esc><esc>", "<C-\\><C-n>")
map({ "n", "v" }, "K", "<cmd>lua vim.lsp.buf.hover()<cr>", { silent = true })
map("n", "<tab>", "za", { silent = true })
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
map("n", "gr", "<cmd>lua vim.lsp.buf.rename()<cr>", { silent = true })

cmd([[
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
  au FocusGained,BufEnter * :silent! !
]])

-- cmd.colorscheme("catppuccin")
-- cmd.colorscheme("decay")
cmd.colorscheme("kanagawa-wave")
-- cmd.colorscheme("catppuccin-mocha")

api.nvim_set_hl(0, "Normal", { bg = "none" })
api.nvim_set_hl(0, "NormalNC", { bg = "none" })
api.nvim_set_hl(0, "SignColumn", { bg = "none" })
api.nvim_set_hl(0, "Visual", { reverse = true })
