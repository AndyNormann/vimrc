local vim = vim
local opt = vim.opt
local map = vim.keymap.set
local g = vim.g
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api

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
	{ "RRethy/base16-nvim", lazy = false, priority = 1000 },
	{ "mattn/emmet-vim", lazy = true, event = "VeryLazy" },
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
	{ "Tetralux/jai.vim", ft = { "jai" } },
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = true,
		version = "*",
		event = "BufRead",
		build = ":TSUpdate",
		config = function()
			if not g.vscode then
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
			end
		end,
	},
	{ "norcalli/nvim-colorizer.lua", event = "VeryLazy", opts = {} },
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
		"cohama/lexima.vim",
		event = "VeryLazy",
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
			-- local border_chars_outer_thin_telescope = { "▔", "▕", "▁", "▏", "🭽", "🭾", "🭿", "🭼" }
			-- local border_chars_outer_thick_telescope = { "▀", "▐", "▄", "▌", "▛", "▜", "▟", "▙" }
			local actions = require("telescope.actions")
			require("telescope").setup({
				pickers = {
					colorscheme = {
						enable_preview = true,
					},
				},
				defaults = {
					-- borderchars = {
					-- 	prompt = border_chars_outer_thick_telescope,
					-- 	results = border_chars_outer_thick_telescope,
					-- 	preview = border_chars_outer_thick_telescope,
					-- },
					-- border = true,
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
		lazy = false,
		-- event = "VeryLazy",
		opts = {
			presets = {
				bottom_search = true,
				command_palette = false,
				lsp_doc_border = false,
			},
			messages = {
				enabled = true,
				view_search = false,
			},
			cmdline = {
				view = "cmdline",
			},
		},
		dependencies = { "MunifTanjim/nui.nvim" },
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
		"kdheepak/lazygit.nvim",
		cmd = {
			"LazyGit",
			"LazyGitConfig",
			"LazyGitCurrentFile",
			"LazyGitFilter",
			"LazyGitFilterCurrentFile",
		},
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		keys = {
			{ "<leader>g", "<cmd>LazyGit<cr>", desc = "LazyGit" },
		},
	},
	{
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		event = "BufRead",
		filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
		opts = {},
	},
	{
		"hrsh7th/nvim-cmp",
		event = { "LspAttach" },
		dependencies = {
			-- "hrsh7th/cmp-cmdline",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"nvim-lua/plenary.nvim",
			"onsails/lspkind.nvim",
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-nvim-lsp-signature-help",
		},
		config = function()
			local cmp = require("cmp")
			local lspkind = require("lspkind")

			cmp.setup({
				completion = { completeopt = "menu, menuone, noselect" },
				mapping = cmp.mapping.preset.insert({
					["<C-k>"] = cmp.mapping.select_prev_item(), -- previous suggestion
					["<C-j>"] = cmp.mapping.select_next_item(), -- next suggestion
					["<C-u>"] = cmp.mapping.scroll_docs(4), -- scroll up preview
					["<C-d>"] = cmp.mapping.scroll_docs(-4), -- scroll down preview
					["<C-Space>"] = cmp.mapping.complete({}), -- show completion suggestions
					["<C-c>"] = cmp.mapping.abort(), -- close completion window
					["<C-CR>"] = cmp.mapping.confirm({ select = true }), -- select suggestion
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

				-- sources for autocompletion
				sources = cmp.config.sources({
					{ name = "codeium", max_item_count = 1, group_index = 1 },
					{ name = "path", group_index = 1 },
					{ name = "nvim_lsp", group_index = 2 },
					{ name = "buffer", group_index = 3 },
					{ name = "nvim_lsp_signature_help" },
				}),
				-- Enable icons for lsp/autocompletion
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
					ghost_text = true,
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
			"Exafunction/codeium.nvim",
		},
		config = function()
			require("mason").setup({})
			require("mason-lspconfig").setup({})
			require("codeium").setup({ enable_chat = true })

			local prettier_config = { "prettierd", "prettier" }
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					css = { prettier_config },
					scss = { prettier_config },
					typescript = { prettier_config },
					typescriptreact = { prettier_config },
					svelte = { prettier_config },
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
			}

			-- Default handlers for LSP
			local default_handlers = {
				["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
				["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
			}
			vim.diagnostic.config({
				virtual_text = false,
			})

			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(args)
					local client = vim.lsp.get_client_by_id(args.data.client_id)
					client.server_capabilities.semanticTokensProvider = nil
				end,
			})

			api.nvim_exec(
				[[autocmd CursorHold * lua vim.diagnostic.open_float(0, {scope = "cursor", focusable = false})]],
				false
			)

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

-- Options
opt.history = 700
opt.signcolumn = "yes:1"
opt.autoread = true
opt.inccommand = "split"
opt.mouse = "a"
opt.shortmess = opt.shortmess + "I"
opt.hidden = true
opt.updatetime = 100
opt.cmdheight = 2
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

opt.laststatus = 2
opt.smd = false
opt.ru = false
opt.display = "lastline"
opt.termguicolors = true
opt.encoding = "utf8"
opt.background = "dark"

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

api.nvim_exec([[au BufRead,BufNewFile *.jai set filetype=jai]], false)

-- Statusline
local modes = {
	["n"] = "NORMAL",
	["no"] = "NORMAL",
	["v"] = "VISUAL",
	["V"] = "V-LINE",
	[""] = "V-BLCK",
	["s"] = "SELECT",
	["S"] = "SELECT LINE",
	[""] = "SELECT BLOCK",
	["i"] = "INSERT",
	["ic"] = "INSERT",
	["R"] = "REPLACE",
	["Rv"] = "VISUAL REPLACE",
	["c"] = "COMAND",
	["cv"] = "VIM EX",
	["ce"] = "EX",
	["r"] = "PROMPT",
	["rm"] = "MOAR",
	["r?"] = "CONFIRM",
	["!"] = "SHELL",
	["t"] = "TERMINAL",
}
local function mode()
	local current_mode = api.nvim_get_mode().mode
	return string.format(" %s ", modes[current_mode]):upper()
end

local function update_mode_colors()
	local current_mode = api.nvim_get_mode().mode
	local mode_color = "%#StatusLineAccent#"
	if current_mode == "n" then
		mode_color = "%#StatuslineAccent#"
	elseif current_mode == "i" or current_mode == "ic" then
		mode_color = "%#StatuslineInsertAccent#"
	elseif current_mode == "v" or current_mode == "V" or current_mode == "" then
		mode_color = "%#StatuslineVisualAccent#"
	elseif current_mode == "R" then
		mode_color = "%#StatuslineReplaceAccent#"
	elseif current_mode == "c" then
		mode_color = "%#StatuslineCmdLineAccent#"
	elseif current_mode == "t" then
		mode_color = "%#StatuslineTerminalAccent#"
	end
	return mode_color
end
local function filename()
	local fname = fn.expand("%:t")
	if fname == "" then
		return ""
	end
	return fname .. " "
end

local function lsp()
	if #vim.lsp.buf_get_clients() == 0 then
		return ""
	end
	local names = ""
	for _, client in ipairs(vim.lsp.get_active_clients()) do
		if client.name == "typescript-tools" then
			names = names .. "ts "
		elseif client.name == "jsonls" then
			names = names .. "json "
		elseif client.name == "cssls" then
			names = names .. "css "
		elseif client.name == "htmlls" then
			names = names .. "html "
		elseif client.name == "lua_ls" then
			names = names .. "lua "
		else
			names = names .. client.name .. " "
		end
	end
	names = names:sub(1, -2)

	return "%#Comment# [" .. names .. "]"
end

Statusline = {}

Statusline.active = function()
	return table.concat({
		"%#Statusline#",
		update_mode_colors(),
		mode(),
		lsp(),
		"%#Normal# ",
		filename(),
		"%m",
		"%#Normal#",
	})
end

function Statusline.inactive()
	return " %t"
end

if g.vscode then
else
	-- cmd("colorscheme kanagawa")
	-- cmd("colorscheme doomchad")
	-- cmd.colorscheme("catppuccin")
	-- cmd.colorscheme("rxyhn")
	-- cmd.colorscheme("onenord_light")
	-- cmd.colorscheme("everforest")
	-- cmd.colorscheme("base16-catppuccin")
	-- cmd.colorscheme("base16-tokyo-city-dark")
	cmd.colorscheme("base16-material-darker")
	-- cmd.colorscheme("base16-decaf")
	api.nvim_set_hl(0, "Normal", { bg = "none" })
	api.nvim_set_hl(0, "NormalNC", { bg = "none" })
	api.nvim_set_hl(0, "SignColumn", { bg = "none" })
	api.nvim_set_hl(0, "Visual", { reverse = true })
	-- api.nvim_set_hl(0, "CursorLine", { reverse = true })
end

-- Monochrome colors
-- api.nvim_exec([[ hi StatuslineAccent guibg=#CCCCCC guifg=#101010 ]], false)
-- api.nvim_exec([[ hi StatuslineInsertAccent guibg=#AAAAAA guifg=#0A1219 ]], false)
-- api.nvim_exec([[ hi StatuslineVisualAccent guibg=#F9FAFB guifg=#111827 ]], false)
-- api.nvim_exec([[ hi StatuslineReplaceAccent guibg=#c6c6c6 guifg=#262626 ]], false)
-- api.nvim_exec([[ hi StatuslineCmdLineAccent guibg=#888888 guifg=#101010 ]], false)
-- api.nvim_exec([[ hi StatuslineTerminalAccent guibg=#EBEBEB guifg=#101010 ]], false)

-- One dark colors
api.nvim_exec([[ hi StatuslineAccent guibg=#95c561 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineInsertAccent guibg=#7199ee guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineVisualAccent guibg=#d7a65f guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineReplaceAccent guibg=#ee6d85 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineCmdLineAccent guibg=#95c561 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineTerminalAccent guibg=#95c561 guifg=#06080a ]], false)

-- Catppuccin colors
-- api.nvim_exec([[ hi StatuslineAccent guibg=#CBA6F7 guifg=#06080a ]], false)
-- api.nvim_exec([[ hi StatuslineInsertAccent guibg=#89B4FA guifg=#06080a ]], false)
-- api.nvim_exec([[ hi StatuslineVisualAccent guibg=#F5C2E7 guifg=#06080a ]], false)
-- api.nvim_exec([[ hi StatuslineReplaceAccent guibg=#ee6d85 guifg=#06080a ]], false)
-- api.nvim_exec([[ hi StatuslineCmdLineAccent guibg=#95c561 guifg=#06080a ]], false)
-- api.nvim_exec([[ hi StatuslineTerminalAccent guibg=#95c561 guifg=#06080a ]], false)

api.nvim_exec(
	[[
  augroup Statusline
  au!
  au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
  au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
  augroup END
]],
	false
)
