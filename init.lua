local opt = vim.opt
local wo = vim.wo
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
-- if not vim.loop.fs_stat(lazypath) then
--   fn.system({
--     "git",
--     "clone",
--     "--filter=blob:none",
--     "https://github.com/folke/lazy.nvim.git",
--     "--branch=stable", -- latest stable release
--     lazypath,
--   })
-- end
opt.rtp:prepend(lazypath)

vim.loader.enable()

require("lazy").setup({
  -- {"onur-ozkan/nimda.vim", lazy = true, priority = 1000},
  -- {"sainnhe/everforest", lazy = false, priority = 1000},
  -- {"tiagovla/tokyodark.nvim", lazy = true, priority = 1000},
  { "pacokwon/onedarkhc.vim", lazy = false, priority = 1000 },
  -- { "EdenEast/nightfox.nvim", lazy = false, priority = 1000 },
  {
    "junegunn/fzf.vim",
    lazy = true,
    dependencies = { { "junegunn/fzf", dir = "~/.fzf", build = "./install --all" } },
    keys = {
      { "<Leader>e", "<cmd>Files<cr>" },
      { "<Leader>f", "<cmd>Rg<cr>" },
      { "<Leader>b", "<cmd>Buffers<cr>" },
    }
  },
  { "tpope/vim-surround",     lazy = true,  event = "VeryLazy" },
  { "tpope/vim-commentary",   lazy = true,  event = "VeryLazy" },
  { "mattn/emmet-vim",        lazy = true,  event = "VeryLazy" },
  { "tikhomirov/vim-glsl",    lazy = true,  event = "VeryLazy" },
  {
    "voldikss/vim-floaterm",
    lazy = true,
    event = "VeryLazy",
    config = function()
      g.floaterm_wintype = "float"
      g.floaterm_autoclose = 1
      g.floaterm_height = 0.5
      g.floaterm_position = "bottomright"
      g.floaterm_autoinsert = "false"
      map("n", "<Leader>t", ":FloatermToggle<cr>", { silent = true })
      map("n", "<Leader>T", ":FloatermNew<cr>")
      map("n", "<Leader>n", ":FloatermSend ")
      map("n", "<Leader>m", ":FloatermSend<Up><cr>")
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = true,
    event = "VeryLazy",
    build = ":TSUpdate",
    config = function()
      local configs = require('nvim-treesitter.configs')
      require "nvim-treesitter.install".compilers = { "clang" }
      configs.setup({
        ensure_installed = { "c", "lua", "cpp", "css", "odin", "gitignore", "json", "scss",
          "typescript", "javascript",
          "markdown", "markdown_inline", "org", "vim", "vimdoc", "query" },
        autopairs = {
          enable = true,
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
        context_commentstring = {
          enable = true,
          enable_autocmd = false,
        },
      })
    end
  },
  {
    "windwp/nvim-autopairs",
    lazy = true,
    event = { "VeryLazy" },
    config = function() require("nvim-autopairs").setup() end
  },
  {
    "L3MON4D3/LuaSnip",
    version = "v2.*",
    build = "make install_jsregexp",
    dependencies = { "honza/vim-snippets" },
    lazy = true,
    event = { "VeryLazy" },
    config = function()
      local ls = require('luasnip')
      require("luasnip.loaders.from_snipmate").lazy_load()
      vim.keymap.set({ "i" }, "<Tab>", function() ls.expand_or_jump() end, { silent = true })
    end
  },

  -- {"hrsh7th/nvim-cmp",
  -- event = { "VeryLazy" },
  -- dependencies = {
  -- "hrsh7th/cmp-nvim-lsp",
  -- "hrsh7th/cmp-buffer",
  -- "hrsh7th/cmp-path",
  --     "nvim-lua/plenary.nvim",
  --     -- "zbirenbaum/copilot.lua",
  --     -- "zbirenbaum/copilot-cmp",
  -- "onsails/lspkind.nvim",
  --     "hrsh7th/cmp-nvim-lsp-signature-help",
  -- "windwp/nvim-ts-autotag",
  -- "windwp/nvim-autopairs",
  -- },
  -- config = function()
  -- local cmp = require("cmp")
  -- local lspkind = require("lspkind")

  -- require("nvim-autopairs").setup()

  --     -- require("copilot").setup({
  --     --   suggestion = { enabled = false },
  --     --   panel = { enabled = false },
  --     -- })
  --     -- require("copilot_cmp").setup()

  -- cmp.setup({
  -- window = {
  -- completion = cmp.config.window.bordered(),
  -- documentation = cmp.config.window.bordered(),
  -- },
  -- mapping = cmp.mapping.preset.insert({
  -- ["<C-k>"] = cmp.mapping.select_prev_item(), -- previous suggestion
  -- ["<C-j>"] = cmp.mapping.select_next_item(), -- next suggestion
  -- ["<C-u>"] = cmp.mapping.scroll_docs(4), -- scroll up preview
  -- ["<C-d>"] = cmp.mapping.scroll_docs(-4), -- scroll down preview
  -- ["<C-Space>"] = cmp.mapping.complete({}), -- show completion suggestions
  -- ["<C-c>"] = cmp.mapping.abort(), -- close completion window
  -- ["<C-CR>"] = cmp.mapping.confirm({ select = true }), -- select suggestion
  -- }),
  -- -- sources for autocompletion
  -- sources = cmp.config.sources({
  -- { name = "nvim_lsp", max_item_count = 20 }, -- lsp
  -- { name = "buffer", max_item_count = 5 }, -- text within current buffer
  -- -- { name = "copilot", max_item_count = 1 }, -- Copilot suggestions
  -- { name = "path", max_item_count = 5 }, -- file system paths
  --         { name = "nvim_lsp_signature_help" },
  -- }),
  -- -- Enable icons for lsp/autocompletion
  -- formatting = {
  -- expandable_indicator = true,
  -- format = lspkind.cmp_format({
  -- mode = "symbol_text",
  -- maxwidth = 40,
  -- ellipsis_char = "...",
  -- symbol_map = {
  -- Copilot = "",
  -- },
  -- }),
  -- },
  -- experimental = {
  -- ghost_text = true,
  -- },
  -- })
  -- end,
  -- },
  -- {"neovim/nvim-lspconfig",
  -- event = { "BufReadPost" },
  -- cmd = { "LspInfo", "LspInstall", "LspUninstall", "Mason" },
  -- dependencies = {
  -- "williamboman/mason.nvim",
  -- "williamboman/mason-lspconfig.nvim",
  -- "hrsh7th/cmp-nvim-lsp",
  -- "nvimtools/none-ls.nvim",
  --     "nvimdev/lspsaga.nvim",
  --     -- "j-hui/fidget.nvim",
  --     { "mrded/nvim-lsp-notify", dependencies = { "rcarriga/nvim-notify" } },
  --     { "folke/neodev.nvim", opts = {} },
  --   },
  -- config = function()
  --     require("neodev").setup({})
  -- -- local null_ls = require("null-ls")
  -- map("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", {silent = true})
  --     map("n", "<M-CR>", "<cmd>Lspsaga hover_doc<CR>", {silent = true})
  --     map("n", "<Leader>d", "<cmd>Lspsaga show_buf_diagnostics<CR>", {silent = true})

  --     -- require('fidget').setup({})
  --     require("notify").setup({
  --       background_colour = "#000000",
  --     })
  --     require('lsp-notify').setup({
  --       notify = require('notify'),
  --     })
  --     require('lspsaga').setup({
  --       symbol_in_winbar = {
  --         enable = false
  --       },
  --       ui = {
  --         code_action = ""
  --       },
  --       code_action_prompt = { enable = false, },
  --       code_action_icon = "",
  --     })
  --     -- require("diagflow").setup({})

  -- require("mason").setup({})
  -- require("mason-lspconfig").setup({})

  -- -- LSP servers to install (see list here: https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers )
  -- local servers = {
  -- bashls = {},
  -- clangd = {},
  -- cssls = {},
  -- graphql = {},
  -- html = {},
  -- jsonls = {},
  -- lua_ls = {
  -- settings = {
  -- Lua = {
  -- workspace = { checkThirdParty = false },
  -- telemetry = { enabled = false },
  -- },
  -- },
  -- },
  --       svelte = {},
  -- marksman = {},
  -- ols = {},
  -- tailwindcss = {},
  --       eslint = {},
  -- tsserver = {},
  -- }

  -- -- Default handlers for LSP
  -- local default_handlers = {
  -- ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
  -- ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
  -- }

  -- -- nvim-cmp supports additional completion capabilities
  -- local capabilities = vim.lsp.protocol.make_client_capabilities()
  -- local default_capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  -- -- Iterate over our servers and set them up
  -- for name, config in pairs(servers) do
  -- require("lspconfig")[name].setup({
  -- capabilities = default_capabilities,
  -- filetypes = config.filetypes,
  -- handlers = vim.tbl_deep_extend("force", {}, default_handlers, config.handlers or {}),
  -- settings = config.settings,
  -- })
  -- end

  -- -- Congifure LSP linting, formatting, diagnostics, and code actions
  -- -- local formatting = null_ls.builtins.formatting
  -- -- local diagnostics = null_ls.builtins.diagnostics
  -- -- local code_actions = null_ls.builtins.code_actions

  -- -- null_ls.setup({
  -- -- 	border = "rounded",
  -- -- 	sources = {
  -- -- 		-- formatting
  -- -- 		-- formatting.prettierd,
  -- -- 		-- formatting.stylua,
  -- -- 		-- formatting.ocamlformat,

  -- -- 		-- diagnostics
  -- -- 		-- diagnostics.eslint_d.with({
  -- -- 		-- 	condition = function(utils)
  -- -- 		-- 		return utils.root_has_file({ ".eslintrc.js", ".eslintrc.cjs", ".eslintrc.json" })
  -- -- 		-- 	end,
  -- -- 		-- }),

  -- -- 		-- code actions
  -- -- 		-- code_actions.eslint_d.with({
  -- -- 		-- 	condition = function(utils)
  -- -- 		-- 		return utils.root_has_file({ ".eslintrc.js", ".eslintrc.cjs", ".eslintrc.json" })
  -- -- 		-- 	end,
  -- -- 		-- }),
  -- -- 	},
  -- -- })

  --     vim.api.nvim_create_autocmd("LspAttach", {
  --       callback = function(args)
  --         local client = vim.lsp.get_client_by_id(args.data.client_id)
  --         client.server_capabilities.semanticTokensProvider = nil
  --       end,
  --     });


  -- -- Configure borderd for LspInfo ui
  -- require("lspconfig.ui.windows").default_options.border = "rounded"

  -- -- Configure diagostics border
  -- vim.diagnostic.config({
  -- float = {
  -- border = "rounded",
  -- },
  -- })
  -- end,
  -- },

  {
    "neoclide/coc.nvim",
    event = "VeryLazy",
    branch = "master",
    build = "npm ci",
    config = function()
      local keyset = vim.keymap.set
      function _G.show_docs()
        local cw = fn.expand('<cword>')
        if fn.index({ 'vim', 'help' }, vim.bo.filetype) >= 0 then
          api.nvim_command('h ' .. cw)
        elseif api.nvim_eval('coc#rpc#ready()') then
          fn.CocActionAsync('doHover')
        else
          api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
        end
      end

      api.nvim_create_user_command("Format", "call CocAction('format')", {})
      -- opt.statusline:prepend("%{coc#status()}%{get(b:,'coc_current_function','')}")

      keyset("n", "K", '<CMD>lua _G.show_docs()<CR>', { silent = true })
      cmd [[
        inoremap <silent><expr> <TAB>
              \ coc#pum#visible() ? coc#_select_confirm() :
              \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
              \ CheckBackspace() ? "\<TAB>" :
              \ coc#refresh()

        function! CheckBackspace() abort
          let col = col('.') - 1
          return !col || getline('.')[col - 1]  =~# '\s'
        endfunction

        let g:coc_snippet_next = '<tab>'
      ]]
    end
  }
}, {
  defaults = { lazy = true },
  performance = {
    cache = {
      enabled = true
    },
    rtp = {
      disabled_plugins = {
        -- "netrwPlugin",
        -- "gzip",
        -- "tarPlugin",
        -- "tohtml",
        -- "tutor",
        -- "zipPlugin",
        "editorconfig",
        "health",
        -- "matchit",
        "matchparen",
        "spellfile",
        "man",
        -- "rplugin",
        "nvim",
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
        "matchit",
        "tar",
        "tarPlugin",
        "rrhelper",
        "spellfile_plugin",
        "vimball",
        "vimballPlugin",
        "zip",
        "zipPlugin",
        "tutor",
        "rplugin",
        "syntax",
        "synmenu",
        "optwin",
        "compiler",
        "bugreport",
        "ftplugin",
      }
    }
  }
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

map({ "n", "v" }, "{", "10gk", { silent = true })
map({ "n", "v" }, "}", "10gj", { silent = true })

map("t", "<esc><esc>", "<C-\\><C-n>")

cmd([[
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
]])


-- Options
opt.history = 700
opt.signcolumn = "yes:1"
opt.autoread = true
opt.inccommand = "split"
opt.mouse = "a"
opt.shortmess = opt.shortmess + "I"
opt.hidden = true
opt.updatetime = 300
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
opt.expandtab = true
opt.lbr = true
opt.tw = 500
opt.ai = true
opt.si = true
opt.wrap = true

opt.shada = "!,'300,<50,s10,h"

opt.clipboard = "unnamedplus"
opt.undofile = true

if g.vscode then
else
  -- cmd("colorscheme dawnfox")
  cmd("colorscheme onedarkhc")
  api.nvim_set_hl(0, "Normal", { bg = "none" })
  api.nvim_set_hl(0, "NormalNC", { bg = "none" })
  -- api.nvim_set_hl(0, "Visual", { bg = "#363841" })
end


opt.foldmethod = "expr"
wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.foldenable = false
opt.foldnestmax = 20
opt.foldlevel = 9999
opt.foldcolumn = '0'
g.markdown_folding = 1
map("n", "<tab>", "za", { silent = true })


-- Statusline
local modes = {
  ["n"] = "NORMAL",
  ["no"] = "NORMAL",
  ["v"] = "VISUAL",
  ["V"] = "VISUAL LINE",
  [""] = "VISUAL BLOCK",
  ["s"] = "SELECT",
  ["S"] = "SELECT LINE",
  [""] = "SELECT BLOCK",
  ["i"] = "INSERT",
  ["ic"] = "INSERT",
  ["R"] = "REPLACE",
  ["Rv"] = "VISUAL REPLACE",
  ["c"] = "COMMAND",
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

api.nvim_exec([[ hi StatuslineAccent guibg=#95c561 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineInsertAccent guibg=#7199ee guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineVisualAccent guibg=#d7a65f guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineReplaceAccent guibg=#ee6d85 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineCmdLineAccent guibg=#95c561 guifg=#06080a ]], false)
api.nvim_exec([[ hi StatuslineTerminalAccent guibg=#95c561 guifg=#06080a ]], false)
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
  local fname = fn.expand "%:t"
  if fname == "" then
    return ""
  end
  return fname .. " "
end

local function lsp()
  local count = {}
  local levels = {
    errors = "Error",
    warnings = "Warn",
    info = "Info",
    hints = "Hint",
  }

  for k, level in pairs(levels) do
    count[k] = vim.tbl_count(vim.diagnostic.get(0, { severity = level }))
  end

  local errors = ""
  local warnings = ""
  local hints = ""
  local info = ""

  if count["errors"] ~= 0 then
    errors = " %#LspDiagnosticsSignError# " .. count["errors"]
  end
  if count["warnings"] ~= 0 then
    warnings = " %#LspDiagnosticsSignWarning# " .. count["warnings"]
  end
  if count["hints"] ~= 0 then
    hints = " %#LspDiagnosticsSignHint# " .. count["hints"]
  end
  if count["info"] ~= 0 then
    info = " %#LspDiagnosticsSignInformation# " .. count["info"]
  end

  return errors .. warnings .. hints .. info .. "%#Normal#"
end

Statusline = {}

Statusline.active = function()
  return table.concat {
    "%#Statusline#", update_mode_colors(),
    mode(),
    "%#Normal# ",
    filename(),
    "%m",
    "%#Normal#",
  }
end

function Statusline.inactive()
  return " %F"
end

api.nvim_exec([[
  augroup Statusline
  au!
  au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
  au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
  augroup END
]], false)
