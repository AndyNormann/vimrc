vim.loader.enable()
local o = vim.opt
local g = vim.g
local map = vim.keymap.set
local pack = vim.pack

g.mapleader = " "
g.localleader = " "

pack.add({
  -- { src = "https://github.com/everviolet/nvim",                 name = "evergarden" },
  { src = "https://github.com/folke/tokyonight.nvim" },
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
  { src = "https://github.com/neovim/nvim-lspconfig" },
  { src = "https://github.com/mason-org/mason.nvim" },
  { src = "https://github.com/mason-org/mason-lspconfig.nvim" },
  { src = "https://github.com/folke/snacks.nvim" },
  { src = "https://github.com/stevearc/oil.nvim" },
  { src = "https://github.com/echasnovski/mini.nvim" },
  { src = "https://github.com/stevearc/conform.nvim" },
  { src = "https://github.com/L3MON4D3/LuaSnip" },
  { src = "https://github.com/stevearc/overseer.nvim" },
  { src = "https://github.com/Saghen/blink.cmp",                version = "v1.7.0" },
})

if vim.g.neovide then
  vim.opt.linespace = -5
  vim.g.neovide_position_animation_length = 0
  vim.g.neovide_scroll_animation_length = 0
  vim.g.neovide_scroll_animation_far_lines = 0
  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_cursor_short_animation_length = 0
  vim.g.neovide_cursor_trail_size = 0
  vim.g.neovide_refresh_rate = 100
end

require("tokyonight").setup({
  style = "night",
  transparent = true,
  terminal_colors = true
})

vim.cmd.colorscheme("tokyonight")
vim.cmd('highlight Visual gui=reverse cterm=reverse')

require("nvim-treesitter.configs").setup({ highlight = { enable = true } })

vim.schedule(function()
  require("snacks").setup({
    scope = { enabled = true },
    input = { enabled = true },
    notifier = { enabled = true },
  })

  require("mason").setup()
  require("mason-lspconfig").setup()

  require("conform").setup({
    format_on_save = {
      timeout_ms = 500,
      lsp_format = "fallback",
    },
    formatters_by_ft = {
      typescript = { "prettierd", "prettier", stop_after_first = true },
      typescriptreact = { "prettierd", "prettier", stop_after_first = true },
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

  require("oil").setup({
    show_hidden = true
  })
  require("mini.surround").setup()
  require("mini.pairs").setup()
  require("blink.cmp").setup({
    completion = { documentation = { auto_show = true }, list = { selection = { preselect = false, auto_insert = true } } },
    sources = {
      default = { 'lsp', 'path', 'buffer' },
    },

    signature = { enabled = true },
    fuzzy = { implementation = "prefer_rust_with_warning" },
    keymap = {
      ['<C-CR>'] = { 'accept', 'fallback' },
    }
  })
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
    if client then
      client.server_capabilities.semanticTokensProvider = nil
    end
    -- vim.lsp.completion.enable(true, args.data.client_id, 0, { autotrigger = true })
  end,
})

vim.lsp.document_color.enable()
vim.lsp.on_type_formatting.enable()

-- Options
o.laststatus = 0
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.undofile = true
o.mouse = "a"
o.backup = false
o.breakindent = true
o.ruler = false
o.signcolumn = "yes"
o.writebackup = false
o.shell = "fish"
o.number = false
o.winborder = "single"
o.virtualedit = "block"
o.formatoptions = "qjl1"
o.cursorline = false
o.ignorecase = true
o.incsearch = true
o.infercase = true
o.smartcase = true
o.smartindent = true
o.completeopt = "noselect,menuone"
o.clipboard = "unnamedplus"
o.showmode = false
o.shortmess = "IWATFsScnxC"
o.termguicolors = true
o.pumheight = 10

-- Keymaps
map("n", "<Leader><cr>", ":noh<cr>", { silent = true })
map({ "n", "v" }, "0", "^")

map({ "n", "v" }, "j", "gj")
map({ "n", "v" }, "k", "gk")

map("n", ";", ":")
map("n", ":", ";")
map("n", "<Tab>", "za")

map({ "n", "v" }, "{", "15gk", { silent = true })
map({ "n", "v" }, "}", "15gj", { silent = true })
map("t", "<esc>", "<C-\\><C-n>", { silent = true })
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", { silent = true })
map("i", "<C-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", { noremap = true, silent = true })

map("n", "<leader>e", "<cmd>lua Snacks.picker.files()<cr>", { silent = true })
map("n", "<leader>f", "<cmd>lua Snacks.picker.grep()<cr>", { silent = true })
map("n", "<leader>b", "<cmd>lua Snacks.picker.buffers()<cr>", { silent = true })
map("n", "<leader>r", "<cmd>lua Snacks.picker.resume()<cr>", { silent = true })
map("n", "-", "<cmd>Oil<cr>", { silent = true })
