{
  flake.modules.nvf.keymaps = {
    vim.keymaps = [
      # file bindings
      {
        key = "<leader>fs";
        mode = [ "n" "v" ];
        action = ":w<CR>";
        desc = "Save file";
      }
      {
        key = "<leader>.";
        mode = [ "n" "v" ];
        action = ":Telescope file_browser path=%:p:h select_buffer=true<CR>";
        desc = "Find files in cwd [Telescope]";
      }
      {
        key = "<leader>pf";
        mode = [ "n" "v" ];
        action = ":Telescope find_files<CR>";
        desc = "Find file [Telescope]";
      }

      # buffer bindings
      {
        key = "<leader>bp";
        mode = [ "n" "v" ];
        action = ":bprevious<CR>";
        desc = "Go to previous buffer";
      }
      {
        key = "<leader>bn";
        mode = [ "n" "v" ];
        action = ":bnext<CR>";
        desc = "Go to next buffer";
      }

      # git
      {
        key = "<leader>gg";
        mode = [ "n" "v" ];
        action = ":Neogit<CR>";
        desc = "Git Status [Neogit]";
      }

      # window bindings
      {
        key = "<leader>ww";
        mode = [ "n" "v" ];
        action = "<C-w>w";
        desc = "Switch window focus";
      }
      {
        key = "<leader>wv";
        mode = [ "n" "v" ];
        action = "<C-w>v";
        desc = "Open vertical split";
      }
      {
        key = "<leader>wn";
        mode = [ "n" "v" ];
        action = "<C-w>n";
        desc = "Open horizontal split";
      }
      {
        key = "<leader>wc";
        mode = [ "n" "v" ];
        action = "<C-w>c";
        desc = "Close current window";
      }
      {
        key = "<leader>w1";
        mode = [ "n" "v" ];
        action = "<C-w>o";
        desc = "Close other windows";
      }
      {
        key = "<leader>w0";
        mode = [ "n" "v" ];
        action = "<C-w>o";
        desc = "Close current window";
      }

      # miscellaneous
      {
        key = "<leader>ot";
        mode = [ "n" "v" ];
        action = ":ToggleTerm<CR>";
        desc = "Toggle terminal";
      }
      {
        key = "<leader>oe";
        mode = [ "n" "v" ];
        action = ":Explore<CR>";
        desc = "Open NetRW (File Explorer)";
      }

      # yanky
      {
        key = "<leader>y";
        mode = [ "n" "v" ];
        action = "<cmd>YankyRingHistory<CR>";
        desc = "Open Yank History";
      }
      {
        key = "y";
        action = "<Plug>(YankyYank)";
        mode = [ "n" "x" ];
        desc = "Yank text";
      }
      {
        key = "p";
        action = "<Plug>(YankyPutAfter)";
        mode = [ "n" "x" ];
        desc = "Put yanked text after cursor";
      }
      {
        key = "P";
        action = "<Plug>(YankyPutBefore)";
        mode = [ "n" "x" ];
        desc = "Put yanked text before cursor";
      }
      {
        key = "gp";
        action = "<Plug>(YankyGPutAfter)";
        mode = [ "n" "x" ];
        desc = "Put yanked text after selection";
      }
      {
        key = "gP";
        action = "<Plug>(YankyGPutBefore)";
        mode = [ "n" "x" ];
        desc = "Put yanked text before selection";
      }
      {
        key = "<c-p>";
        action = "<Plug>(YankyPreviousEntry)";
        desc = "Select previous entry through yank history";
        mode = [ "n" "v" ];
      }
      {
        key = "<c-n>";
        action = "<Plug>(YankyNextEntry)";
        desc = "Select next entry through yank history";
        mode = [ "n" "v" ];
      }
      {
        key = "]p";
        action = "<Plug>(YankyPutIndentAfterLinewise)";
        desc = "Put indented after cursor (linewise)";
        mode = [ "n" "v" ];
      }
      {
        key = "[p";
        action = "<Plug>(YankyPutIndentBeforeLinewise)";
        desc = "Put indented before cursor (linewise)";
        mode = [ "n" "v" ];
      }
      {
        key = "]P";
        action = "<Plug>(YankyPutIndentAfterLinewise)";
        desc = "Put indented after cursor (linewise)";
        mode = [ "n" "v" ];
      }
      {
        key = "[P";
        action = "<Plug>(YankyPutIndentBeforeLinewise)";
        desc = "Put indented before cursor (linewise)";
        mode = [ "n" "v" ];
      }
      {
        key = ">p";
        action = "<Plug>(YankyPutIndentAfterShiftRight)";
        desc = "Put and indent right";
        mode = [ "n" "v" ];
      }
      {
        key = "<p";
        action = "<Plug>(YankyPutIndentAfterShiftLeft)";
        desc = "Put and indent left";
        mode = [ "n" "v" ];
      }
      {
        key = ">P";
        action = "<Plug>(YankyPutIndentBeforeShiftRight)";
        desc = "Put before and indent right";
        mode = [ "n" "v" ];
      }
      {
        key = "<P";
        action = "<Plug>(YankyPutIndentBeforeShiftLeft)";
        desc = "Put before and indent left";
        mode = [ "n" "v" ];
      }
      {
        key = "=p";
        action = "<Plug>(YankyPutAfterFilter)";
        desc = "Put after applying a filter";
        mode = [ "n" ];
      }
      {
        key = "=P";
        action = "<Plug>(YankyPutBeforeFilter)";
        desc = "Put before applying a filter";
        mode = [ "n" ];
      }
    ];
  };
}
