((nil . ((eval . (add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode)))
;; It needs to be haskell-language-server, NOT haskell-language-server wrapper,
;; because that could pull a globally-installed version of HLS instead of the
;; nix-installed one that we want
         (lsp-haskell-server-path . "haskell-language-server"))))
