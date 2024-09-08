(use-package elcord
  :custom
  (elcord-boring-buffers-regexp-list '("^ " "\\\\*Messages\\\\*"
				       "\\\\*Help\\\\*" "\\\\*elpher\\\\*"
				       "\\\\*Org Src .*\\\\*"
				       "\\\\*Occur\\\\*"
				       "\\\\*Embark Collect .*\\\\*")))

(provide 'cur-config-elcord)
