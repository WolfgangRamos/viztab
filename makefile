test:
	emacs.exe --no-init-file --batch --directory=. --load=ert --load=viztab-test --funcall=ert-run-tests-batch-and-exit

screenshot:
	emacs.exe --no-init-file --directory=. --load=C://Users//wra//prj//viztap//viztab-demo.el --eval='(setq viztab-demo-screenshot-command "C://Users//wra//prj//viztap//Screenshooter//Screenshooter//bin//Release//Screenshooter.exe")' --eval='(setq viztab-demo-screenshot-command-args (list "-u" (format "%d,%d" (+ 11 (car (frame-position))) (+ 35 (cdr (frame-position)))) "-l" (format "%d,%d" (+ 105 (frame-outer-width)) (+ 96 (frame-outer-height))) "RainbowTable.png"))' --funcall=viztab-demo--make-rainbow-table-screenshot-and-exit
