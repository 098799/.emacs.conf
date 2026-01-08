# Emacs Config Improvement Tasks

## High Priority

- [x] **Enable lexical-binding in all .el files** _(done)_
  - Changed all files to `;;; filename.el --- Description -*- lexical-binding: t -*-`
  - Files updated: `general.el`, `elisp.el`, `gptel-custom.el`, `elisp_after_general.el`, `autoimport.el`

- [x] **Fix duplicate function definition in autoimport.el** _(done)_
  - Removed the first (incomplete) definition, kept the one with shell fallback

- [x] **Update .gitignore** _(done)_
  - Added: `#*#`, `*~`, `*.elc`, `*~undo-tree~`, `.stfolder/`, `.claude/`

## Medium Priority

- [x] **Add proper Elisp file headers** _(done)_
  - All files now have proper headers with lexical-binding

- [x] **Add byte-compilation check script** _(done)_
  - Created `check.sh` - run `./check.sh` to check all files
  - Run `./check.sh --verbose` to see warnings

## Low Priority (Cleanup)

- [x] **Fix deprecated patterns** _(done)_
  - Replaced `defvar` + `setq-local` with proper `let` bindings
  - Fixed: `my-backward-word`, `my-forward-word`, `find-string-delimiter`, `count-initial-spaces`, `how-many-lines-with-same-indent`

- [x] **Add basic ERT tests for critical functions** _(done)_
  - Created `test-elisp.el` with 12 tests
  - Tests for `my-forward-word`, `my-backward-word` boundary behavior
  - Tests for `is-beginning-of-word`, `is-end-of-word`
  - Tests for `autoimport` lookup and insertion
  - Run: `emacs -batch -l ert -l elisp.el -l autoimport.el -l test-elisp.el -f ert-run-tests-batch-and-exit`

## Future Considerations

- [ ] **Consider using Eask or makem.sh for automation**
  - Lint, byte-compile, test in one command
  - Could integrate with CI if desired

- [ ] **Elsa static analysis** (probably overkill)
  - Type checking for Elisp
  - Dead code detection

## Notes

- This is a personal config, not a MELPA package - don't need full package-lint compliance
- Syncthing keeps this synced between work laptop and home desktop
- secrets.el is already gitignored (contains API keys)
