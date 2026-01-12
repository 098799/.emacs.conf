# Emacs Configuration

Personal Emacs configuration with focus on Python development, vim-style modal editing, and modern tooling.

## Files

| File | Purpose |
|------|---------|
| `general.el` | Main config: packages, keybindings, UI |
| `elisp.el` | Custom elisp functions and text operations |
| `autoimport.el` | Python auto-import functionality |
| `gptel-custom.el` | GPT/LLM integration customizations |
| `elisp_after_general.el` | Config loaded after general.el |
| `secrets.el` | API keys (not tracked in git) |
| `check.sh` | Byte-compile and run ERT tests |

## Key Features

- **Modal editing**: `ryo-modal` for vim-style keybindings
- **LSP**: `eglot` with `ty` (fast Python type checker)
- **Completion**: `company` + `ivy` + `counsel`
- **Project management**: `projectile`
- **Git**: `magit` + `git-gutter-fringe`
- **Themes**: doom-themes (gruvbox, solarized)
- **LLM**: `gptel` for AI assistance

## Hydras

- `C-c h` - Main hydra menu
- `!` - Flycheck navigation
- `\` - Toggle modes (line numbers, whitespace, etc.)
- `fc` - Eglot actions (rename, format, code actions)

## Setup

```bash
git clone git@github.com:098799/.emacs.conf.git ~/.emacs.conf

# Create secrets.el with your API keys
cat > ~/.emacs.conf/secrets.el << 'EOF'
(setq gptel-api-key "your-key-here")
EOF

# Load from your init.el
(load "~/.emacs.conf/general.el")
```

## Testing

```bash
cd ~/.emacs.conf
./check.sh  # Byte-compile + ERT tests
```
