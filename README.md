# Ripgrep Completion at Point Function

Use [ripgrep](https://github.com/BurntSushi/ripgrep) to look for completion candidates.

## Installation

### Straight and use-package

```el
(use-package ripgrep-capf
  :straight (:host github :repo "aaronjensen/ripgrep-capf"))
```

## Usage

```el
(setq completion-at-point-functions '(ripgrep-capf))
```
