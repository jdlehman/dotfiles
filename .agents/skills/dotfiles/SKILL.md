---
name: dotfiles
description: >-
  Manage the user's dotfiles repo (dotbot-based; github.com/jdlehman/dotfiles)
  and the shared coding-agent config it owns, including locating the repo via
  its symlinks or cloning it onto a new machine. Use when adding, moving, or removing
  a config file or symlink in $HOME; creating, installing, or removing an agent
  skill; changing Claude Code, Codex, or Muse settings or global instructions
  (CLAUDE.md / AGENTS.md); adding a Homebrew package to the setup; or when the
  user mentions dotfiles, install.conf.yaml, dotbot, ~/.agents, or "my config".
---

# Dotfiles

The user's dotfiles repo is `git@github.com:jdlehman/dotfiles.git`
(https://github.com/jdlehman/dotfiles). Its usual location is
`~/code/github/jdlehman/dotfiles`. Files in `$HOME` such as `~/.zshrc`,
`~/.config/nvim`, and the coding-agent config are **symlinks into this repo**.
Edit the repo copy. Do not replace a symlink with a regular file.

## Find the repo

Don't assume the path. Resolve it from a symlink that the repo manages:

```bash
DOTFILES="$(git -C "$(dirname "$(readlink ~/.zshrc)")" rev-parse --show-toplevel 2>/dev/null)"
echo "$DOTFILES"
```

dotbot creates absolute links, so a single `readlink` is enough. If `~/.zshrc`
is not a link, try another managed path: `readlink ~/.gitconfig`,
`readlink ~/.agents/skills`, or `readlink ~/.config/nvim`. To find which repo
file backs a given path in `$HOME`, run `ls -la <path>`. Then run every git
command against the repo itself (`git -C "$DOTFILES" ...`), not against the
symlinked path in `$HOME`.

If none of those paths are symlinks and `~/code/github/jdlehman/dotfiles`
doesn't exist, the dotfiles are not on this machine. Go to the next section.

## Set up on a new machine

Ask the user before doing this. It clones a repo and changes files in `$HOME`.

```bash
mkdir -p ~/code/github/jdlehman
git clone --recurse-submodules git@github.com:jdlehman/dotfiles.git ~/code/github/jdlehman/dotfiles
# no GitHub SSH key yet? clone over HTTPS: https://github.com/jdlehman/dotfiles.git
cd ~/code/github/jdlehman/dotfiles
./install --only create link clean     # symlinks only
```

- Run the bare `./install` only if the user wants the full setup: Homebrew,
  brew and cask packages, pip packages, and tmux plugins. Expect it to take a
  while.
- dotbot won't replace existing real files such as a default `~/.zshrc` or
  `~/.claude/settings.json`, so the install reports failures for them. List
  them for the user. For each one, either back it up
  (`mv ~/.zshrc ~/.zshrc.pre-dotfiles`) or merge its useful settings into the
  repo copy. Then rerun the link step. Never delete these files silently.
- After linking, the shared skills (this one included) and `AGENTS.md` are
  active for every agent. Restart open agent sessions to load them.
- On Windows, run `install.ps1`. It does links only; there is no brew plugin.

## How it works

- [dotbot](https://github.com/anishathalye/dotbot) (git submodule `dotbot/`)
  reads `install.conf.yaml` and creates symlinks. `dotbot-brew/` is a plugin
  that adds the `tap` / `brew` / `cask` directives.
- `./install` (macOS/Linux) and `install.ps1` (Windows, no brew plugin) run
  dotbot against `install.conf.yaml`.
- `install.conf.yaml` is the source of truth for what links where. Read it
  instead of guessing. Directives run in order: `defaults` (`relink: true`),
  `clean` (removes dead links that point into the repo), `create` (makes
  directories), `link`, then `shell`, `tap`, `brew`, and `cask`.
- Repo convention: top-level folders have no leading dot and map to dotted
  paths in `$HOME` (`zsh/` → `~/.zsh`, `git/gitconfig` → `~/.gitconfig`).
  `.agents/` is the exception because it mirrors `~/.agents`.

## Running install safely

A bare `./install` also runs Homebrew, `pip3 install`, submodule updates, and
tmux plugin installs. **Don't run it without asking.** Run only the steps you
need:

```bash
cd "$DOTFILES"   # see "Find the repo"
./install --only create link          # after adding or changing link entries
./install --only clean link           # after adding or removing a skill
```

dotbot will **not** overwrite a real file or directory at a link destination,
and it will fail instead. Don't add `force: true`. Move the existing file into
the repo yourself (see below), then link it.

## Coding agents: `.agents/`

| Repo path | Linked to | Read by |
|---|---|---|
| `.agents/AGENTS.md` | `~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md` | Claude Code, Codex; Muse via its personal-rules fallback |
| `.agents/skills/` | `~/.agents/skills` (whole dir) | Codex, Muse, `metavr skills install` |
| `.agents/skills/*` | `~/.claude/skills/<id>` (one link per skill, glob) | Claude Code |
| `.agents/claude/settings.json` | `~/.claude/settings.json` | Claude Code |
| `.agents/codex/config.toml` | `~/.codex/config.toml` | Codex |
| `.agents/muse/settings.json` | `~/.config/muse/settings.json` | Muse |

`.agents/AGENTS.md` holds the global instructions shared by every agent. Keep
it tool-neutral. When a rule applies to only one tool, say which tool.

### Add a skill

1. Decide where it goes. A skill that works in any agent goes in
   `.agents/skills/<id>/`, which is the default. Use a tool's own skills
   folder only when the skill depends on that tool, for example Claude-only
   hooks or tool names.
2. Write `.agents/skills/<id>/SKILL.md`:
   - Frontmatter needs `name` (must equal `<id>`: lowercase, hyphens) and a
     `description` that says what the skill does and when to use it.
   - Put longer reference material in `references/` or `resources/`. Link it
     from `SKILL.md` with relative paths.
   - Keep the body portable. Use plain shell commands and avoid
     tool-specific tool names unless the skill needs them.
3. Run `./install --only clean link` so Claude Code gets the new
   `~/.claude/skills/<id>` link. Codex and Muse read `~/.agents/skills`
   directly, so they need no step.
4. Check it: `ls -la ~/.claude/skills/<id>`. For Muse, run
   `muse skills list --source user` from any directory other than `$HOME`.
   In `$HOME`, `~/.agents/skills` also counts as a project folder, so Muse
   lists the skills as shadowed.

**Third-party skills:** `metavr skills install <name>` (and `metavr skills update`)
write into `~/.agents/skills`, which is this repo. After installing one, run the
`clean link` step to link it for Claude Code.

**Remove a skill:** delete `.agents/skills/<id>/` (use `metavr skills remove <id>`
for skills that metavr installed), then run `./install --only clean link`.

**Glob caveat:** the `~/.claude/skills/` glob link fails when it matches
exactly one skill (dotbot calls that "ambiguous") or none. Keep at least two
skills in `.agents/skills/`. If that ever can't hold, change the entry to a
plain link.

### Change agent settings or instructions

Edit the file under `.agents/`. If a tool saves its settings by writing a new
file in place of the old one, the symlink can be replaced by a regular file.
Check with `ls -la ~/.claude/settings.json ~/.config/muse/settings.json`. If a
link has become a regular file, copy any new values into the repo copy, delete
the regular file, and rerun `./install --only link`.

### Add a new agent tool

Create `.agents/<tool>/` containing only the user-authored config. Add a
`create:` entry for the tool's config directory and `link:` entries for each
file, then run `./install --only create link`. If the tool can read
`~/.agents/skills` or `AGENTS.md`, point it at the shared copies and don't
duplicate them.

## Adopt an existing config file

```bash
cd "$DOTFILES"
mkdir -p <tool> && cp -p ~/.<file> <tool>/<file>
cmp ~/.<file> <tool>/<file> && rm ~/.<file>   # only remove after verifying the copy
# add "~/.<file>: <tool>/<file>" under link: in install.conf.yaml
./install --only create link
```

For a whole directory, `mv` it into the repo, then link the directory.

## Homebrew packages

Add the formula to the `brew:` list in `install.conf.yaml` (or `cask:` / `tap:`).
To install it now, run `brew install <name>` directly. Don't run the full
`./install` for this.

## Never put these in the repo

The repo is public on GitHub. Never add: credentials or tokens (`auth.json`,
`~/.codex/auth.json`, `.credentials*`, API keys), trust lists (`trust.json`),
or session data (`history.jsonl`, `sessions/`, `projects/`, caches, logs).
Link individual config files rather than whole tool directories such as
`~/.claude` or `~/.codex`, because those directories hold secrets and state.

## Git

- Commit or push only when the user asks. The working tree often has
  unrelated work in progress, so stage only the paths you changed
  (`git add <paths>`). Never use `git add -A`.
- Commit subject style: `[area] Imperative summary`, e.g.
  `[nvim] Make paths compatible on windows`, `[agents] Add dotfiles skill`.
- `dotbot`, `dotbot-brew`, and `tmux/tpm` are submodules. Don't edit inside
  them.
