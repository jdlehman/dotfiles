# Global agent instructions

Shared by every coding agent on this machine. The source of truth is
the `.agents/AGENTS.md` file in the dotfiles repo (github.com/jdlehman/dotfiles,
usually `~/code/github/jdlehman/dotfiles`; find it with `readlink ~/.claude/CLAUDE.md`). It is
symlinked to:

- `~/.claude/CLAUDE.md` (Claude Code)
- `~/.codex/AGENTS.md` (Codex)
- Muse picks up the two files above through its personal-rules fallback.

Edit the copy in the repo. Load the `dotfiles` skill before changing dotfiles,
symlinks in `$HOME`, agent settings, or skills. The skill also covers finding
the repo and cloning it onto a new machine.

## Skills

- Agent-agnostic skills live in `~/.agents/skills/<skill-id>/SKILL.md`.
  `~/.agents/skills` is a symlink to `~/code/github/jdlehman/dotfiles/.agents/skills`,
  so anything written there is tracked in dotfiles.
- Create new skills there by default. Do not put them in `~/.claude/skills`,
  `~/.codex/skills`, or `~/.config/muse/skills` unless the skill only makes sense
  for that one tool (e.g. it relies on Claude-only hooks or tool names).
- Keep skills portable: standard `SKILL.md` frontmatter (`name`, `description`,
  optionally `license` and `allowed-tools`), supporting files in subfolders
  referenced by relative path, and no tool-specific tool names in the body
  unless the skill needs them.
- Codex and Muse read `~/.agents/skills` directly. Claude Code needs one symlink
  per skill in `~/.claude/skills`; after adding or removing a skill, run
  `./install --only clean link` from the dotfiles repo to refresh them.
- Third-party skills land here too: `metavr skills install <name>` writes to
  `~/.agents/skills` (update with `metavr skills update`).
