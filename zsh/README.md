# zsh structure

The zsh load structure is worth discussing briefly. It will source all files zsh files within the `zsh` directory that are not in the form `*completion.zsh`. Then it will add completions to the path, `$fpath`, and source all zsh files in the form `*completion.zsh`

You can create a `.local.zsh` file to source environment variables or setup anything you might need for that directory/project.

There also exists a `custom.zsh` file in this repository, where you can store machine specific settings that you might not want to commit to the repo.
