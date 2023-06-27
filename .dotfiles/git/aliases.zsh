alias g='git'
alias lg='lazygit'

alias ga='git add'

alias gst='git status'
alias gsb='git status -sb'

alias gb='git branch'
alias gbd='git branch -d'
alias gbD='git branch -D'

alias gc='git commit -v'
alias gca='git commit -va'

alias gco='git checkout'
alias gcom='git checkout master'
alias gcob='git checkout -b'

alias gpu='git push'
alias gpl='git pull'
alias gpuuo='git push --set-upstream origin $(git_current_branch)'

alias gl='git log'
alias glo='git log --oneline'
alias glg='git log --graph --oneline'

alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase origin/$(git_main_branch) -i'
