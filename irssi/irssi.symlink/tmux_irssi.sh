#!/bin/sh

T3=$(pidof irssi)

irssi_nickpane() {
  tmux setw main-pane-width $(( 111 - 26));
  #tmux setw main-pane-width $(( $(tput cols) - 26));
  tmux send-keys -t irssi "tmux send-keys -t0 \"irssi\" C-m; \ 
  tmux send-keys -t0 \"/set nicklist_height 55\" C-m" C-m 
  #tmux send-keys -t0 \"/set nicklist_height $(tput lines)\" C-m" C-m 
  tmux splitw -v "cut -c -25 ~/.irssi/nicklistfifo";
  tmux selectl main-vertical;
  tmux selectw -t irssi;
  tmux selectp -t 0;
}

irssi_repair() {
  tmux selectw -t irssi
  (( $(tmux lsp | wc -l) > 1 )) && tmux killp -a -t 0
  irssi_nickpane
}

if [ -z "$T3" ]; then
  tmux new-session -d -s main;
  tmux new-window -t main -n irssi;
  irssi_nickpane ;
tmux attach-session -d -t main;
exit 0
fi
tmux attach-session -d -t main;
irssi_repair ;
exit 0
