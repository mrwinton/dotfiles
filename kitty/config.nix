{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    extraConfig = ''
      font_family "MesloLGS Nerd Font Mono"
      font_size 13
      bold_font auto
      italic_font auto

      clear_all_shortcuts yes
      close_on_child_death yes
      enable_audio_bell no
      remember_window_size no
      window_padding_width 5

      active_border_color none
      inactive_text_alpha 0.6

      tab_bar_edge top
      tab_bar_style separator
      tab_separator " ┇"
      tab_title_template {index}:{title}

      open_url_modifiers cmd
      url_style single
      url_color #d33682

      scrollback_lines 100000
      map cmd+shift+l show_scrollback

      copy_on_select yes
      map cmd+c copy_to_clipboard
      map cmd+v paste_from_clipboard

      map cmd+d new_window_with_cwd
      map cmd+t new_tab_with_cwd
      map cmd+] next_window
      map cmd+[ previous_window
      map cmd+w close_window

      map cmd+1 goto_tab 1
      map cmd+2 goto_tab 2
      map cmd+3 goto_tab 3
      map cmd+4 goto_tab 4
      map cmd+5 goto_tab 5
      map cmd+6 goto_tab 6
      map cmd+7 goto_tab 7
      map cmd+8 goto_tab 8
      map cmd+9 goto_tab 9

      map cmd+shift+u kitten hints
      map cmd+shift+p kitten hints --type path --program -
      map cmd+shift+h kitten hints --type hash --program -

      map cmd+equal change_font_size all +2.0
      map cmd+minus change_font_size all -2.0
      map cmd+0 change_font_size all 0

      background            #fafafa
      foreground            #5b6673
      cursor                #ff6900
      selection_background  #f0ede4
      color0                #000000
      color8                #323232
      color1                #ff3333
      color9                #ff6565
      color2                #86b200
      color10               #b8e532
      color3                #f19618
      color11               #ffc849
      color4                #41a6d9
      color12               #73d7ff
      color5                #f07078
      color13               #ffa3aa
      color6                #4cbe99
      color14               #7ff0cb
      color7                #ffffff
      color15               #ffffff
      selection_foreground  #fafafa
    '';
  };
}
