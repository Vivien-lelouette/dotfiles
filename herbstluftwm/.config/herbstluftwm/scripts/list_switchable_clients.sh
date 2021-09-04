#!/usr/bin/env bash
list_all_clients=${1:-0}

client_list=$(herbstclient object_tree clients \
                  | grep -E "0x" \
                  | sed -e "s/.* 0x/0x/g" \
                  | while read -r winid;do \
                  echo \
                      $(herbstclient silent new_attr string clients.$winid.my_last_focused && herbstclient set_attr clients.$winid.my_last_focused "0") \
                      visible=$(herbstclient get_attr clients.$winid.visible) \
                      $(herbstclient get_attr clients.$winid.tag) \
                      $(herbstclient get_attr clients.$winid.my_last_focused) \
                      $winid \
                      $(herbstclient get_attr clients.$winid.class) \
                      "\"herbstclient bring $winid\"" \
                  ;done \
                  | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//')

if [ $list_all_clients -ne 1 ]
then
    client_list=$(echo "${client_list}" \
                      | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//' \
                      | grep -v "^visible=false ")
fi

client_list=$(echo "${client_list}" \
                  | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//' \
                  | cut -d' ' -f2- \
                  | sort)

client_visible_list=$(echo "${client_list}" \
                          | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//' \
                          | cut -d' ' -f1 \
                          | uniq \
                          | while read -r tagid;do \
                          herbstclient dump $tagid \
                              | sed -e 's/(//g' -e 's/)//g' -e "s/clients /\n/g" \
                              | grep "^max:" \
                              | sed "s/max://g" \
                              | while read -r index line;do \
                              echo $line \
                                  | cut -d' ' -f$(( $index + 1)) \
                              ;done \
                          ;done)

if [ $list_all_clients -ne 1 ]
then
    client_list=$(echo "${client_list}" \
                      | grep -v "$(echo "${client_visible_list}" \
    | uniq \
    | xargs echo \
    | sed -e 's/ /\\|/g' -e 's/\\|$//g')")
fi

echo "${client_list}" \
    | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//' \
    | cut -d' ' -f3- \
    | sed '/^$/d'
