#Это все попытки заставить работать java приложения в xmonad
#wmname LG3D 
#export XDG_CURRENT_DESKTOP = XFCE
export _JAVA_AWT_WM_NONREPARENTING=1

export LANG=ru_RU.UTF-8
export MM_CHARSET=UTF-8
xsetroot -cursor_name left_ptr

xmobar /home/igor/.xmonad/.xmobarr_bottom  &
#/usr/bin/nemo -n &  #файловий менеджер з прорисовкою робочого стола
#xfdesktop &
#unity-2d-shell &
#feh --bg-fill --auto-zoom --full-screen --hide-pointer --randomize --slideshow-delay 5  /home/igor/Pictures/Wallpapers/xmonad/*.png /home/igor/Pictures/Wallpapers/xmonad/*.jpg &
#feh --bg-fill /home/igor/Pictures/Wallpapers/xmonad/xmonad.png &
#feh --bg-fill /home/igor/Xwallpapers/3.jpg &
xfsettingsd --sm-client-disable & # Это демон xfce для того чтобы настройки тем и шрифтов XFCE сохранялись в xmonad
xfce4-panel & # панелька с системным лотком и датчиком температуры HDD от графической оболочки XFCE
#/usr/bin/nemo -n &
#plasma-desktop &
xfce4-volumed &
#volumeicon &
xfce4-notifyd & #уведомление о различных событиях
nm-applet & #апплет управления сетями
#ejecter & #апплет для извлечения сьемных носителей
unclutter -idle 3 & #прячет курсор мыши при остутствии активности
#gnome-settings-daemon & #апплет для настройки переключения раскладок клавиатур
#/opt/extras.ubuntu.com/touchpad-indicator/bin/touchpad-indicator &
touchpad-indicator & #индикатор для отключения тачпада
xfce4-power-manager & #апплет управления питанием
#gnome-sound-applet & #апплет управления громкостью
#/usr/share/indicator-virtual-box/indicator-virtual-box.py & # аплет виртуалбокса
caffeine-indicator & # Чтобы те тух экран при просмотре фильмов
#kbdd & # запоминает раскладку для каждого окна
#lookit & # апплет для снятия скриншотов
numlockx on & # включает цифровую клавиатуру
#conky & # включает цифровую клавиатуру
#psensor &
#indicator-stickynotes &
#pastie & # аплет для буфера обмена
#indicator-cpufreq & #апплет для изменения тактовой частоты процессора
#export LM_LICENSE_FILE=/usr/Siemens/PLMLicenseServer/splm6.lic &
#/usr/Siemens/PLMLicenseServer/lmgrd &
#sleep 8s; spacefm --desktop & # прорисовка рабочего стола
#window-list &
#classicmenu-indicator & #индикатор классического меню Gnome
nitrogen --restore & #восстановление картинки на рабочем столе с предыдущего сеанса
#conky & # табличка справа
#cd && rm .goutputstream-* -v &
#setxkbmap -layout 'ua,ru' -option grp:alt_shift_toggle,grp_led:scroll & # переключение раскладки по капсу
#krusader & #двухпараллельный файловый менеджер от KDE
#chromium-browser & # браузер 
#thunderbird & # почтовая программа 
#trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 14 --transparent true --alpha 0 --tint 0x000000 --height 15 &






