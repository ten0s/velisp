#!/bin/bash -e

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <DEST_DIR>"
    exit 1
fi

DEST_DIR=$1

mkdir -p $DEST_DIR/mingw64/bin

cp /mingw64/bin/libatk-1.0-0.dll          $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libbrotlicommon.dll       $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libbrotlidec.dll          $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libbz2-1.dll              $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libcairo-2.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libcairo-gobject-2.dll    $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libdatrie-1.dll           $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libepoxy-0.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libexpat-1.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libffi-7.dll              $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libfontconfig-1.dll       $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libfreetype-6.dll         $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libfribidi-0.dll          $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgcc_s_seh-1.dll        $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgdk_pixbuf-2.0-0.dll   $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgdk-3-0.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgio-2.0-0.dll          $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgirepository-1.0-1.dll $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libglib-2.0-0.dll         $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgmodule-2.0-0.dll      $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgobject-2.0-0.dll      $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgraphite2.dll          $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libgtk-3-0.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libharfbuzz-0.dll         $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libharfbuzz-gobject-0.dll $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libiconv-2.dll            $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libintl-8.dll             $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpango-1.0-0.dll        $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpangocairo-1.0-0.dll   $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpangoft2-1.0-0.dll     $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpangowin32-1.0-0.dll   $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpcre-1.dll             $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpixman-1-0.dll         $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libpng16-16.dll           $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libstdc++-6.dll           $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libthai-0.dll             $DEST_DIR/mingw64/bin/
cp /mingw64/bin/libwinpthread-1.dll       $DEST_DIR/mingw64/bin/
cp /mingw64/bin/zlib1.dll                 $DEST_DIR/mingw64/bin/

mkdir -p $DEST_DIR/mingw64/lib/girepository-1.0

cp /mingw64/lib/girepository-1.0/Atk-1.0.typelib          $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/cairo-1.0.typelib        $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/freetype2-2.0.typelib    $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/Gdk-3.0.typelib          $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/GdkPixbuf-2.0.typelib    $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/Gio-2.0.typelib          $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/GIRepository-2.0.typelib $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/GLib-2.0.typelib         $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/GModule-2.0.typelib      $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/GObject-2.0.typelib      $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/Gtk-3.0.typelib          $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/HarfBuzz-0.0.typelib     $DEST_DIR/mingw64/lib/girepository-1.0/
cp /mingw64/lib/girepository-1.0/Pango-1.0.typelib        $DEST_DIR/mingw64/lib/girepository-1.0/

exit 0
