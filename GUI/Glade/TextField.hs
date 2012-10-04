{- |
Module      :  GUI.Glade.TextField
Description :  Glade xmlstring for TextField
Copyright   :  (c) Thiemo Wiedemeyer, Uni Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  raider@informatik.uni-bremen.de
Stability   :  provisional
Portability :  portable

This module provides a string containing the xml data of the glade file for:
TextField

This module is automatically created.
-}

module GUI.Glade.TextField (get)
where

get :: (String, String)
get = ("TextField", xmlString)

xmlString :: String
xmlString =

 "<?xml version=\"1.0\"?>\n<glade-interface>\n  <!-- interface-requires gtk+ 2.8 -->\n  <!-- interface-naming-policy toplevel-contextual -->\n  <widget class=\"GtkWindow\" id=\"TextField\">\n    <property name=\"default_width\">500</property>\n    <property name=\"default_height\">60</property>\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox1\">\n        <property name=\"visible\">True</property>\n        <child>\n          <widget class=\"GtkEntry\" id=\"entry\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">True</property>\n          </widget>\n        </child>\n        <child>\n          <widget class=\"GtkHButtonBox\" id=\"hbuttonbox1\">\n            <property name=\"visible\">True</property>\n            <property name=\"layout_style\">end</property>\n            <child>\n              <widget class=\"GtkButton\" id=\"abort\">\n                <property name=\"label\">Abort</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </widget>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"add\">\n                <property name=\"label\">Add</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </widget>\n            </child>\n          </widget>\n        </child>\n      </widget>\n    </child>\n  </widget>\n</glade-interface>\n"
