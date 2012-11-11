{- |
Module      :  GUI.Glade.NodeChecker
Description :  Glade xmlstring for NodeChecker
Copyright   :  (c) Thiemo Wiedemeyer, Uni Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  raider@informatik.uni-bremen.de
Stability   :  provisional
Portability :  portable

This module provides a string containing the xml data of the glade file for:
NodeChecker

This module is automatically created.
-}

module GUI.Glade.NodeChecker (get)
where

get :: (String, String)
get = ("NodeChecker", xmlString)

xmlString :: String
xmlString =

 "<?xml version=\"1.0\"?>\n<glade-interface>\n  <!-- interface-requires gtk+ 2.8 -->\n  <!-- interface-naming-policy project-wide -->\n  <widget class=\"GtkWindow\" id=\"NodeChecker\">\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox3\">\n        <property name=\"visible\">True</property>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox1\">\n            <property name=\"visible\">True</property>\n            <child>\n              <widget class=\"GtkVBox\" id=\"vbox4\">\n                <property name=\"visible\">True</property>\n                <child>\n                  <widget class=\"GtkFrame\" id=\"frame2\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <widget class=\"GtkAlignment\" id=\"alignment2\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"top_padding\">6</property>\n                        <property name=\"bottom_padding\">6</property>\n                        <property name=\"left_padding\">6</property>\n                        <property name=\"right_padding\">6</property>\n                        <child>\n                          <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow2\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"hscrollbar_policy\">automatic</property>\n                            <property name=\"vscrollbar_policy\">automatic</property>\n                            <child>\n                              <widget class=\"GtkTreeView\" id=\"trvNodes\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                              </widget>\n                            </child>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkLabel\" id=\"label2\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"label\" translatable=\"yes\">Nodes or goals:</property>\n                        <property name=\"use_markup\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"type\">label_item</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"position\">0</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkHButtonBox\" id=\"hbuttonbox6\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"layout_style\">start</property>\n                    <child>\n                      <widget class=\"GtkButton\" id=\"btnNodesAll\">\n                        <property name=\"label\" translatable=\"yes\">All</property>\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"receives_default\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"fill\">False</property>\n                        <property name=\"position\">0</property>\n                      </packing>\n                    </child>\n                    <child>\n                      <widget class=\"GtkButton\" id=\"btnNodesNone\">\n                        <property name=\"label\" translatable=\"yes\">None</property>\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"receives_default\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"fill\">False</property>\n                        <property name=\"position\">1</property>\n                      </packing>\n                    </child>\n                    <child>\n                      <widget class=\"GtkButton\" id=\"btnNodesInvert\">\n                        <property name=\"label\" translatable=\"yes\">Invert</property>\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"receives_default\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"fill\">False</property>\n                        <property name=\"position\">2</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"position\">1</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkHButtonBox\" id=\"hbuttonbox4\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"layout_style\">start</property>\n                    <child>\n                      <widget class=\"GtkButton\" id=\"btnNodesUnchecked\">\n                        <property name=\"label\" translatable=\"yes\">Unchecked</property>\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"receives_default\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"fill\">False</property>\n                        <property name=\"position\">0</property>\n                      </packing>\n                    </child>\n                    <child>\n                      <widget class=\"GtkButton\" id=\"btnNodesTimeout\">\n                        <property name=\"label\" translatable=\"yes\">Timedout</property>\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"receives_default\">True</property>\n                        <property name=\"yalign\">0.52999997138977051</property>\n                      </widget>\n                      <packing>\n                        <property name=\"expand\">False</property>\n                        <property name=\"fill\">False</property>\n                        <property name=\"position\">1</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"position\">2</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkVBox\" id=\"vbox1\">\n                <property name=\"visible\">True</property>\n                <child>\n                  <widget class=\"GtkFrame\" id=\"frame3\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <widget class=\"GtkAlignment\" id=\"alignment7\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"top_padding\">3</property>\n                        <property name=\"bottom_padding\">6</property>\n                        <property name=\"left_padding\">12</property>\n                        <child>\n                          <widget class=\"GtkVBox\" id=\"vbox2\">\n                            <property name=\"visible\">True</property>\n                            <child>\n                              <widget class=\"GtkHBox\" id=\"hbox4\">\n                                <property name=\"visible\">True</property>\n                                <child>\n                                  <widget class=\"GtkLabel\" id=\"label6\">\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"label\" translatable=\"yes\">Timeout:</property>\n                                  </widget>\n                                  <packing>\n                                    <property name=\"expand\">False</property>\n                                    <property name=\"position\">0</property>\n                                  </packing>\n                                </child>\n                                <child>\n                                  <widget class=\"GtkAlignment\" id=\"alignment8\">\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"left_padding\">10</property>\n                                    <child>\n                                      <widget class=\"GtkSpinButton\" id=\"sbTimeout\">\n                                        <property name=\"visible\">True</property>\n                                        <property name=\"can_focus\">True</property>\n                                        <property name=\"width_chars\">5</property>\n                                        <property name=\"adjustment\">20 1 3600 1 0 0</property>\n                                        <property name=\"climb_rate\">1</property>\n                                      </widget>\n                                    </child>\n                                  </widget>\n                                  <packing>\n                                    <property name=\"expand\">False</property>\n                                    <property name=\"pack_type\">end</property>\n                                    <property name=\"position\">1</property>\n                                  </packing>\n                                </child>\n                              </widget>\n                              <packing>\n                                <property name=\"position\">0</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkCheckButton\" id=\"cbInclThms\">\n                                <property name=\"label\" translatable=\"yes\">Include Theorems</property>\n                                <property name=\"visible\">True</property>\n                                <property name=\"active\">True</property>\n                                <property name=\"draw_indicator\">True</property>\n                              </widget>\n                              <packing>\n                                <property name=\"position\">1</property>\n                              </packing>\n                            </child>\n                            <child>\n                              <widget class=\"GtkHButtonBox\" id=\"hbuttonbox2\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"layout_style\">end</property>\n                                <child>\n                                  <widget class=\"GtkButton\" id=\"btnStop\">\n                                    <property name=\"label\" translatable=\"yes\">Stop</property>\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"can_focus\">True</property>\n                                    <property name=\"receives_default\">True</property>\n                                  </widget>\n                                  <packing>\n                                    <property name=\"expand\">False</property>\n                                    <property name=\"fill\">False</property>\n                                    <property name=\"position\">0</property>\n                                  </packing>\n                                </child>\n                                <child>\n                                  <widget class=\"GtkButton\" id=\"btnCheck\">\n                                    <property name=\"label\" translatable=\"yes\">Check</property>\n                                    <property name=\"visible\">True</property>\n                                    <property name=\"can_focus\">True</property>\n                                    <property name=\"receives_default\">True</property>\n                                  </widget>\n                                  <packing>\n                                    <property name=\"expand\">False</property>\n                                    <property name=\"fill\">False</property>\n                                    <property name=\"pack_type\">end</property>\n                                    <property name=\"position\">1</property>\n                                  </packing>\n                                </child>\n                              </widget>\n                              <packing>\n                                <property name=\"position\">2</property>\n                              </packing>\n                            </child>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkLabel\" id=\"label4\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"label\" translatable=\"yes\">Checker Configuration:</property>\n                        <property name=\"use_markup\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"type\">label_item</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"position\">0</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkFrame\" id=\"frame6\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <widget class=\"GtkAlignment\" id=\"alignment4\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"top_padding\">3</property>\n                        <property name=\"bottom_padding\">6</property>\n                        <property name=\"left_padding\">12</property>\n                        <child>\n                          <widget class=\"GtkLabel\" id=\"lblSublogic\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"xalign\">0</property>\n                            <property name=\"yalign\">0</property>\n                            <property name=\"label\" translatable=\"yes\">No sublogic</property>\n                            <property name=\"use_markup\">True</property>\n                            <property name=\"selectable\">True</property>\n                            <property name=\"max_width_chars\">30</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkLabel\" id=\"label5\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"label\" translatable=\"yes\">Sublogic of selected theories:</property>\n                        <property name=\"use_markup\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"type\">label_item</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"position\">1</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkFrame\" id=\"frame1\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <widget class=\"GtkAlignment\" id=\"alignment1\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"bottom_padding\">6</property>\n                        <property name=\"left_padding\">6</property>\n                        <child>\n                          <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow1\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"hscrollbar_policy\">automatic</property>\n                            <property name=\"vscrollbar_policy\">automatic</property>\n                            <child>\n                              <widget class=\"GtkTreeView\" id=\"trvFinder\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                              </widget>\n                            </child>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkLabel\" id=\"label1\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"label\" translatable=\"yes\">Pick Model finder:</property>\n                        <property name=\"use_markup\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"type\">label_item</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"position\">2</property>\n                  </packing>\n                </child>\n                <child>\n                  <widget class=\"GtkFrame\" id=\"frame5\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <widget class=\"GtkAlignment\" id=\"alignment3\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"bottom_padding\">6</property>\n                        <property name=\"left_padding\">6</property>\n                        <child>\n                          <widget class=\"GtkComboBox\" id=\"cbComorphism\">\n                            <property name=\"width_request\">220</property>\n                            <property name=\"visible\">True</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                    <child>\n                      <widget class=\"GtkLabel\" id=\"label3\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"label\" translatable=\"yes\">Select comorphism path:</property>\n                        <property name=\"use_markup\">True</property>\n                      </widget>\n                      <packing>\n                        <property name=\"type\">label_item</property>\n                      </packing>\n                    </child>\n                  </widget>\n                  <packing>\n                    <property name=\"expand\">False</property>\n                    <property name=\"position\">3</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">0</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHSeparator\" id=\"hseparator1\">\n            <property name=\"visible\">True</property>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHButtonBox\" id=\"hbuttonbox1\">\n            <property name=\"visible\">True</property>\n            <property name=\"layout_style\">end</property>\n            <child>\n              <widget class=\"GtkButton\" id=\"btnResults\">\n                <property name=\"label\" translatable=\"yes\">View results</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">0</property>\n                <property name=\"secondary\">True</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkButton\" id=\"btnClose\">\n                <property name=\"label\" translatable=\"yes\">Close</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n  <widget class=\"GtkWindow\" id=\"ModelView\">\n    <child>\n      <widget class=\"GtkVBox\" id=\"vbox5\">\n        <property name=\"visible\">True</property>\n        <child>\n          <widget class=\"GtkHBox\" id=\"hbox2\">\n            <property name=\"visible\">True</property>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frResNodes\">\n                <property name=\"visible\">True</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">none</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment9\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"left_padding\">6</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow3\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"hscrollbar_policy\">automatic</property>\n                        <property name=\"vscrollbar_policy\">automatic</property>\n                        <child>\n                          <widget class=\"GtkTreeView\" id=\"trvResNodes\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label7\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label\" translatable=\"yes\">Nodes:</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <widget class=\"GtkFrame\" id=\"frame4\">\n                <property name=\"visible\">True</property>\n                <property name=\"label_xalign\">0</property>\n                <property name=\"shadow_type\">none</property>\n                <child>\n                  <widget class=\"GtkAlignment\" id=\"alignment10\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"left_padding\">6</property>\n                    <child>\n                      <widget class=\"GtkScrolledWindow\" id=\"scrolledwindow4\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">True</property>\n                        <property name=\"hscrollbar_policy\">automatic</property>\n                        <property name=\"vscrollbar_policy\">automatic</property>\n                        <child>\n                          <widget class=\"GtkTextView\" id=\"tvResModel\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                          </widget>\n                        </child>\n                      </widget>\n                    </child>\n                  </widget>\n                </child>\n                <child>\n                  <widget class=\"GtkLabel\" id=\"label8\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"label\" translatable=\"yes\">Result:</property>\n                    <property name=\"use_markup\">True</property>\n                  </widget>\n                  <packing>\n                    <property name=\"type\">label_item</property>\n                  </packing>\n                </child>\n              </widget>\n              <packing>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"position\">0</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHSeparator\" id=\"hseparator2\">\n            <property name=\"visible\">True</property>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <widget class=\"GtkHButtonBox\" id=\"hbuttonbox5\">\n            <property name=\"visible\">True</property>\n            <property name=\"layout_style\">end</property>\n            <child>\n              <widget class=\"GtkButton\" id=\"btnResClose\">\n                <property name=\"label\" translatable=\"yes\">Close</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </widget>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">False</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n          </widget>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </widget>\n    </child>\n  </widget>\n</glade-interface>\n"