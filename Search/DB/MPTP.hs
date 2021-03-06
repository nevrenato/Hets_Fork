{- |
Module      :  $Header$
Description :  database layout
Copyright   :  (c) Immanuel Normann, Uni Bremen 2007
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  inormann@jacobs-university.de
Stability   :  provisional
Portability :  portable
-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Search.DB.MPTP where

import Database.HaskellDB.DBLayout

import qualified MPTP.Profile
import qualified MPTP.Statistics
import qualified MPTP.Inclusion

mptp :: DBInfo
mptp = DBInfo {dbname = "mptp",
               opts = DBOptions {useBString = False},
               tbls = [TInfo {tname = "profile",
                              cols = [CInfo {cname = "library",
                                             descr = (StringT, False)},
                                      CInfo {cname = "file",
                                             descr = (StringT, False)},
                                      CInfo {cname = "line",
                                             descr = (IntT, False)},
                                      CInfo {cname = "formula",
                                             descr = (StringT, False)},
                                      CInfo {cname = "skeleton",
                                             descr = (StringT, False)},
                                      CInfo {cname = "skeleton_md5",
                                             descr = (StringT, False)},
                                      CInfo {cname = "parameter",
                                             descr = (StringT, False)},
                                      CInfo {cname = "role",
                                             descr = (StringT, False)},
                                      CInfo {cname = "norm_strength",
                                             descr = (StringT, False)},
                                      CInfo {cname = "skeleton_length",
                                             descr = (IntT, False)}]},
                       TInfo {tname = "statistics",
                              cols = [CInfo {cname = "library",
                                             descr = (StringT, False)},
                                      CInfo {cname = "file",
                                             descr = (StringT, False)},
                                      CInfo {cname = "tautologies",
                                             descr = (IntT, False)},
                                      CInfo {cname = "duplicates",
                                             descr = (IntT, False)},
                                      CInfo {cname = "formulae",
                                             descr = (IntT, False)}]},
                       TInfo {tname = "inclusion",
                              cols = [CInfo {cname = "source",
                                             descr = (StringT, False)},
                                      CInfo {cname = "target",
                                             descr = (StringT, False)},
                                      CInfo {cname = "line_assoc",
                                             descr = (StringT, False)},
                                      CInfo {cname = "morphism",
                                             descr = (StringT, False)},
                                      CInfo {cname = "morphism_size",
                                             descr = (IntT, False)}]}]}