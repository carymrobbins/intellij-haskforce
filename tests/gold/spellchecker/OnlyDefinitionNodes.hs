-- Only definition nodes should be considered typos

module OnlyDefinitionNodes where

import Notatypo
import Notatypo.Notatypo

<TYPO>imatypo</TYPO> :: Notatypo <TYPO>imatypotoo</TYPO>
<TYPO>imatypo</TYPO> = notatypo

data <TYPO>Imatypo</TYPO> <TYPO>imatypo</TYPO> = <TYPO>Imatypo</TYPO> Notatypo

newtype <TYPO>Imatypo</TYPO> <TYPO>imatypo</TYPO> = <TYPO>Imatypo</TYPO> notatypo

class <TYPO>Imatypo</TYPO> <TYPO>imatypo</TYPO> where
  <TYPO>imatypo</TYPO> :: Notatypo <TYPO>imatypo</TYPO>

instance Notatypo (Notatypo <TYPO>imatypo</TYPO>) where
  notatypo = Notatypo
